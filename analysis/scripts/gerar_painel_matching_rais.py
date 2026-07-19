from __future__ import annotations

import sys
from pathlib import Path

import pandas as pd

SCRIPT_DIR = Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
    sys.path.insert(0, str(SCRIPT_DIR))

from quadro_teoria_mudanca import (  # noqa: E402
    DATA_DIR,
    cnpj_key_to_root_map,
    company_profile,
    parse_base_operacoes,
    region_from_uf,
)


UNIVERSO = DATA_DIR / "RAIS_universo_empresas_2018_2024.csv"
OUT = DATA_DIR / "RAIS_matching_tratadas_controles_2018_2024.xlsx"
CONTROLS_PER_TREATED = 20


def setor_label(value: object) -> str:
    if pd.isna(value):
        return "Subsetor IBGE 00"
    digits = "".join(ch for ch in str(value) if ch.isdigit())
    return "Subsetor IBGE " + digits.zfill(2)[-2:]


def read_universo_chunks():
    usecols = [
        "ano",
        "cnpj_raiz",
        "total_empregados",
        "empregados_lag",
        "variacao_pct_empregados",
        "faixa_emprego_pre",
        "uf_principal",
        "subsetor_ibge_principal_codigo",
    ]
    return pd.read_csv(
        UNIVERSO,
        sep=";",
        usecols=usecols,
        dtype={
            "ano": "int16",
            "cnpj_raiz": "string",
            "faixa_emprego_pre": "string",
            "uf_principal": "string",
            "subsetor_ibge_principal_codigo": "string",
        },
        chunksize=1_000_000,
        low_memory=False,
    )


def clean_chunk(chunk: pd.DataFrame, years: list[int]) -> pd.DataFrame:
    chunk = chunk[chunk["ano"].isin(years)].copy()
    if chunk.empty:
        return chunk
    chunk["cnpj_raiz"] = (
        chunk["cnpj_raiz"].astype(str).str.replace(r"\D", "", regex=True).str.zfill(8)
    )
    chunk["variacao_pct_empregados"] = pd.to_numeric(
        chunk["variacao_pct_empregados"], errors="coerce"
    )
    chunk = chunk[chunk["variacao_pct_empregados"].notna()].copy()
    if chunk.empty:
        return chunk
    chunk["total_empregados"] = pd.to_numeric(chunk["total_empregados"], errors="coerce")
    chunk["empregados_lag"] = pd.to_numeric(chunk["empregados_lag"], errors="coerce")
    chunk["regiao"] = chunk["uf_principal"].map(region_from_uf)
    chunk["porte"] = chunk["faixa_emprego_pre"].fillna("Sem informacao")
    chunk["setor"] = chunk["subsetor_ibge_principal_codigo"].map(setor_label)
    return chunk


def treated_panel(profile: pd.DataFrame, years: list[int]) -> tuple[pd.DataFrame, set[str]]:
    key_root = cnpj_key_to_root_map()
    profile_root = profile.merge(key_root, on="cnpj_key", how="left")
    profile_root["cnpj_raiz"] = profile_root["cnpj_raiz"].fillna(profile_root["cnpj_raiz_base"])
    profile_root = profile_root[profile_root["cnpj_raiz"].notna()].copy()
    profile_root["cnpj_raiz"] = profile_root["cnpj_raiz"].astype(str).str.zfill(8)
    fust_roots = set(profile_root["cnpj_raiz"])

    parts = []
    for chunk in read_universo_chunks():
        chunk = clean_chunk(chunk, [year for year in years if year <= 2024])
        if chunk.empty:
            continue
        chunk = chunk[chunk["cnpj_raiz"].isin(fust_roots)].copy()
        if not chunk.empty:
            parts.append(chunk)

    treated = pd.concat(parts, ignore_index=True) if parts else pd.DataFrame()
    if treated.empty:
        return treated, fust_roots
    treated = treated.merge(
        profile_root[
            [
                "cnpj_raiz",
                "cnpj_key",
                "ano_primeira_operacao",
                "tipo_primeira_operacao",
                "regiao_fust",
                "porte_fust",
                "setor_cnae_fust",
                "subsetor_cnae_fust",
            ]
        ],
        on="cnpj_raiz",
        how="left",
    )
    treated = treated[
        treated["ano_primeira_operacao"].notna()
        & (treated["ano"] == treated["ano_primeira_operacao"].astype("int64"))
    ].copy()
    treated["treated_id"] = [
        f"T{idx:05d}" for idx in range(1, len(treated) + 1)
    ]
    return treated, fust_roots


def make_requests(treated: pd.DataFrame) -> list[dict[str, object]]:
    requests = []
    dimensions = [
        ("Geral", None, "Total"),
        ("Regiao", "regiao", None),
        ("Porte", "porte", None),
        ("Setor", "setor", None),
    ]
    for _, row in treated.iterrows():
        for dimension, col, fixed_category in dimensions:
            category = fixed_category if fixed_category is not None else row[col]
            requests.append(
                {
                    "request_id": f"{row['treated_id']}_{dimension}",
                    "treated_id": row["treated_id"],
                    "ano": int(row["ano"]),
                    "faixa_emprego_pre": row["faixa_emprego_pre"],
                    "empregados_lag_tratada": row["empregados_lag"],
                    "dimensao": dimension,
                    "categoria": category,
                    "category_col": col,
                }
            )
    return requests


def top_controls_for_requests(
    requests: list[dict[str, object]],
    fust_roots: set[str],
    years: list[int],
) -> dict[str, pd.DataFrame]:
    current: dict[str, pd.DataFrame] = {}
    for chunk_idx, chunk in enumerate(read_universo_chunks(), start=1):
        chunk = clean_chunk(chunk, [year for year in years if year <= 2024])
        if chunk.empty:
            continue
        chunk = chunk[~chunk["cnpj_raiz"].isin(fust_roots)].copy()
        if chunk.empty:
            continue
        for req in requests:
            pool = chunk[
                (chunk["ano"] == req["ano"])
                & (chunk["faixa_emprego_pre"] == req["faixa_emprego_pre"])
            ].copy()
            col = req["category_col"]
            if col is not None:
                pool = pool[pool[col] == req["categoria"]].copy()
            if pool.empty:
                continue
            pool["distancia_emprego_pre"] = (
                pool["empregados_lag"] - float(req["empregados_lag_tratada"])
            ).abs()
            pool["request_id"] = req["request_id"]
            pool["treated_id"] = req["treated_id"]
            pool["dimensao"] = req["dimensao"]
            pool["categoria"] = req["categoria"]
            pool = pool.sort_values(
                ["distancia_emprego_pre", "cnpj_raiz"],
                kind="mergesort",
            ).head(CONTROLS_PER_TREATED)
            keep_cols = [
                "request_id",
                "treated_id",
                "dimensao",
                "categoria",
                "ano",
                "cnpj_raiz",
                "total_empregados",
                "empregados_lag",
                "variacao_pct_empregados",
                "faixa_emprego_pre",
                "uf_principal",
                "regiao",
                "porte",
                "setor",
                "distancia_emprego_pre",
            ]
            combined = pd.concat(
                [current.get(req["request_id"], pd.DataFrame()), pool[keep_cols]],
                ignore_index=True,
            )
            combined = (
                combined.sort_values(["distancia_emprego_pre", "cnpj_raiz"], kind="mergesort")
                .drop_duplicates(["request_id", "cnpj_raiz"])
                .head(CONTROLS_PER_TREATED)
            )
            current[req["request_id"]] = combined
        if chunk_idx % 10 == 0:
            print(f"chunks processados: {chunk_idx}; requests com controles: {len(current)}")
    return current


def main() -> None:
    if not UNIVERSO.exists():
        raise FileNotFoundError(f"{UNIVERSO} nao encontrado")
    base = parse_base_operacoes()
    profile = company_profile(base)
    years = sorted(base["ano_aprovacao"].dropna().astype(int).unique())
    treated, fust_roots = treated_panel(profile, years)
    requests = make_requests(treated)
    controls = top_controls_for_requests(requests, fust_roots, years)

    treated_long = []
    treated_by_id = treated.set_index("treated_id")
    for req in requests:
        row = treated_by_id.loc[req["treated_id"]].to_dict()
        treated_long.append(
            {
                "request_id": req["request_id"],
                "treated_id": req["treated_id"],
                "papel": "tratada",
                "dimensao": req["dimensao"],
                "categoria": req["categoria"],
                "ano": req["ano"],
                "cnpj_raiz": row["cnpj_raiz"],
                "cnpj_key": row.get("cnpj_key"),
                "tipo_primeira_operacao": row.get("tipo_primeira_operacao"),
                "ano_primeira_operacao": row.get("ano_primeira_operacao"),
                "total_empregados": row["total_empregados"],
                "empregados_lag": row["empregados_lag"],
                "variacao_pct_empregados": row["variacao_pct_empregados"],
                "faixa_emprego_pre": row["faixa_emprego_pre"],
                "uf_principal": row["uf_principal"],
                "regiao": row["regiao"],
                "porte": row["porte"],
                "setor": row["setor"],
                "distancia_emprego_pre": 0,
            }
        )

    control_long = []
    for df in controls.values():
        if df.empty:
            continue
        part = df.copy()
        part["papel"] = "controle"
        part["cnpj_key"] = pd.NA
        part["tipo_primeira_operacao"] = pd.NA
        part["ano_primeira_operacao"] = pd.NA
        control_long.append(part)

    painel = pd.concat(
        [pd.DataFrame(treated_long), *control_long],
        ignore_index=True,
        sort=False,
    )
    cols = [
        "request_id",
        "treated_id",
        "papel",
        "dimensao",
        "categoria",
        "ano",
        "cnpj_raiz",
        "cnpj_key",
        "tipo_primeira_operacao",
        "ano_primeira_operacao",
        "total_empregados",
        "empregados_lag",
        "variacao_pct_empregados",
        "faixa_emprego_pre",
        "uf_principal",
        "regiao",
        "porte",
        "setor",
        "distancia_emprego_pre",
    ]
    painel = painel[cols].sort_values(
        ["ano", "tipo_primeira_operacao", "treated_id", "dimensao", "papel", "distancia_emprego_pre"],
        kind="mergesort",
    )
    with pd.ExcelWriter(OUT, engine="openpyxl") as writer:
        painel.to_excel(writer, index=False, sheet_name="painel_matching")
    print(OUT)
    print(
        f"tratadas={painel[painel['papel'].eq('tratada')]['cnpj_raiz'].nunique()} "
        f"controles={painel[painel['papel'].eq('controle')]['cnpj_raiz'].nunique()} "
        f"linhas={len(painel)}"
    )


if __name__ == "__main__":
    main()
