from __future__ import annotations

import csv
import re
import unicodedata
from collections import defaultdict
from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
RAIS_DIR = Path("D:/rais")
OUT = ROOT / "data" / "RAIS_universo_empresas_2018_2024.csv"
YEARS = range(2018, 2025)

UF_MAP = {
    "11": "RO",
    "12": "AC",
    "13": "AM",
    "14": "RR",
    "15": "PA",
    "16": "AP",
    "17": "TO",
    "21": "MA",
    "22": "PI",
    "23": "CE",
    "24": "RN",
    "25": "PB",
    "26": "PE",
    "27": "AL",
    "28": "SE",
    "29": "BA",
    "31": "MG",
    "32": "ES",
    "33": "RJ",
    "35": "SP",
    "41": "PR",
    "42": "SC",
    "43": "RS",
    "50": "MS",
    "51": "MT",
    "52": "GO",
    "53": "DF",
}


def normalize(value: str) -> str:
    value = str(value)
    value = "".join(
        char
        for char in unicodedata.normalize("NFKD", value)
        if not unicodedata.combining(char)
    )
    value = value.upper()
    value = re.sub(r"[^A-Z0-9]+", " ", value)
    return re.sub(r"\s+", " ", value).strip()


def digits(value: object) -> str:
    return re.sub(r"\D", "", "" if pd.isna(value) else str(value))


def parse_number(value: object) -> float:
    if value is None or pd.isna(value):
        return 0.0
    text = str(value).strip()
    if not text:
        return 0.0
    if "," in text:
        text = text.replace(".", "").replace(",", ".")
    try:
        return float(text)
    except ValueError:
        return 0.0


def employment_band(value: float) -> str:
    if pd.isna(value):
        return "Sem informacao"
    value = float(value)
    if value <= 0:
        return "0"
    if value <= 9:
        return "1 a 9"
    if value <= 49:
        return "10 a 49"
    if value <= 99:
        return "50 a 99"
    if value <= 249:
        return "100 a 249"
    if value <= 499:
        return "250 a 499"
    return "500 ou mais"


def uf_from_municipio(value: object) -> str:
    code = digits(value).zfill(6)[:2]
    return UF_MAP.get(code, "")


def find_estab_file(year: int) -> Path:
    folder = RAIS_DIR / f"RAIS_{year}"
    files = [
        path
        for path in folder.iterdir()
        if path.is_file()
        and re.search(r"estb|estab", path.name, flags=re.IGNORECASE)
        and re.search(r"\.(txt|comt)$", path.name, flags=re.IGNORECASE)
    ]
    if not files:
        raise FileNotFoundError(f"Arquivo de estabelecimentos RAIS {year} nao encontrado")
    files.sort(
        key=lambda path: (
            not path.name.upper().endswith(".COMT"),
            not path.name.upper().startswith("RAIS_ESTAB"),
            path.name,
        )
    )
    return files[0]


def pick(columns: list[str], patterns: list[str]) -> str:
    normalized = {column: normalize(column) for column in columns}
    for pattern in patterns:
        for column, norm in normalized.items():
            if re.search(pattern, norm):
                return column
    raise KeyError(f"Coluna nao encontrada: {patterns}. Disponiveis: {columns}")


def process_year(year: int) -> pd.DataFrame:
    path = find_estab_file(year)
    with path.open("r", encoding="latin-1", errors="replace", newline="") as handle:
        sample = handle.readline()
    delimiter = "," if sample.count(",") > sample.count(";") else ";"
    header = next(
        csv.reader([sample], delimiter=delimiter, quotechar='"')
    )

    cols = {
        "cnpj_raiz": pick(header, [r"^CNPJ RAIZ"]),
        "municipio": pick(header, [r"^MUNICIPIO"]),
        "empregados": pick(header, [r"^QTD VINCULOS ATIVOS"]),
        "cnae_classe": pick(header, [r"^CNAE 2 0 CLASSE", r"^CNAE 95 CLASSE"]),
        "cnae_subclasse": pick(header, [r"^CNAE 2 0 SUBCLASSE"]),
        "subsetor_ibge": pick(header, [r"^IBGE SUBSETOR"]),
    }

    chunks = pd.read_csv(
        path,
        sep=delimiter,
        quotechar='"',
        encoding="latin-1",
        usecols=list(cols.values()),
        dtype=str,
        chunksize=350_000,
        low_memory=False,
    )

    total_emp: defaultdict[str, float] = defaultdict(float)
    total_firmas: defaultdict[str, int] = defaultdict(int)
    uf_emp: defaultdict[tuple[str, str], float] = defaultdict(float)
    setor_emp: defaultdict[tuple[str, str, str, str], float] = defaultdict(float)

    for idx, chunk in enumerate(chunks, start=1):
        chunk = chunk.rename(columns={v: k for k, v in cols.items()})
        chunk["cnpj_raiz"] = chunk["cnpj_raiz"].map(lambda x: digits(x).zfill(8))
        chunk = chunk[chunk["cnpj_raiz"].str.len().eq(8) & chunk["cnpj_raiz"].ne("00000000")]
        if chunk.empty:
            continue
        chunk["empregados"] = chunk["empregados"].map(parse_number)
        chunk["uf"] = chunk["municipio"].map(uf_from_municipio)
        chunk["cnae_classe"] = chunk["cnae_classe"].map(lambda x: digits(x))
        chunk["cnae_subclasse"] = chunk["cnae_subclasse"].map(lambda x: digits(x))
        chunk["subsetor_ibge"] = chunk["subsetor_ibge"].map(lambda x: digits(x).zfill(2)[-2:])

        grp_total = chunk.groupby("cnpj_raiz", dropna=False).agg(
            total_empregados=("empregados", "sum"),
            total_firmas=("cnpj_raiz", "size"),
        )
        for cnpj, row in grp_total.iterrows():
            total_emp[cnpj] += float(row["total_empregados"])
            total_firmas[cnpj] += int(row["total_firmas"])

        grp_uf = chunk.groupby(["cnpj_raiz", "uf"], dropna=False)["empregados"].sum()
        for (cnpj, uf), value in grp_uf.items():
            if uf:
                uf_emp[(cnpj, uf)] += float(value)

        grp_setor = chunk.groupby(
            ["cnpj_raiz", "cnae_classe", "cnae_subclasse", "subsetor_ibge"],
            dropna=False,
        )["empregados"].sum()
        for key, value in grp_setor.items():
            setor_emp[key] += float(value)

        if idx % 10 == 0:
            print(f"{year}: chunks {idx}, empresas acumuladas {len(total_emp):,}")

    best_uf: dict[str, tuple[str, float]] = {}
    for (root, uf), value in uf_emp.items():
        current = best_uf.get(root)
        if current is None or value > current[1]:
            best_uf[root] = (uf, value)

    best_setor: dict[str, tuple[str, str, str, float]] = {}
    for (root, classe, subclasse, subsetor), value in setor_emp.items():
        current = best_setor.get(root)
        if current is None or value > current[3]:
            best_setor[root] = (classe, subclasse, subsetor, value)

    rows = []
    for cnpj, empregados in total_emp.items():
        uf_principal = best_uf.get(cnpj, ("", 0))[0]
        classe, subclasse, subsetor, empregados_setor = best_setor.get(cnpj, ("", "", "", 0))
        rows.append(
            {
                "ano": year,
                "cnpj_raiz": cnpj,
                "total_empregados": empregados,
                "total_firmas": total_firmas[cnpj],
                "uf_principal": uf_principal,
                "setor_cnae_classe_principal": classe,
                "setor_cnae_subclasse_principal": subclasse,
                "subsetor_ibge_principal_codigo": subsetor,
                "empregados_setor_principal": empregados_setor,
            }
        )

    out = pd.DataFrame(rows)
    print(f"{year}: empresas {len(out):,}, empregados {out['total_empregados'].sum():,.0f}")
    return out


def main() -> None:
    OUT.parent.mkdir(parents=True, exist_ok=True)
    parts = []
    for year in YEARS:
        parts.append(process_year(year))
    panel = pd.concat(parts, ignore_index=True)
    panel = panel.sort_values(["cnpj_raiz", "ano"])
    panel["empregados_lag"] = panel.groupby("cnpj_raiz")["total_empregados"].shift(1)
    panel["ano_lag"] = panel.groupby("cnpj_raiz")["ano"].shift(1)
    panel["variacao_pct_empregados"] = (
        panel["total_empregados"] - panel["empregados_lag"]
    ) / panel["empregados_lag"]
    panel.loc[
        (panel["empregados_lag"].isna())
        | (panel["empregados_lag"] <= 0)
        | (panel["ano_lag"] != panel["ano"] - 1),
        "variacao_pct_empregados",
    ] = pd.NA
    panel["faixa_emprego_pre"] = panel["empregados_lag"].map(employment_band)
    panel.to_csv(OUT, index=False, sep=";")
    print(OUT)
    print(f"linhas={len(panel):,}; empresas={panel['cnpj_raiz'].nunique():,}")


if __name__ == "__main__":
    main()
