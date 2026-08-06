# Resultados — frequências mensal e trimestral

## Síntese

A extensão pós-Covid usa janeiro de 2022 a dezembro de 2024 e os desfechos
`log(1 + emprego)` e `log(1 + massa salarial)`. O baseline do
Callaway–Sant'Anna é universal: o período relativo −1 é zero em todas as
especificações. Há 350 unidades pareadas, mas somente 12 tratadas dentro da
janela (1 em 2023 e 11 em 2024); as outras 338 são controles ou tratadas após o
fim do painel.

As estimativas não dão suporte a uma conclusão causal robusta:

- Para emprego, Callaway–Sant'Anna mensal é positivo, mas o intervalo de 95%
  inclui zero por pequena margem; o trimestral é negativo e impreciso.
- Para emprego, SDID mensal é próximo de zero. O trimestral é negativo, mas
  sustentado por apenas duas coortes estimáveis.
- Para massa salarial, os quatro intervalos de 95% incluem zero. As estimativas
  trimestrais são especialmente imprecisas.

Consulte `outputs/resumo_resultados.csv` para os valores exatos da última
execução.

## Diagnósticos e limites

1. As coortes mensais são pequenas. Das sete coortes observadas, apenas duas têm
   pelo menos dois tratados e podem ser usadas no SDID mensal.
2. Há sinais pontuais de pré-tendência no estudo de evento mensal. A matriz de
   covariância do Callaway–Sant'Anna é singular, impedindo o teste conjunto de
   pré-tendências.
3. Emprego mensal foi reconstruído a partir das datas de admissão e
   desligamento. Massa salarial foi reconstruída somando as remunerações
   nominais mensais dos vínculos por CNPJ raiz.
4. O painel termina em dezembro de 2024. Isso produz pouco pós-tratamento para
   coortes do fim de 2024 e exclui as 96 unidades da coorte anual de 2025.
5. A inferência deve ser considerada exploratória. O resultado trimestral do
   SDID, isoladamente significativo, é especialmente sensível à agregação e à
   composição das poucas coortes.

## Leitura recomendada

O principal resultado é ausência de evidência convergente entre métodos e
frequências. A frequência maior acrescenta observações no tempo, mas não cria
novas unidades tratadas; por isso, não resolve a limitação de massa efetiva nas
coortes. Uma atualização da RAIS mensal até 2025/2026 seria mais valiosa do que
simplesmente aumentar o número de reamostragens.
