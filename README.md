
DDME - Repositório de funções do R úteis para o dia a dia
=========================================================

1 inspeciona\_outlier()
-----------------------

Função base criada por Gabriel Motta, útil para detecção de outliers em séries temporais com base no pacote `anomalize`. As opções de uso são:

-   Atômicos:
    -   vetor com inteiros ou numéricos
    -   objeto ts
-   Tabelas:
    -   data.table
    -   tbl\_df
    -   data.frame

### Exemplo série unidimensional

Vetor sem data guardado em `x`:

![](README_figs/README-unnamed-chunk-1-1.png)

O resultado da chamada é um objeto `data.table`. Note que a coluna `DATA` é criada arbitrariamente e nem faz sentido, já que o objeto inicial não possui referências temporais.

``` r
x_alterado <- inspeciona_outlier(x)
x_alterado[49:55]
```

    ##        DATA SERIE SERIE_IMPUTADA FLAG_OUTLIER
    ## 1: 49-01-01     9              9            0
    ## 2: 50-01-01   -10              9            1
    ## 3: 51-01-01    10             10            0
    ## 4: 52-01-01    18              7            1
    ## 5: 53-01-01     5              5            0
    ## 6: 54-01-01     5              5            0
    ## 7: 55-01-01     7              7            0

``` r
x_alterado %>% 
  melt(id = "DATA", measure = patterns("SERIE")) %>% 
  dplyr::mutate(variable = factor(variable, labels = c("Original","Imputada"))) %>% {.[]} %>% 
  ggplot(aes(DATA,value,color = variable)) +
  geom_line() +
  labs(x = "Índice", y = "Valores", color = "") + 
  theme(legend.position = "bottom")
```

![](README_figs/README-unnamed-chunk-3-1.png)

### Múltiplas séries

O sentido das séries na tabela que estiver trabalhando pode ser por linhas ou colunas, e essencialmente este é o único cuidado que se deve ter para a chamada da função. O argumento `sentido` deve ser definido como **1 para linhas** ou **2 para colunas**.

#### Séries nas linhas

Exemplo para tabelas com séries nas linhas:

``` r
d1[,1:10]
```

    ##    ID       V1      V2       V3       V4       V5        V6       V7
    ## 1:  1 8.000000 15.0000 5.000000 9.000000 6.000000 10.000000 8.000000
    ## 2:  2 8.948249 13.7749 4.208721 8.765303 6.667333  9.317604 9.659117
    ##        V8       V9
    ## 1: 13.000 11.00000
    ## 2: 13.837 11.30835

``` r
# Formato de saída "wide"
inspeciona_outlier(d1, sentido = 1) %>% 
  dplyr::sample_n(10) %>% 
  {.[order(ID,PERIODO)]}
```

    ##     ID PERIODO      SERIE SERIE_IMPUTADA FLAG_OUTLIER
    ##  1:  1     V37   8.000000       8.000000            0
    ##  2:  1     V48   7.000000       7.000000            0
    ##  3:  1     V57   9.000000       9.000000            0
    ##  4:  1     V69  14.000000      14.000000            0
    ##  5:  1     V84   7.000000       7.000000            0
    ##  6:  2     V36  11.247827      11.247827            0
    ##  7:  2     V50 -11.800579      10.017053            1
    ##  8:  2     V66   8.554364       8.554364            0
    ##  9:  2     V73   9.151411       9.151411            0
    ## 10:  2     V83   9.485607       9.485607            0

``` r
# Formato de saída "long"
inspeciona_outlier(d1, sentido = 1, out_format = "long") %>% 
  dplyr::sample_n(10) %>% 
  {.[order(ID,PERIODO)]}
```

    ##     ID PERIODO FLAG_OUTLIER IMPUTADA     VALOR
    ##  1:  1     V47            0        0 17.000000
    ##  2:  1      V7            0        0  8.000000
    ##  3:  1      V8            0        1 13.000000
    ##  4:  1     V82            0        0 18.000000
    ##  5:  2     V15            0        0  9.476406
    ##  6:  2     V23            0        0 15.512933
    ##  7:  2     V37            0        1  7.946557
    ##  8:  2     V60            1        1 10.423963
    ##  9:  2      V8            0        1 13.837000
    ## 10:  2     V82            0        1 17.647898

#### Séries nas colunas

Exemplo para tableas com séries nas colunas:

``` r
d2[1:5]
```

    ##          DATA SERIE1  SERIE2
    ## 1: 2018-01-01      8 -8.0355
    ## 2: 2018-01-02     15 -0.9985
    ## 3: 2018-01-03      5 -0.3217
    ## 4: 2018-01-04      9  0.4614
    ## 5: 2018-01-05      6 -0.4368

``` r
# Formato de saída "wide"
inspeciona_outlier(d2, sentido = 2) %>% head()
```

    ##          DATA SERIE1 SERIE1_IMPUTADA SERIE1_FLAG_OUTLIER  SERIE2
    ## 1: 2018-01-01      8               8                   0 -8.0355
    ## 2: 2018-01-02     15              15                   0 -0.9985
    ## 3: 2018-01-03      5               5                   0 -0.3217
    ## 4: 2018-01-04      9               9                   0  0.4614
    ## 5: 2018-01-05      6               6                   0 -0.4368
    ## 6: 2018-01-06     10              10                   0 -0.6617
    ##    SERIE2_IMPUTADA SERIE2_FLAG_OUTLIER
    ## 1:         -0.6601                   1
    ## 2:         -0.9985                   0
    ## 3:         -0.3217                   0
    ## 4:          0.4614                   0
    ## 5:         -0.4368                   0
    ## 6:         -0.6617                   0

``` r
# Formato de saída "long"
inspeciona_outlier(d2, sentido = 2, out_format = "long") %>% 
  dplyr::sample_n(10) %>% 
  {.[order(SERIE,DATA)]}
```

    ##           DATA  SERIE FLAG_OUTLIER IMPUTADA   VALOR
    ##  1: 2018-01-01 SERIE1            0        1  8.0000
    ##  2: 2018-01-12 SERIE1            0        1  8.0000
    ##  3: 2018-01-20 SERIE1            0        1 12.0000
    ##  4: 2018-01-31 SERIE1            0        1  8.0000
    ##  5: 2018-02-09 SERIE1            0        0 15.0000
    ##  6: 2018-02-10 SERIE1            0        1 15.0000
    ##  7: 2018-02-26 SERIE1            0        0  9.0000
    ##  8: 2018-03-31 SERIE1            0        0  7.0000
    ##  9: 2018-01-17 SERIE2            0        1  0.3891
    ## 10: 2018-02-05 SERIE2            0        0 -0.6354

2 na\_prop()
------------

Função para checar proporção de valores faltantes num `data.table` com variáveis nas colunas.

-   Outras possibilidades:
    -   Apresentar colunas com mais do que `corte` na proporção de NAs
    -   Retirar colunas com proporção de NAs maior do que `corte` se `drop = TRUE`

### Exemplo para banco de dados datasets::attenu

``` r
dados <- datasets::attenu %>% as.data.table()
head(dados)
```

    ##    event mag station dist accel
    ## 1:     1 7.0     117   12 0.359
    ## 2:     2 7.4    1083  148 0.014
    ## 3:     2 7.4    1095   42 0.196
    ## 4:     2 7.4     283   85 0.135
    ## 5:     2 7.4     135  107 0.062
    ## 6:     2 7.4     475  109 0.054

``` r
# Geral
dados %>% na_prop()
```

    ##    VARIAVEL N_MISSING N_TOTAL PROPORCAO_NA
    ## 1:  station        16     182   0.08791209
    ## 2:    event         0     182   0.00000000
    ## 3:      mag         0     182   0.00000000
    ## 4:     dist         0     182   0.00000000
    ## 5:    accel         0     182   0.00000000

``` r
# Restringindo proporcao
dados %>% na_prop(corte = 0.2)
```

    ## Empty data.table (0 rows) of 4 cols: VARIAVEL,N_MISSING,N_TOTAL,PROPORCAO_NA

Mudando base por referência:

``` r
dados %>% na_prop(corte = 0.05, drop = TRUE)
```

    ## Removendo as seguintes colunas:
    ## station

``` r
dados
```

    ##      event mag  dist accel
    ##   1:     1 7.0  12.0 0.359
    ##   2:     2 7.4 148.0 0.014
    ##   3:     2 7.4  42.0 0.196
    ##   4:     2 7.4  85.0 0.135
    ##   5:     2 7.4 107.0 0.062
    ##  ---                      
    ## 178:    23 5.3  46.1 0.070
    ## 179:    23 5.3  47.1 0.080
    ## 180:    23 5.3  47.7 0.033
    ## 181:    23 5.3  49.2 0.017
    ## 182:    23 5.3  53.1 0.022
