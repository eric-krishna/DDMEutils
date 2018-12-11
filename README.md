
DDMEutils - Repository of useful functions for daily analysis and R programming
===============================================================================

1 `insp_outlier()`
------------------

Created by Gabriel Motta for outlier detection in time series, based on `anomalize` package. S3 methods are defined for `ts` and `data.frame` objects.

### Unidimensional time series

Undated object `x`:

![](README_figs/README-unnamed-chunk-1-1.png)

The function call results in a `data.table` object. If `x` has no explicit dates, a default date sequence is created.

``` r
x_modif <- insp_outlier(as.ts(x))
x_modif[49:55]
```

    ##          DATE SERIES IMPUTED_SERIES OUTLIER_FLAG
    ## 1: 2018-10-21      8              8            0
    ## 2: 2018-10-22    -10              8            1
    ## 3: 2018-10-23      8              8            0
    ## 4: 2018-10-24      9              9            0
    ## 5: 2018-10-25     12             12            0
    ## 6: 2018-10-26     11             11            0
    ## 7: 2018-10-27      5              5            0

``` r
x_modif %>% 
  melt(id = "DATE", measure = patterns("SERIES")) %>% 
  dplyr::mutate(variable = factor(variable, labels = c("Original","Imputed"))) %>% {.[]} %>% 
  ggplot(aes(DATE,value,color = variable)) +
  geom_line() +
  labs(x = "Index", y = "Values", color = "") + 
  theme(legend.position = "bottom")
```

![](README_figs/README-unnamed-chunk-3-1.png)

### Multiple time series

When working with multiple time series, one has to pay attention to two additional arguments: `margin` and `idcol, dtcol`.

-   If the series are arranged by rows, then set `margin = 1` and `idcol = <integer specifying index column>`. If there is no index column, then set `idcol = 0` or `idcol = NULL` (default);
-   If framed by columns, then set `margin = 2` and `dtcol = <integer specifying a date column>`. Likewise, `dtcol = 0` or `dtcol = NULL` if there is not an explicit date column.

#### Time series framed in rows

Example:

``` r
d1[,1:10]
```

    ##    ID       V1       V2       V3       V4       V5       V6       V7
    ## 1:  1 13.00000 15.00000 9.000000 16.00000 12.00000 14.00000 7.000000
    ## 2:  2 13.00321 15.85243 9.820114 16.33274 11.79309 16.36154 6.823636
    ##          V8       V9
    ## 1: 5.000000 13.00000
    ## 2: 5.415589 15.25655

``` r
# 'Wide' output format
insp_outlier(d1, margin = 1, idcol = 1) %>% 
  dplyr::sample_n(10) %>% 
  {.[order(ID, PERIOD)]}
```

    ##     ID PERIOD    SERIES IMPUTED_SERIES OUTLIER_FLAG
    ##  1:  1    V36 10.000000      10.000000            0
    ##  2:  1    V41 14.000000      14.000000            0
    ##  3:  1    V44  7.000000       7.000000            0
    ##  4:  1    V80 11.000000      11.000000            0
    ##  5:  1    V98 16.000000      16.000000            0
    ##  6:  2    V17  5.373607       5.373607            0
    ##  7:  2    V41 12.154600      12.154600            0
    ##  8:  2    V57  9.334206       9.334206            0
    ##  9:  2    V58 12.524598      12.524598            0
    ## 10:  2    V74  8.554688       8.554688            0

``` r
# 'Long' output format
insp_outlier(d1, margin = 1, out_format = "long") %>% 
  dplyr::sample_n(10) %>% 
  {.[order(ID, PERIOD)]}
```

    ##     ID PERIOD OUTLIER_FLAG IMPUTED     VALUE
    ##  1:  1    V57            0       1  9.000000
    ##  2:  1    V64            0       1  9.000000
    ##  3:  1    V76            0       0 16.000000
    ##  4:  2    V13            0       1 17.805308
    ##  5:  2    V18            0       0  6.813689
    ##  6:  2    V25            0       0  9.436514
    ##  7:  2    V72            0       1 18.137185
    ##  8:  2    V75            0       1 12.419367
    ##  9:  2    V85            0       1  9.821971
    ## 10:  2    V99            0       1 11.604808

#### Time series framed in cols

Example:

``` r
d2[1:5]
```

    ##          DATE SERIES1 SERIES2
    ## 1: 2018-01-01      13 -0.4406
    ## 2: 2018-01-02      15 -0.5089
    ## 3: 2018-01-03       9 -0.9169
    ## 4: 2018-01-04      16 -0.4337
    ## 5: 2018-01-05      12  1.6234

``` r
# 'Wide' output format
insp_outlier(d2, margin = 2, dtcol = 1) %>% head()
```

    ##          DATE SERIES1 IMPUTED_SERIES1 OUTLIER_FLAG_SERIES1 SERIES2
    ## 1: 2018-01-01      13              13                    0 -0.4406
    ## 2: 2018-01-02      15              15                    0 -0.5089
    ## 3: 2018-01-03       9               9                    0 -0.9169
    ## 4: 2018-01-04      16              16                    0 -0.4337
    ## 5: 2018-01-05      12              12                    0  1.6234
    ## 6: 2018-01-06      14              14                    0 -0.4083
    ##    IMPUTED_SERIES2 OUTLIER_FLAG_SERIES2
    ## 1:         -0.4406                    0
    ## 2:         -0.5089                    0
    ## 3:         -0.9169                    0
    ## 4:         -0.4337                    0
    ## 5:          1.6234                    0
    ## 6:         -0.4083                    0

``` r
# 'Long' output format
insp_outlier(d2, margin = 2, dtcol = 1, out_format = "long") %>% 
  dplyr::sample_n(10) %>% 
  {.[order(SERIES, DATE)]}
```

    ##           DATE  SERIES OUTLIER_FLAG IMPUTED   VALUE
    ##  1: 2018-03-12 SERIES1            0       0  9.0000
    ##  2: 2018-03-16 SERIES1            0       1 12.0000
    ##  3: 2018-01-03 SERIES2            0       1 -0.9169
    ##  4: 2018-01-05 SERIES2            0       0  1.6234
    ##  5: 2018-02-02 SERIES2            0       1  2.0181
    ##  6: 2018-02-03 SERIES2            0       0 -0.4724
    ##  7: 2018-02-07 SERIES2            0       1 -0.3899
    ##  8: 2018-03-06 SERIES2            0       0  0.1446
    ##  9: 2018-03-23 SERIES2            0       1 -0.6911
    ## 10: 2018-04-10 SERIES2            0       0  0.2083

2 `insp_seasonality()`
----------------------

3 Value replacements with `subs_any()` and `subs_na()`
------------------------------------------------------

### `subs_na()`

Replaces NA occurrences in variables of a table. The syntax is either on the form `subs_na(data, col_1 = "?", col_2 = 999)` or `subs_na(data, list(col_1 = "?", col_2 = 999))`, where '?' and 999 are examples of replacement values.

### Example with `airquality` dataset

``` r
data <- airquality %>% as.data.table()
head(data)
```

    ##    Ozone Solar.R Wind Temp Month Day
    ## 1:    41     190  7.4   67     5   1
    ## 2:    36     118  8.0   72     5   2
    ## 3:    12     149 12.6   74     5   3
    ## 4:    18     313 11.5   62     5   4
    ## 5:    NA      NA 14.3   56     5   5
    ## 6:    28      NA 14.9   66     5   6

``` r
data %>% 
  subs_na(Ozone = 999, Solar.R = 999)
```

    ##      Ozone Solar.R Wind Temp Month Day
    ##   1:    41     190  7.4   67     5   1
    ##   2:    36     118  8.0   72     5   2
    ##   3:    12     149 12.6   74     5   3
    ##   4:    18     313 11.5   62     5   4
    ##   5:   999     999 14.3   56     5   5
    ##  ---                                  
    ## 149:    30     193  6.9   70     9  26
    ## 150:   999     145 13.2   77     9  27
    ## 151:    14     191 14.3   75     9  28
    ## 152:    18     131  8.0   76     9  29
    ## 153:    20     223 11.5   68     9  30

Regular expressions for equal value replacement and multiple column matching are supported:

``` r
# Replaces NA by 999 at columns that contain '.' 
data %>% subs_na("\\." = 999)
```

    ##      Ozone Solar.R Wind Temp Month Day
    ##   1:    41     190  7.4   67     5   1
    ##   2:    36     118  8.0   72     5   2
    ##   3:    12     149 12.6   74     5   3
    ##   4:    18     313 11.5   62     5   4
    ##   5:    NA     999 14.3   56     5   5
    ##  ---                                  
    ## 149:    30     193  6.9   70     9  26
    ## 150:    NA     145 13.2   77     9  27
    ## 151:    14     191 14.3   75     9  28
    ## 152:    18     131  8.0   76     9  29
    ## 153:    20     223 11.5   68     9  30

### `subs_any()`

Replaces any `x` value for any `y` within `data.frame` variables. The syntax is `subs_any(data, col_1 = list(<value>, <input>), col_2 = list(<value>, <input>))`.

Just like `subs_na()`, refer to multiple columns with regular expressions is supported.

``` r
data %>% 
  subs_any("^M|^D" = list(5, 10000))
```

    ##      Ozone Solar.R Wind Temp Month   Day
    ##   1:    41     190  7.4   67 10000     1
    ##   2:    36     118  8.0   72 10000     2
    ##   3:    12     149 12.6   74 10000     3
    ##   4:    18     313 11.5   62 10000     4
    ##   5:    NA      NA 14.3   56 10000 10000
    ##  ---                                    
    ## 149:    30     193  6.9   70     9    26
    ## 150:    NA     145 13.2   77     9    27
    ## 151:    14     191 14.3   75     9    28
    ## 152:    18     131  8.0   76     9    29
    ## 153:    20     223 11.5   68     9    30

4 Cleaning text variables and column names with `clean()`
---------------------------------------------------------

5 Helpers for dealing with NA: `na_prop()` and `na_input()`
-----------------------------------------------------------

### `na_prop()`

Check total and proportion of missing values by `data.frame` variables.

``` r
# Default print
data %>% na_prop()
```

    ##    VARIABLE N_MISSING N_TOTAL    NA_FREQ
    ## 1:    Ozone        37     153 0.24183007
    ## 2:  Solar.R         7     153 0.04575163
    ## 3:     Wind         0     153 0.00000000
    ## 4:     Temp         0     153 0.00000000
    ## 5:    Month         0     153 0.00000000
    ## 6:      Day         0     153 0.00000000

``` r
# Restricted. Useful to retrieve variables that present more than min_prop of NA.   
data %>% na_prop(min_prop = 0.1)
```

    ##    VARIABLE N_MISSING N_TOTAL   NA_FREQ
    ## 1:    Ozone        37     153 0.2418301

### `na_input()`
