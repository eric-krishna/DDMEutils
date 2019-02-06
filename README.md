
DDMEutils - Repository of useful functions for daily analysis and R programming
===============================================================================

1 `insp_outlier()`
------------------

Written by Gabriel Motta for outlier detection in time series, based on `anomalize` package. S3 methods are defined for `ts` and `data.frame` objects.

### Unidimensional time series

Undated object `x`:

![](README_figs/README-unnamed-chunk-1-1.png)

The function call results in a `data.table` object. If `x` has no explicit dates, a default date sequence is created.

``` r
x_modif <- insp_outlier(as.ts(x))
x_modif[49:55]
```

    ##          DATE SERIES IMPUTED_SERIES OUTLIER_FLAG
    ## 1: 2018-12-16     12             12            0
    ## 2: 2018-12-17    -10             11            1
    ## 3: 2018-12-18      7              7            0
    ## 4: 2018-12-19      8              8            0
    ## 5: 2018-12-20     12             12            0
    ## 6: 2018-12-21      6              6            0
    ## 7: 2018-12-22     17             17            0

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

    ##    ID       V1       V2       V3       V4       V5        V6       V7
    ## 1:  1 7.000000 8.000000 7.000000 4.000000 8.000000 10.000000 7.000000
    ## 2:  2 6.979288 9.223284 6.678826 4.568702 7.292705  9.117328 7.426393
    ##          V8       V9
    ## 1: 7.000000 10.00000
    ## 2: 7.532658 11.55808

``` r
# 'Wide' output format
insp_outlier(d1, margin = 1, idcol = 1) %>% 
  dplyr::sample_n(10) %>% 
  {.[order(ID, PERIOD)]}
```

    ##     ID PERIOD   SERIES IMPUTED_SERIES OUTLIER_FLAG
    ##  1:  1    V38 12.00000       12.00000            0
    ##  2:  1    V56 10.00000       10.00000            0
    ##  3:  1    V68 10.00000       10.00000            0
    ##  4:  1    V74 10.00000       10.00000            0
    ##  5:  1    V96 11.00000       11.00000            0
    ##  6:  2    V28 12.78426       12.78426            0
    ##  7:  2    V53 12.30618       12.30618            0
    ##  8:  2    V67 12.88026       12.88026            0
    ##  9:  2    V73 11.18531       11.18531            0
    ## 10:  2    V98 10.95936       10.95936            0

``` r
# 'Long' output format
insp_outlier(d1, margin = 1, out_format = "long") %>% 
  dplyr::sample_n(10) %>% 
  {.[order(ID, PERIOD)]}
```

    ##     ID PERIOD OUTLIER_FLAG IMPUTED     VALUE
    ##  1:  1    V13            0       1  7.000000
    ##  2:  1    V37            0       1 10.000000
    ##  3:  1    V38            0       1 12.000000
    ##  4:  1    V66            0       0  8.000000
    ##  5:  1    V80            0       0 12.000000
    ##  6:  2    V40            0       1  9.763261
    ##  7:  2    V44            0       0 11.430838
    ##  8:  2    V45            0       0  7.122475
    ##  9:  2     V7            0       0  7.426393
    ## 10:  2    V85            0       1 10.638640

#### Time series framed in cols

Example:

``` r
d2[1:5]
```

    ##          DATE SERIES1 SERIES2
    ## 1: 2018-01-01       7  2.8832
    ## 2: 2018-01-02       8  1.3912
    ## 3: 2018-01-03       7  0.1946
    ## 4: 2018-01-04       4 -0.9933
    ## 5: 2018-01-05       8  0.4761

``` r
# 'Wide' output format
insp_outlier(d2, margin = 2, dtcol = 1) %>% head()
```

    ##          DATE SERIES1 IMPUTED_SERIES1 OUTLIER_FLAG_SERIES1 SERIES2
    ## 1: 2018-01-01       7               7                    0  2.8832
    ## 2: 2018-01-02       8               8                    0  1.3912
    ## 3: 2018-01-03       7               7                    0  0.1946
    ## 4: 2018-01-04       4               4                    0 -0.9933
    ## 5: 2018-01-05       8               8                    0  0.4761
    ## 6: 2018-01-06      10              10                    0  0.2319
    ##    IMPUTED_SERIES2 OUTLIER_FLAG_SERIES2
    ## 1:          2.8832                    0
    ## 2:          1.3912                    0
    ## 3:          0.1946                    0
    ## 4:         -0.9933                    0
    ## 5:          0.4761                    0
    ## 6:          0.2319                    0

``` r
# 'Long' output format
insp_outlier(d2, margin = 2, dtcol = 1, out_format = "long") %>% 
  dplyr::sample_n(10) %>% 
  {.[order(SERIES, DATE)]}
```

    ##           DATE  SERIES OUTLIER_FLAG IMPUTED   VALUE
    ##  1: 2018-01-13 SERIES1            0       0  7.0000
    ##  2: 2018-01-16 SERIES1            0       1  8.0000
    ##  3: 2018-03-23 SERIES1            0       1  8.0000
    ##  4: 2018-03-31 SERIES1            0       0 10.0000
    ##  5: 2018-04-02 SERIES1            0       0  3.0000
    ##  6: 2018-01-10 SERIES2            0       1  0.2598
    ##  7: 2018-01-12 SERIES2            0       1  0.7717
    ##  8: 2018-02-01 SERIES2            0       1 -3.5262
    ##  9: 2018-02-12 SERIES2            0       0  1.3193
    ## 10: 2018-02-15 SERIES2            0       1  0.6839

2 `insp_seasonality()`
----------------------

Written by Gabriel Motta for getting seasonality of time series, based on `forecast` package. S3 methods are defined for `ts` and `data.frame` objects.

Options for `data.frame`: (check `args(insp_outlier.data.frame)` for defaults)

-   `trend` T/F - if time series have trend;
-   `margin`
    -   `margin = 1` for series framed row-wise. In this case, specify as well an integer to an index column on `idcol`;
    -   `margin = 2` for series framed col-wise. Specify an integer to an date column on `dtcol`;
-   `periodicity` of the series

### Example for `airquality` dataset:

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
setorder(data, Month, Day)

data[, .(Wind, Temp)] %>% 
  insp_seasonality(margin = 2, dtcol = 0, periodicity = 'day')
```

    ##      ID SEASONALITY
    ## 1: Wind         365
    ## 2: Temp         365

Considering we have daily observations and a total length no longer than a year for 'Wind' and 'Temp' variables, seasonality 365 means that no periods of seasonality were found.

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

Methods for cleaning headers and text variables within data.frame. For `factor` objects, cleans levels attribute. Default replacements are stored in exported list `.dict`, where names `rm` and `undln` stand for 'remove' and 'underline' actions.

``` r
.dict
```

    ## $rm
    ##  [1] "`" "^" "~" "×" "÷" "¡" "¢" "£" "¤" "¥" "¦" "§" "¨" "©" "ª" "«" "¬"
    ## [18] "­" "®" "¯" "±" "²" "³" "´" "µ" "¶" "·" "¸" "¹" "º" "»" "¼" "½" "¾"
    ## [35] "¿"
    ## 
    ## $undln
    ## [1] "/"   "\\." "-"   "\\s" "\\(" "\\)"
    ## 
    ## $a
    ## [1] "à" "á" "â" "ã" "ä" "å"
    ## 
    ## $A
    ## [1] "À" "Á" "Â" "Ã" "Ä" "Å"
    ## 
    ## $c
    ## [1] "ç"
    ## 
    ## $C
    ## [1] "Ç"
    ## 
    ## $e
    ## [1] "è" "é" "ê" "ë"
    ## 
    ## $E
    ## [1] "È" "É" "Ê" "Ë"
    ## 
    ## $i
    ## [1] "ì" "í" "î" "ï"
    ## 
    ## $I
    ## [1] "Ì" "Í" "Î" "Ï"
    ## 
    ## $o
    ## [1] "ò" "ó" "ô" "õ" "ö" "ø"
    ## 
    ## $O
    ## [1] "Ò" "Ó" "Ô" "Õ" "Ö" "Ø"
    ## 
    ## $u
    ## [1] "ù" "ú" "û" "ü"
    ## 
    ## $U
    ## [1] "Ù" "Ú" "Û" "Ü"

``` r
to_clean <- c('Header with Nº of meas./hour(in measurement unit)', 
              '\u00c0rtificial p\u00fcnctuat\u00ead attribute')
to_clean
```

    ## [1] "Header with Nº of meas./hour(in measurement unit)"
    ## [2] "Àrtificial pünctuatêd attribute"

``` r
# S3 method for char 
clean(to_clean)
```

    ## [1] "Header_with_N_of_meas_hour_in_measurement_unit"
    ## [2] "Artificial_punctuated_attribute"

Additional arguments:

-   `keep` - vector of chars to remain unchanged;
-   `add_repl` - named vector of additional replacements desired.

``` r
clean(to_clean, keep ='º')
```

    ## [1] "Header_with_Nº_of_meas_hour_in_measurement_unit"
    ## [2] "Artificial_punctuated_attribute"

``` r
clean(to_clean, add_repl = c('H' = 'HHH'))
```

    ## [1] "HHHeader_with_N_of_meas_hour_in_measurement_unit"
    ## [2] "Artificial_punctuated_attribute"

For `data.frame`, further logical options are:

-   `col_names` - T/F for changes in table header;
-   `vars` - T/F for changes in table variables;
-   `byref` - T/F if replacements are made by reference in the table.

``` r
df_to_clean <- data.table(
  '"quoted name"' = 1,
  'text' = LETTERS[1:5] %>% stringr::str_replace_all( c('C'='Ç', 'A' = '"An extract from a book"'))
)

df_to_clean
```

    ##    "quoted name"                     text
    ## 1:             1 "An extract from a book"
    ## 2:             1                        B
    ## 3:             1                        Ç
    ## 4:             1                        D
    ## 5:             1                        E

``` r
# default: col_names = T, vars = F
df_to_clean %>% 
  clean(add_repl = c('"' = '')) %>% 
  clean(vars = T, col_names = F, keep = '\\s')
```

    ##    quoted_name                     text
    ## 1:           1 "An extract from a book"
    ## 2:           1                        B
    ## 3:           1                        C
    ## 4:           1                        D
    ## 5:           1                        E

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

Numerical imputation of vectors. Supported types:

-   Aggregating functions: adjustable-window mean or median;
-   Linear interpolation;
-   Cubic spline interpolation;
-   LOCF (Last Observation Carried Forward);
-   NOCB (Next Observation Carried Backward).

``` r
x <- c(NA, 10, 9, 8, 7, NA, NA, 100, 50)

# mean or median, controling window size
na_input(x, how = 'mean') # window default = Inf
```

    ## [1]  30.66667  10.00000   9.00000   8.00000   7.00000  30.66667  30.66667
    ## [8] 100.00000  50.00000

``` r
na_input(x, how = 'median', window = 3)
```

    ## [1]   9  10   9   8   7   9  29 100  50

``` r
# locf, nocb
na_input(x, how = 'locf')
```

    ## [1]  NA  10   9   8   7   7   7 100  50

``` r
na_input(x, how = 'nocb')
```

    ## [1]  10  10   9   8   7 100 100 100  50

``` r
# Approximations
na_input(x, how = 'lin_interp')
```

    ## [1]  NA  10   9   8   7  38  69 100  50

``` r
na_input(x, how = 'cub_spline')
```

    ## [1]  13.67734  10.00000   9.00000   8.00000   7.00000  39.60130  83.71901
    ## [8] 100.00000  50.00000
