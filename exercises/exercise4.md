Exercise 4
================

# Import Data from Exercise 3

Data contains applications information and centrality scores for
examiners in work group 168.

``` r
data <- read_csv("final_ex3.csv")
```

    ## Rows: 5063 Columns: 30

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (11): application_number, examiner_name_last, examiner_name_first, exam...
    ## dbl  (14): examiner_id, examiner_art_unit, appl_status_code, tc, tenure_days...
    ## date  (5): filing_date, patent_issue_date, abandon_date, earliest_date, late...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data
```

    ## # A tibble: 5,063 x 30
    ##    examiner_id application_number filing_date examiner_name_la~ examiner_name_f~
    ##          <dbl> <chr>              <date>      <chr>             <chr>           
    ##  1       61276 12380877           2009-03-03  DO                PENSEE          
    ##  2       61276 12499548           2009-07-08  DO                PENSEE          
    ##  3       61276 13032316           2011-02-22  DO                PENSEE          
    ##  4       61276 10726337           2003-11-30  DO                PENSEE          
    ##  5       61276 10160575           2002-05-31  DO                PENSEE          
    ##  6       61276 13098932           2011-05-02  DO                PENSEE          
    ##  7       61276 13385427           2012-02-21  DO                PENSEE          
    ##  8       61276 13115230           2011-05-25  DO                PENSEE          
    ##  9       61276 12571004           2009-09-30  DO                PENSEE          
    ## 10       61276 13566195           2012-08-03  DO                PENSEE          
    ## # ... with 5,053 more rows, and 25 more variables: examiner_name_middle <chr>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender <chr>, race <chr>, earliest_date <date>,
    ## #   latest_date <date>, tenure_days <dbl>, tenure_years <dbl>, workgroup <dbl>,
    ## #   degree <dbl>, eigen <dbl>, pagerank <dbl>, authority <dbl>, hub <dbl>, ...

# 1. Create ‘app\_proc\_time’ variable

Only valid for patent issued or abandoned.

``` r
data <- data %>%
  mutate(app_proc_time = case_when(
    disposal_type == 'ISS' ~ as.numeric(as.Date(patent_issue_date) - as.Date(filing_date)),
    disposal_type == 'ABN' ~ as.numeric(as.Date(abandon_date) - as.Date(filing_date))))
data
```

    ## # A tibble: 5,063 x 31
    ##    examiner_id application_number filing_date examiner_name_la~ examiner_name_f~
    ##          <dbl> <chr>              <date>      <chr>             <chr>           
    ##  1       61276 12380877           2009-03-03  DO                PENSEE          
    ##  2       61276 12499548           2009-07-08  DO                PENSEE          
    ##  3       61276 13032316           2011-02-22  DO                PENSEE          
    ##  4       61276 10726337           2003-11-30  DO                PENSEE          
    ##  5       61276 10160575           2002-05-31  DO                PENSEE          
    ##  6       61276 13098932           2011-05-02  DO                PENSEE          
    ##  7       61276 13385427           2012-02-21  DO                PENSEE          
    ##  8       61276 13115230           2011-05-25  DO                PENSEE          
    ##  9       61276 12571004           2009-09-30  DO                PENSEE          
    ## 10       61276 13566195           2012-08-03  DO                PENSEE          
    ## # ... with 5,053 more rows, and 26 more variables: examiner_name_middle <chr>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender <chr>, race <chr>, earliest_date <date>,
    ## #   latest_date <date>, tenure_days <dbl>, tenure_years <dbl>, workgroup <dbl>,
    ## #   degree <dbl>, eigen <dbl>, pagerank <dbl>, authority <dbl>, hub <dbl>, ...

Patent pending rows are removed.

``` r
data <- data %>% 
  filter_at(vars(app_proc_time), all_vars(!is.na(.)))
data
```

    ## # A tibble: 3,416 x 31
    ##    examiner_id application_number filing_date examiner_name_la~ examiner_name_f~
    ##          <dbl> <chr>              <date>      <chr>             <chr>           
    ##  1       61276 12380877           2009-03-03  DO                PENSEE          
    ##  2       61276 12499548           2009-07-08  DO                PENSEE          
    ##  3       61276 13032316           2011-02-22  DO                PENSEE          
    ##  4       61276 10726337           2003-11-30  DO                PENSEE          
    ##  5       61276 10160575           2002-05-31  DO                PENSEE          
    ##  6       61276 13098932           2011-05-02  DO                PENSEE          
    ##  7       61276 13385427           2012-02-21  DO                PENSEE          
    ##  8       61276 13115230           2011-05-25  DO                PENSEE          
    ##  9       61276 12571004           2009-09-30  DO                PENSEE          
    ## 10       61276 13566195           2012-08-03  DO                PENSEE          
    ## # ... with 3,406 more rows, and 26 more variables: examiner_name_middle <chr>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender <chr>, race <chr>, earliest_date <date>,
    ## #   latest_date <date>, tenure_days <dbl>, tenure_years <dbl>, workgroup <dbl>,
    ## #   degree <dbl>, eigen <dbl>, pagerank <dbl>, authority <dbl>, hub <dbl>, ...

# 2. Regression Model

New variable: number of applications per examiner

``` r
data <- data %>% 
  group_by(examiner_id) %>% 
  mutate(numapps = n())
data
```

    ## # A tibble: 3,416 x 32
    ## # Groups:   examiner_id [18]
    ##    examiner_id application_number filing_date examiner_name_la~ examiner_name_f~
    ##          <dbl> <chr>              <date>      <chr>             <chr>           
    ##  1       61276 12380877           2009-03-03  DO                PENSEE          
    ##  2       61276 12499548           2009-07-08  DO                PENSEE          
    ##  3       61276 13032316           2011-02-22  DO                PENSEE          
    ##  4       61276 10726337           2003-11-30  DO                PENSEE          
    ##  5       61276 10160575           2002-05-31  DO                PENSEE          
    ##  6       61276 13098932           2011-05-02  DO                PENSEE          
    ##  7       61276 13385427           2012-02-21  DO                PENSEE          
    ##  8       61276 13115230           2011-05-25  DO                PENSEE          
    ##  9       61276 12571004           2009-09-30  DO                PENSEE          
    ## 10       61276 13566195           2012-08-03  DO                PENSEE          
    ## # ... with 3,406 more rows, and 27 more variables: examiner_name_middle <chr>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender <chr>, race <chr>, earliest_date <date>,
    ## #   latest_date <date>, tenure_days <dbl>, tenure_years <dbl>, workgroup <dbl>,
    ## #   degree <dbl>, eigen <dbl>, pagerank <dbl>, authority <dbl>, hub <dbl>, ...

Checking the numbers of applications are correct.

``` r
data %>% count(examiner_id)
```

    ## # A tibble: 18 x 2
    ## # Groups:   examiner_id [18]
    ##    examiner_id     n
    ##          <dbl> <int>
    ##  1       61276   119
    ##  2       63316   362
    ##  3       65654   138
    ##  4       65805   140
    ##  5       67013   269
    ##  6       69917    31
    ##  7       70858   247
    ##  8       70881   294
    ##  9       71861   289
    ## 10       80167   276
    ## 11       82047   255
    ## 12       82105    44
    ## 13       85987   209
    ## 14       95604   216
    ## 15       95634   175
    ## 16       95784   116
    ## 17       96697    34
    ## 18       97910   202

Correlation

``` r
cor(data[c("app_proc_time", "degree", "tenure_years", "numapps")])
```

    ##               app_proc_time       degree tenure_years     numapps
    ## app_proc_time   1.000000000  0.006617166   0.09156867 -0.24841089
    ## degree          0.006617166  1.000000000   0.07007095 -0.04961332
    ## tenure_years    0.091568665  0.070070948   1.00000000  0.18818981
    ## numapps        -0.248410894 -0.049613316   0.18818981  1.00000000

Run the regression with control variables: tenure and number of
application

``` r
attach(data)
reg = lm(app_proc_time ~ degree + tenure_years + numapps)
summary(reg)
```

    ## 
    ## Call:
    ## lm(formula = app_proc_time ~ degree + tenure_years + numapps)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1464.4  -457.5  -135.0   286.8  3929.5 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  395.8016   163.9725   2.414   0.0158 *  
    ## degree        -0.2613     0.2497  -1.047   0.2953    
    ## tenure_years  86.2048     9.9754   8.642   <2e-16 ***
    ## numapps       -2.6671     0.1614 -16.521   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 703.2 on 3412 degrees of freedom
    ## Multiple R-squared:  0.08184,    Adjusted R-squared:  0.08103 
    ## F-statistic: 101.4 on 3 and 3412 DF,  p-value: < 2.2e-16

The regression results show a negative relationship between the degree
centrality and the time it takes to process an application. Which would
mean that the examiners who seek or provide advice more often spend less
time on applications on average. However, since there is no
statistically significant linear dependence of the mean of this variable
‘app\_proc\_time’ on ‘degree centrality’ we can’t conclude anything
based on this.

If we run a regression on a bigger sample or if we add more control
variables we might have significance.

# 3. Regression Model with Gender

Create dummy variables (to run 2 regressions)

``` r
data <- dummy_cols(data,  select_columns = "gender")
data
```

    ## # A tibble: 3,416 x 35
    ##    examiner_id application_number filing_date examiner_name_la~ examiner_name_f~
    ##          <dbl> <chr>              <date>      <chr>             <chr>           
    ##  1       61276 12380877           2009-03-03  DO                PENSEE          
    ##  2       61276 12499548           2009-07-08  DO                PENSEE          
    ##  3       61276 13032316           2011-02-22  DO                PENSEE          
    ##  4       61276 10726337           2003-11-30  DO                PENSEE          
    ##  5       61276 10160575           2002-05-31  DO                PENSEE          
    ##  6       61276 13098932           2011-05-02  DO                PENSEE          
    ##  7       61276 13385427           2012-02-21  DO                PENSEE          
    ##  8       61276 13115230           2011-05-25  DO                PENSEE          
    ##  9       61276 12571004           2009-09-30  DO                PENSEE          
    ## 10       61276 13566195           2012-08-03  DO                PENSEE          
    ## # ... with 3,406 more rows, and 30 more variables: examiner_name_middle <chr>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender <chr>, race <chr>, earliest_date <date>,
    ## #   latest_date <date>, tenure_days <dbl>, tenure_years <dbl>, workgroup <dbl>,
    ## #   degree <dbl>, eigen <dbl>, pagerank <dbl>, authority <dbl>, hub <dbl>, ...

Regression using gender\_male

``` r
attach(data)
```

    ## The following objects are masked from data (pos = 3):
    ## 
    ##     abandon_date, app_proc_time, appl_status_code, appl_status_date,
    ##     application_number, authority, betweenness, closeness, degree,
    ##     disposal_type, earliest_date, eigen, examiner_art_unit,
    ##     examiner_id, examiner_name_first, examiner_name_last,
    ##     examiner_name_middle, filing_date, gender, hub, latest_date,
    ##     numapps, pagerank, patent_issue_date, patent_number, race, tc,
    ##     tenure_days, tenure_years, uspc_class, uspc_subclass, workgroup

``` r
reg_gender = lm(app_proc_time ~ degree + tenure_years + numapps + gender_male + degree * gender_male)
summary(reg_gender)
```

    ## 
    ## Call:
    ## lm(formula = app_proc_time ~ degree + tenure_years + numapps + 
    ##     gender_male + degree * gender_male)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1526.8  -438.7  -125.3   275.6  3895.6 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        357.6899   165.5701   2.160   0.0308 *  
    ## degree               0.4221     0.8032   0.526   0.5992    
    ## tenure_years        83.6729    10.1445   8.248 2.37e-16 ***
    ## numapps             -2.7964     0.1809 -15.457  < 2e-16 ***
    ## gender_male        148.4322    32.5842   4.555 5.44e-06 ***
    ## degree:gender_male  -1.4064     0.8772  -1.603   0.1090    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 675.2 on 3036 degrees of freedom
    ##   (374 observations deleted due to missingness)
    ## Multiple R-squared:  0.08916,    Adjusted R-squared:  0.08766 
    ## F-statistic: 59.44 on 5 and 3036 DF,  p-value: < 2.2e-16

Regression using gender\_female

``` r
reg_gender = lm(app_proc_time ~ degree + tenure_years + numapps + gender_female + degree * gender_female)
summary(reg_gender)
```

    ## 
    ## Call:
    ## lm(formula = app_proc_time ~ degree + tenure_years + numapps + 
    ##     gender_female + degree * gender_female)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1526.8  -438.7  -125.3   275.6  3895.6 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           506.1221   161.3155   3.137 0.001720 ** 
    ## degree                 -0.9843     0.2767  -3.557 0.000381 ***
    ## tenure_years           83.6729    10.1445   8.248 2.37e-16 ***
    ## numapps                -2.7964     0.1809 -15.457  < 2e-16 ***
    ## gender_female        -148.4322    32.5842  -4.555 5.44e-06 ***
    ## degree:gender_female    1.4064     0.8772   1.603 0.108995    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 675.2 on 3036 degrees of freedom
    ##   (374 observations deleted due to missingness)
    ## Multiple R-squared:  0.08916,    Adjusted R-squared:  0.08766 
    ## F-statistic: 59.44 on 5 and 3036 DF,  p-value: < 2.2e-16

The fact that some of coefficients are not statistically significant
makes it hard to draw definitive conclusions. But from what we can
observe, men spend more time on average to process applications. Men
with higher degree centrality are little more productive than men
without. For women, the degree centrality coefficient is significant,
which means that the higher the degree centrality is the least amount of
time it takes to process an application.

Conclusions for USPTO: Examiners who reach out to their colleagues are
more efficient than other. There is a difference in productivity between
genders that will need to be explored further in order to understand the
underlying cause.

# Extra: Analysis on work group 220

Transform data

``` r
df <- read_csv("final_ex3_2.csv")
```

    ## Rows: 8098 Columns: 30

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (10): application_number, examiner_name_last, examiner_name_first, exam...
    ## dbl  (15): examiner_id, examiner_art_unit, uspc_class, appl_status_code, tc,...
    ## date  (5): filing_date, patent_issue_date, abandon_date, earliest_date, late...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df <- df %>%
  mutate(app_proc_time = case_when(
    disposal_type == 'ISS' ~ as.numeric(as.Date(patent_issue_date) - as.Date(filing_date)),
    disposal_type == 'ABN' ~ as.numeric(as.Date(abandon_date) - as.Date(filing_date))))

df <- df %>% filter_at(vars(app_proc_time), all_vars(!is.na(.)))
df <- df %>% group_by(examiner_id) %>% mutate(numapps = n())
df
```

    ## # A tibble: 6,541 x 32
    ## # Groups:   examiner_id [38]
    ##    examiner_id application_number filing_date examiner_name_la~ examiner_name_f~
    ##          <dbl> <chr>              <date>      <chr>             <chr>           
    ##  1       59491 11002089           2004-12-03  NGUYEN            VAN             
    ##  2       59491 13175771           2011-07-01  NGUYEN            VAN             
    ##  3       59491 11842803           2007-08-21  NGUYEN            VAN             
    ##  4       59491 13810024           2013-01-14  NGUYEN            VAN             
    ##  5       59491 13014085           2011-01-26  NGUYEN            VAN             
    ##  6       59491 13136359           2011-07-29  NGUYEN            VAN             
    ##  7       59491 13223839           2011-09-01  NGUYEN            VAN             
    ##  8       59491 13089123           2011-04-18  NGUYEN            VAN             
    ##  9       59491 13151094           2011-06-01  NGUYEN            VAN             
    ## 10       59491 14259451           2014-04-23  NGUYEN            VAN             
    ## # ... with 6,531 more rows, and 27 more variables: examiner_name_middle <chr>,
    ## #   examiner_art_unit <dbl>, uspc_class <dbl>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender <chr>, race <chr>, earliest_date <date>,
    ## #   latest_date <date>, tenure_days <dbl>, tenure_years <dbl>, workgroup <dbl>,
    ## #   degree <dbl>, eigen <dbl>, pagerank <dbl>, authority <dbl>, hub <dbl>, ...

Correlation

``` r
cor(df[c("app_proc_time", "degree", "tenure_years", "numapps")])
```

    ##               app_proc_time       degree tenure_years      numapps
    ## app_proc_time    1.00000000  0.051359418    0.1661681  0.039564363
    ## degree           0.05135942  1.000000000    0.2681059 -0.005690885
    ## tenure_years     0.16616808  0.268105948    1.0000000  0.632974209
    ## numapps          0.03956436 -0.005690885    0.6329742  1.000000000

Regression

``` r
attach(df)
```

    ## The following objects are masked from data (pos = 3):
    ## 
    ##     abandon_date, app_proc_time, appl_status_code, appl_status_date,
    ##     application_number, authority, betweenness, closeness, degree,
    ##     disposal_type, earliest_date, eigen, examiner_art_unit,
    ##     examiner_id, examiner_name_first, examiner_name_last,
    ##     examiner_name_middle, filing_date, gender, hub, latest_date,
    ##     numapps, pagerank, patent_issue_date, patent_number, race, tc,
    ##     tenure_days, tenure_years, uspc_class, uspc_subclass, workgroup

    ## The following objects are masked from data (pos = 4):
    ## 
    ##     abandon_date, app_proc_time, appl_status_code, appl_status_date,
    ##     application_number, authority, betweenness, closeness, degree,
    ##     disposal_type, earliest_date, eigen, examiner_art_unit,
    ##     examiner_id, examiner_name_first, examiner_name_last,
    ##     examiner_name_middle, filing_date, gender, hub, latest_date,
    ##     numapps, pagerank, patent_issue_date, patent_number, race, tc,
    ##     tenure_days, tenure_years, uspc_class, uspc_subclass, workgroup

``` r
reg = lm(app_proc_time ~ degree + tenure_years + numapps)
summary(reg)
```

    ## 
    ## Call:
    ## lm(formula = app_proc_time ~ degree + tenure_years + numapps)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1492.4  -485.9   -74.4   399.4  3085.4 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  803.9884    47.4306  16.951  < 2e-16 ***
    ## degree        -0.6492     0.5960  -1.089    0.276    
    ## tenure_years  57.7814     4.0039  14.431  < 2e-16 ***
    ## numapps       -0.5826     0.0828  -7.036 2.18e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 686.4 on 6537 degrees of freedom
    ## Multiple R-squared:  0.03497,    Adjusted R-squared:  0.03453 
    ## F-statistic: 78.96 on 3 and 6537 DF,  p-value: < 2.2e-16

Dummies

``` r
df <- dummy_cols(df,  select_columns = "gender")
```

Regression with gender

``` r
attach(df)
```

    ## The following objects are masked from df (pos = 3):
    ## 
    ##     abandon_date, app_proc_time, appl_status_code, appl_status_date,
    ##     application_number, authority, betweenness, closeness, degree,
    ##     disposal_type, earliest_date, eigen, examiner_art_unit,
    ##     examiner_id, examiner_name_first, examiner_name_last,
    ##     examiner_name_middle, filing_date, gender, hub, latest_date,
    ##     numapps, pagerank, patent_issue_date, patent_number, race, tc,
    ##     tenure_days, tenure_years, uspc_class, uspc_subclass, workgroup

    ## The following objects are masked from data (pos = 4):
    ## 
    ##     abandon_date, app_proc_time, appl_status_code, appl_status_date,
    ##     application_number, authority, betweenness, closeness, degree,
    ##     disposal_type, earliest_date, eigen, examiner_art_unit,
    ##     examiner_id, examiner_name_first, examiner_name_last,
    ##     examiner_name_middle, filing_date, gender, gender_female,
    ##     gender_male, gender_NA, hub, latest_date, numapps, pagerank,
    ##     patent_issue_date, patent_number, race, tc, tenure_days,
    ##     tenure_years, uspc_class, uspc_subclass, workgroup

    ## The following objects are masked from data (pos = 5):
    ## 
    ##     abandon_date, app_proc_time, appl_status_code, appl_status_date,
    ##     application_number, authority, betweenness, closeness, degree,
    ##     disposal_type, earliest_date, eigen, examiner_art_unit,
    ##     examiner_id, examiner_name_first, examiner_name_last,
    ##     examiner_name_middle, filing_date, gender, hub, latest_date,
    ##     numapps, pagerank, patent_issue_date, patent_number, race, tc,
    ##     tenure_days, tenure_years, uspc_class, uspc_subclass, workgroup

``` r
reg_gender = lm(app_proc_time ~ degree + tenure_years + numapps + gender_male + degree * gender_male)
summary(reg_gender)
```

    ## 
    ## Call:
    ## lm(formula = app_proc_time ~ degree + tenure_years + numapps + 
    ##     gender_male + degree * gender_male)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2816.36  -456.74   -78.74   379.24  3072.98 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         819.70862   63.60976  12.887  < 2e-16 ***
    ## degree                4.41848    0.74813   5.906 3.73e-09 ***
    ## tenure_years         67.62463    4.62600  14.618  < 2e-16 ***
    ## numapps              -0.71306    0.09134  -7.807 7.07e-15 ***
    ## gender_male        -108.07440   24.62790  -4.388 1.17e-05 ***
    ## degree:gender_male  -13.90446    1.50756  -9.223  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 672.4 on 5123 degrees of freedom
    ##   (1412 observations deleted due to missingness)
    ## Multiple R-squared:  0.07841,    Adjusted R-squared:  0.07751 
    ## F-statistic: 87.17 on 5 and 5123 DF,  p-value: < 2.2e-16

Results from the regressions for work group 220 are different. The small
sample sizes (work group 168 has 18 examiners and work group 220 has 38)
explains why we’re not getting consistent results.
