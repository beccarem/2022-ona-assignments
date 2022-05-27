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

# 2. Regression Model

# 3. Regression Model with Gender
