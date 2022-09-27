Tidy Data
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
options(tibble.print_min = 5)
```

# pivot_longer

## Instructions

``` r
pulse_df = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names()

pulse_df
```

    ## # A tibble: 1,087 × 7
    ##      id   age sex   bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##   <dbl> <dbl> <chr>        <dbl>         <dbl>         <dbl>         <dbl>
    ## 1 10003  48.0 male             7             1             2             0
    ## 2 10015  72.5 male             6            NA            NA            NA
    ## 3 10022  58.5 male            14             3             8            NA
    ## 4 10026  72.7 male            20             6            18            16
    ## 5 10035  60.4 male             4             0             1             2
    ## # … with 1,082 more rows

``` r
pulse_tidy_data = 
  pivot_longer(
    pulse_df, # the dataset that you'd like to pivot
    bdi_score_bl:bdi_score_12m, # the columns should be pivoted in
    names_to = "visit", # the new name
    values_to = "bdi")

pulse_tidy_data
```

    ## # A tibble: 4,348 × 5
    ##      id   age sex   visit           bdi
    ##   <dbl> <dbl> <chr> <chr>         <dbl>
    ## 1 10003  48.0 male  bdi_score_bl      7
    ## 2 10003  48.0 male  bdi_score_01m     1
    ## 3 10003  48.0 male  bdi_score_06m     2
    ## 4 10003  48.0 male  bdi_score_12m     0
    ## 5 10015  72.5 male  bdi_score_bl      6
    ## # … with 4,343 more rows

``` r
pulse_tidy_data = 
  pivot_longer(
    pulse_df,
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    names_prefix = "bdi_score_",
    values_to = "bdi") %>%
  mutate(
    visit = replace(visit, visit == "bl", "00m"),
    visit = factor(visit)
  )

pulse_tidy_data
```

    ## # A tibble: 4,348 × 5
    ##      id   age sex   visit   bdi
    ##   <dbl> <dbl> <chr> <fct> <dbl>
    ## 1 10003  48.0 male  00m       7
    ## 2 10003  48.0 male  01m       1
    ## 3 10003  48.0 male  06m       2
    ## 4 10003  48.0 male  12m       0
    ## 5 10015  72.5 male  00m       6
    ## # … with 4,343 more rows

``` r
pulse_df = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names() %>%
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    names_prefix = "bdi_score_",
    values_to = "bdi") %>%
  relocate(visit) %>%
  mutate(
    visit = replace(visit, visit == "bl", "00m"),
    visit = factor(visit)
    ) %>%
  arrange(id, visit)

print(pulse_df, n = 12)
```

    ## # A tibble: 4,348 × 5
    ##    visit    id   age sex     bdi
    ##    <fct> <dbl> <dbl> <chr> <dbl>
    ##  1 00m   10003  48.0 male      7
    ##  2 01m   10003  48.0 male      1
    ##  3 06m   10003  48.0 male      2
    ##  4 12m   10003  48.0 male      0
    ##  5 00m   10015  72.5 male      6
    ##  6 01m   10015  72.5 male     NA
    ##  7 06m   10015  72.5 male     NA
    ##  8 12m   10015  72.5 male     NA
    ##  9 00m   10022  58.5 male     14
    ## 10 01m   10022  58.5 male      3
    ## 11 06m   10022  58.5 male      8
    ## 12 12m   10022  58.5 male     NA
    ## # … with 4,336 more rows

## Learning Assessment

``` r
litters_df = read_csv("./data/FAS_litters.csv") %>% # command + shift + m
  janitor::clean_names() %>% 
  select(litter_number, ends_with("weight")) %>% 
  pivot_longer(
    gd0_weight:gd18_weight,
    names_to = "gd", 
    values_to = "weight") %>% 
  mutate(
    gd = recode(gd, "gd0_weight" = 0, "gd18_weight" = 18)
  )
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
