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
  mutate(gd = recode(gd, "gd0_weight" = 0, "gd18_weight" = 18))
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

In one sense, this is “tidy” because I have a variable for day and a
variable for weight rather that using values in my variable names.
However, it’s less useful if I’m interested in computing or analyzing
weight gain during pregnancy.

# pivot_wider

``` r
analysis_result = tibble(
  group = c("treatment", "treatment", "placebo", "placebo"),
  time = c("pre", "post", "pre", "post"),
  mean = c(4, 8, 3.5, 4)
)

analysis_result
```

    ## # A tibble: 4 × 3
    ##   group     time   mean
    ##   <chr>     <chr> <dbl>
    ## 1 treatment pre     4  
    ## 2 treatment post    8  
    ## 3 placebo   pre     3.5
    ## 4 placebo   post    4

``` r
analysis_results_wide =
  pivot_wider(
    analysis_result, 
    names_from = "time", 
    values_from = "mean")

analysis_results_wide
```

    ## # A tibble: 2 × 3
    ##   group       pre  post
    ##   <chr>     <dbl> <dbl>
    ## 1 treatment   4       8
    ## 2 placebo     3.5     4

# binding rows

``` r
fellowship_ring = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") %>% 
  mutate(movie = "fellowship_ring")

two_towers = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") %>% 
  mutate(movie = "two_towers")

return_king = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") %>% 
  mutate(movie = "return_king")

lotr_tidy = 
  bind_rows(fellowship_ring, two_towers, return_king) %>%
  janitor::clean_names() %>% 
  pivot_longer(
    female:male,
    names_to = "gender", 
    values_to = "words"
  ) %>% 
  mutate(race = str_to_lower(race)) %>% 
  select(movie, everything())
```

# joining datasets

``` r
pup_data = 
  read_csv("./data/FAS_pups.csv") %>%
  janitor::clean_names() %>%
  mutate(
    sex = recode(sex, `1` = "male", `2` = "female"), # `numeric`, "character", back quotes for the numbers, regular quotes for the characters
    sex = factor(sex)) 
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litter_data = 
  read_csv("./data/FAS_litters.csv") %>% 
  janitor::clean_names() %>% 
  separate(group, into = c("dose", "day_of_tx"), sep = 3) %>% # separate after the third character
  relocate(litter_number) %>% 
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    dose = str_to_lower(dose)
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

``` r
fas_data = 
  left_join(pup_data, litter_data, by = "litter_number")

fas_data
```

    ## # A tibble: 313 × 15
    ##   litter_n…¹ sex   pd_ears pd_eyes pd_pi…² pd_walk dose  day_o…³ gd0_w…⁴ gd18_…⁵
    ##   <chr>      <fct>   <dbl>   <dbl>   <dbl>   <dbl> <chr> <chr>     <dbl>   <dbl>
    ## 1 #85        male        4      13       7      11 con   7          19.7    34.7
    ## 2 #85        male        4      13       7      12 con   7          19.7    34.7
    ## 3 #1/2/95/2  male        5      13       7       9 con   7          27      42  
    ## 4 #1/2/95/2  male        5      13       8      10 con   7          27      42  
    ## 5 #5/5/3/83… male        5      13       8      10 con   7          26      41.4
    ## # … with 308 more rows, 5 more variables: gd_of_birth <dbl>,
    ## #   pups_born_alive <dbl>, pups_dead_birth <dbl>, pups_survive <dbl>,
    ## #   wt_gain <dbl>, and abbreviated variable names ¹​litter_number, ²​pd_pivot,
    ## #   ³​day_of_tx, ⁴​gd0_weight, ⁵​gd18_weight

``` r
anti_join(pup_data, litter_data) #pups who dont have a mother data
```

    ## Joining, by = "litter_number"

    ## # A tibble: 9 × 6
    ##   litter_number sex    pd_ears pd_eyes pd_pivot pd_walk
    ##   <chr>         <fct>    <dbl>   <dbl>    <dbl>   <dbl>
    ## 1 #5/3/83/3-2   female       3      12       NA       8
    ## 2 #5/3/83/3-2   female       3      13       NA      10
    ## 3 #7/82/3-2     male         3      12        6       8
    ## 4 #7/82/3-2     male         4      13        5       8
    ## 5 #7/82/3-2     male         3      13        6       8
    ## 6 #7/82/3-2     female       3      13        6       8
    ## 7 #7/82/3-2     female       3      12        6       8
    ## 8 #7/82/3-2     female       3      12        6       8
    ## 9 #7/82/3-2     female       3      12        6       8

## Learning Assessment

``` r
surv_os = read_csv("data/surv_os.csv") %>% 
  janitor::clean_names() %>% 
  rename(id = what_is_your_uni, os = what_operating_system_do_you_use)
```

    ## Rows: 173 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): What is your UNI?, What operating system do you use?
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
surv_pr_git = read_csv("data/surv_program_git.csv") %>% 
  janitor::clean_names() %>% 
  rename(
    id = what_is_your_uni, 
    prog = what_is_your_degree_program,
    git_exp = which_most_accurately_describes_your_experience_with_git)
```

    ## Rows: 135 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): What is your UNI?, What is your degree program?, Which most accurat...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
left_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 175 × 4
    ##   id          os         prog  git_exp                                          
    ##   <chr>       <chr>      <chr> <chr>                                            
    ## 1 student_87  <NA>       MS    Pretty smooth: needed some work to connect Git, …
    ## 2 student_106 Windows 10 Other Pretty smooth: needed some work to connect Git, …
    ## 3 student_66  Mac OS X   MPH   Smooth: installation and connection with GitHub …
    ## 4 student_93  Windows 10 MS    Smooth: installation and connection with GitHub …
    ## 5 student_99  Mac OS X   MS    Smooth: installation and connection with GitHub …
    ## # … with 170 more rows

``` r
inner_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 129 × 4
    ##   id          os         prog  git_exp                                          
    ##   <chr>       <chr>      <chr> <chr>                                            
    ## 1 student_87  <NA>       MS    Pretty smooth: needed some work to connect Git, …
    ## 2 student_106 Windows 10 Other Pretty smooth: needed some work to connect Git, …
    ## 3 student_66  Mac OS X   MPH   Smooth: installation and connection with GitHub …
    ## 4 student_93  Windows 10 MS    Smooth: installation and connection with GitHub …
    ## 5 student_99  Mac OS X   MS    Smooth: installation and connection with GitHub …
    ## # … with 124 more rows

``` r
anti_join(surv_os, surv_pr_git)
```

    ## Joining, by = "id"

    ## # A tibble: 46 × 2
    ##   id          os        
    ##   <chr>       <chr>     
    ## 1 student_86  Mac OS X  
    ## 2 student_91  Windows 10
    ## 3 student_24  Mac OS X  
    ## 4 student_103 Mac OS X  
    ## 5 student_163 Mac OS X  
    ## # … with 41 more rows

``` r
anti_join(surv_pr_git, surv_os)
```

    ## Joining, by = "id"

    ## # A tibble: 15 × 3
    ##    id         prog  git_exp                                                     
    ##    <chr>      <chr> <chr>                                                       
    ##  1 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  2 student_17 PhD   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  3 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  4 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  5 <NA>       MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  6 student_53 MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  7 <NA>       MS    "Smooth: installation and connection with GitHub was easy"  
    ##  8 student_80 PhD   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ##  9 student_16 MPH   "Smooth: installation and connection with GitHub was easy"  
    ## 10 student_98 MS    "Smooth: installation and connection with GitHub was easy"  
    ## 11 <NA>       MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
    ## 12 <NA>       MS    "What's \"Git\" ...?"                                       
    ## 13 <NA>       MS    "Smooth: installation and connection with GitHub was easy"  
    ## 14 <NA>       MPH   "Pretty smooth: needed some work to connect Git, GitHub, an…
    ## 15 <NA>       MS    "Pretty smooth: needed some work to connect Git, GitHub, an…
