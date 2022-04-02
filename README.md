
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mminer

<!-- badges: start -->
<!-- badges: end -->

The mminer package is a simple wrapper for [Marketing Miner Profilers
API](https://help.marketingminer.com/en/article/profilers-api/).
Currently it provides functions to fetch search volume data only.
Fetched data are cached locally in a file and used to save credits when
necessary.

## Installation

You can install the development version of mminer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MarekProkop/mminer")
```

## After instalation (important)

You have to get your own [API key for Marketing Miner Profilers
API](https://www.marketingminer.com/en/settings/api-settings) first.
Then save the key in an environment variable named
‘MARKETING\_MINER\_API\_KEY’. If you don’t do that, you will have to
provide your API key in the `api_key` argument of all function calls.

## Examples

To get search volume data for a single search query:

``` r
library(mminer)
get_volume_data("seo", lang = "cs")
#> # A tibble: 1 x 19
#>   keyword search_volume cpc_value cpc_text cpc_currency_code yoy_change
#>   <chr>           <int>     <dbl> <chr>    <chr>                  <dbl>
#> 1 seo              5800      28.0 28.01 Kč CZK                     12.5
#> # ... with 13 more variables: peak_month <chr>, monthly_sv_07 <int>,
#> #   monthly_sv_08 <int>, monthly_sv_09 <int>, monthly_sv_10 <int>,
#> #   monthly_sv_11 <int>, monthly_sv_12 <int>, monthly_sv_01 <int>,
#> #   monthly_sv_02 <int>, monthly_sv_03 <int>, monthly_sv_04 <int>,
#> #   monthly_sv_05 <int>, monthly_sv_06 <int>
```

To get search volume data for multiple search queries:

``` r
queries <- c("seo", "seo optimalizace", "seo company")
get_volume_data(queries, lang = "cs")
#> # A tibble: 3 x 19
#>   keyword          search_volume cpc_value cpc_text cpc_currency_code yoy_change
#>   <chr>                    <int>     <dbl> <chr>    <chr>                  <dbl>
#> 1 seo                       5800      28.0 28.01 Kč CZK                    12.5 
#> 2 seo optimalizace           470      45.2 45.21 Kč CZK                     0.8 
#> 3 seo company                210      37.5 37.49 Kč CZK                    -7.71
#> # ... with 13 more variables: peak_month <chr>, monthly_sv_07 <int>,
#> #   monthly_sv_08 <int>, monthly_sv_09 <int>, monthly_sv_10 <int>,
#> #   monthly_sv_11 <int>, monthly_sv_12 <int>, monthly_sv_01 <int>,
#> #   monthly_sv_02 <int>, monthly_sv_03 <int>, monthly_sv_04 <int>,
#> #   monthly_sv_05 <int>, monthly_sv_06 <int>
```

To retrieve data from cache:

``` r
get_volume_cache()
#> # A tibble: 3 x 20
#>   keyword          search_volume cpc_value cpc_text cpc_currency_code yoy_change
#>   <chr>                    <int>     <dbl> <chr>    <chr>                  <dbl>
#> 1 seo                       5800      28.0 28.01 Kč CZK                    12.5 
#> 2 seo company                210      37.5 37.49 Kč CZK                    -7.71
#> 3 seo optimalizace           470      45.2 45.21 Kč CZK                     0.8 
#> # ... with 14 more variables: peak_month <chr>, monthly_sv_07 <int>,
#> #   monthly_sv_08 <int>, monthly_sv_09 <int>, monthly_sv_10 <int>,
#> #   monthly_sv_11 <int>, monthly_sv_12 <int>, monthly_sv_01 <int>,
#> #   monthly_sv_02 <int>, monthly_sv_03 <int>, monthly_sv_04 <int>,
#> #   monthly_sv_05 <int>, monthly_sv_06 <int>, time_stamp <dttm>
```

To calculate costs:

``` r
queries <- c("seo", "seo optimalizace", "seo company", "seo specialista")
get_volume_cost(queries)
#> 1 queries of 4 must be fetched from API. This will cost 3 credits.
```

To delete the cache file:

``` r
delete_volume_cache()
#> [1] TRUE
```

## About caching

By default the cache is a file (rds) in the *mminer-cache* folder, which
is automaticaly created in the current working folder. The folder itself
is not removed by the `delete_volume_cache()` function.

You can change the cache folder by the `cache_path` argument, e.g.:

``` r
get_volume_data("seo", lang = "cs", cache_path = tempdir())
#> # A tibble: 1 x 19
#>   keyword search_volume cpc_value cpc_text cpc_currency_code yoy_change
#>   <chr>           <int>     <dbl> <chr>    <chr>                  <dbl>
#> 1 seo              5800      28.0 28.01 Kč CZK                     12.5
#> # ... with 13 more variables: peak_month <chr>, monthly_sv_07 <int>,
#> #   monthly_sv_08 <int>, monthly_sv_09 <int>, monthly_sv_10 <int>,
#> #   monthly_sv_11 <int>, monthly_sv_12 <int>, monthly_sv_01 <int>,
#> #   monthly_sv_02 <int>, monthly_sv_03 <int>, monthly_sv_04 <int>,
#> #   monthly_sv_05 <int>, monthly_sv_06 <int>
```

Now the default cache dosen’t exist:

``` r
get_volume_cost("seo")
#> 1 queries of 1 must be fetched from API. This will cost 3 credits.
```

But in `tempdir()` it does:

``` r
get_volume_cost("seo", cache_path = tempdir())
#> 0 queries of 1 must be fetched from API. This will cost 0 credits.
```

``` r
delete_volume_cache(tempdir())
#> [1] TRUE
```
