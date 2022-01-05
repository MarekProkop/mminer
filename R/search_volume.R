#' Retrieves search volume data of the provided search queries
#'
#' Calls Marketing Miner Profilers API endpoint '/keywords/search-volume-data'.
#' If a cache exists, the function reads it first and calls the API only for
#' queries that are not cached.
#'
#' API calls are charged according to
#' \url{https://help.marketingminer.com/en/article/profilers-api/}. In the time
#' of writing it was 3 credits per keyword.
#'
#' @param query Queries to retrieve volume data for as a character vector.
#' @param lang A language abbreviation as a character vector of length 1. For
#'   instance "cs".
#' @param api_key Your Marketing Miner API key. Defaults to the environment
#'   variable named "MARKETING_MINER_API_KEY".
#' @param cache A logical value stating whether the resultshould be cached.
#'   Dafaults to "mminer-cache" in working directory.
#' @param cache_name Path to the directory with the cache.
#'
#' @return A tibble (data frame) with search volume data for the provided
#'   queries. Rows are ordered the same as the provided queries.
#' @export
search_volume <- function(
  query, lang = NULL,
  api_key = Sys.getenv("MARKETING_MINER_API_KEY"),
  cache = TRUE, cache_name = "mminer-cache"
) {
  if (length(query) < 1) {
    stop("Argument query has to be of length 1 or more.")
  }

  cached_data <- read_volume(cache_name)
  if (!is.null(cached_data)) {
    cached_data <- cached_data |>
      dplyr::filter(keyword %in% query)
    query_not_cached <- query |>
      purrr::discard(~ .x %in% cached_data$keyword)
  } else {
    query_not_cached <- query
  }

  if (length(query_not_cached) > 0) {
    cont <- search_volume_api_call(query_not_cached, lang, api_key)
    result <- cont$data |>
      purrr::map_dfr(purrr::flatten) |>
      dplyr::rename(
        cpc_value = value,
        cpc_text = text,
        cpc_currency_code = currency_code
      ) |>
      dplyr::rename_with(~ paste0("monthly_sv_", .x), matches("[0-9]{2}"))
    result <- tibble::tibble(keyword = query_not_cached) |>
      dplyr::left_join(result, by = "keyword")
    if (cache) {
      save_volume(result, cache_name)
    }
    if (!is.null(cached_data)) {
      result <- tibble::tibble(keyword = query) |>
        dplyr::left_join(dplyr::bind_rows(cached_data, result), by = "keyword")
    }
  } else {
    result <- tibble::tibble(keyword = query) |>
      dplyr::left_join(cached_data, by = "keyword")
  }
  result
}

search_volume_api_call <- function(query, lang, api_key = Sys.getenv("MARKETING_MINER_API_KEY")) {
  api_url <- "https://profilers-api.marketingminer.com/keywords/search-volume-data"
  if (length(query) == 1) {
    response <- httr::GET(
      url = api_url,
      query = list(api_token = api_key, lang = lang, keyword = query)
    )
  } else if (length(query) > 1) {
    response <- httr::POST(
      url = api_url,
      query = list(api_token = api_key),
      body = list(lang = lang, keywords = query),
      encode = "json"
    )
  }
  httr::content(response, as = "parsed", type = "application/json")
}

search_volume_cache <- function(name = "mminer-cache") {
  read_volume(name)
}

save_volume <- function(df, name) {
  fname <- file.path(name, "volume.rds")
  dir.create(name, showWarnings = FALSE)
  time_stamp <- Sys.time()
  df <- df |>
    dplyr::mutate(time_stamp = time_stamp)

  if (file.exists(fname)) {
    df <- dplyr::bind_rows(readr::read_rds(fname), df) |>
      dplyr::group_by(keyword) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()
  }
  readr::write_rds(df, fname)
}

read_volume <- function(name) {
  fname <- file.path(name, "volume.rds")
  if (file.exists(fname)) {
    readr::read_rds(fname)
  } else {
    NULL
  }
}
