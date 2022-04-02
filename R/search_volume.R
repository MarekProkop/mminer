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
#' @param cache A logical value stating whether the result should be cached.
#'   Dafaults to "mminer-cache" in working directory.
#' @param cache_path Path to the directory with the cache. Defaults to the
#'   'mminer-cache' in the current working directory.
#'
#' @return A tibble (data frame) with search volume data for the provided
#'   queries. Rows are ordered the same as the provided queries.
#' @export
get_volume_data <- function(query, lang = NULL,
                            api_key = Sys.getenv("MARKETING_MINER_API_KEY"),
                            cache = TRUE, cache_path = "mminer-cache") {

  process_api_result <- function(query, lang, api_key) {
    cont <- search_volume_api_call(query, lang, api_key)
    result <- cont$data |>
      purrr::map_dfr(purrr::flatten) |>
      dplyr::rename(
        cpc_value = .data$value,
        cpc_text = .data$text,
        cpc_currency_code = .data$currency_code
      ) |>
      dplyr::rename_with(~ paste0("monthly_sv_", .x), dplyr::matches("[0-9]{2}"))
    tibble::tibble(keyword = query) |>
      dplyr::left_join(result, by = "keyword")
  }

  checkmate::assert_character(
    query,
    any.missing = FALSE, min.len = 1, unique = TRUE
  )
  checkmate::assert_string(lang)
  checkmate::assert_string(api_key)
  checkmate::assert_flag(cache)
  checkmate::assert(
    checkmate::check_null(cache_path),
    checkmate::check_string(cache_path)
  )

  query <- unique(query)
  cached_data <- get_volume_cache(query, cache_path)
  if (!is.null(cached_data)) {
    cached_data <- cached_data[-ncol(cached_data)]
    query_not_cached <- query |>
      purrr::discard(~ .x %in% cached_data$keyword)
  } else {
    query_not_cached <- query
  }

  if (length(query_not_cached) > 0) {
    chunks <- split(
      query_not_cached, ceiling(seq_along(query_not_cached) / 1000)
    )
    result <- chunks |>
      purrr::map_dfr(process_api_result, lang, api_key)

    if (cache) {
      save_volume(result, cache_path)
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

#' Reads search volume data from cache
#'
#' @param query An optional character vector of queries to read from cache.
#' @param cache_path Path to the directory with the cache. Defaults to the
#'   'mminer-cache' in the current working directory.
#'
#' @return A tibble (data frame) with search volume data from cache. If cache
#'   file doesn't exist, returns NULL. If the _query_ argument is provided,
#'   returns the matching queries only.
#' @export
get_volume_cache <- function(query = NULL, cache_path = "mminer-cache") {
  checkmate::assert(
    checkmate::check_null(query),
    checkmate::check_character(
      query,
      any.missing = FALSE, min.len = 1, unique = TRUE
    )
  )
  checkmate::assert_string(cache_path)

  fname <- file.path(cache_path, "volume.rds")
  if (file.exists(fname)) {
    result <- readr::read_rds(fname)
    if (!is.null(query)) {
      result <- result |>
        dplyr::filter(.data$keyword %in% query)
    }
  } else {
    result <- NULL
  }
  result
}

#' Deletes the entire search volume cache file
#'
#' @param cache_path Path to the directory with the cache. Defaults to the
#'   'mminer-cache' in the current working directory.
#'
#' @return TRUE if file exists, FALSE if it doesn't.
#' @export
delete_volume_cache <- function(cache_path = "mminer-cache") {
  checkmate::assert_string(cache_path)

  fname <- file.path(cache_path, "volume.rds")
  result <- file.exists(fname)
  if (result) {
    file.remove(fname)
  }
  result
}

#' From a search volume cache removes queires fetched from API before a specific
#' date and time
#'
#' @param cache_path Path to the directory with the cache. Defaults to the
#'   'mminer-cache' in the current working directory.
#' @param older_than Date-time. Queries with the older time stamps will be
#'   removed from cache.
#'
#' @return Number of removed queries.
#' @export
prune_volume_cache <- function(cache_path = "mminer-cache", older_than = NULL) {
  checkmate::assert_directory(cache_path, access = "w")
  fname <- file.path(cache_path, "volume.rds")
  checkmate::assert_file(fname, access = "w")

  df <- get_volume_cache(cache_path)
  if (!is.null(df)) {
    before <- nrow(df)
    df <- df |>
      dplyr::filter(.data$time_stamp < older_than)
    readr::write_rds(df, fname)
    return(before - nrow(df))
  } else {
    return(0)
  }
}

#' Calculates costs for a search volume API call
#'
#' Prints a message how many queries are already cached and the count of the
#' rest multiplies by 3 credits.
#'
#' @param query Queries to retrieve volume data for as a character vector.
#' @param cache_path Path to the directory with the cache. Defaults to the
#'   'mminer-cache' in the current working directory.
#'
#' @return None.
#' @export
get_volume_cost <- function(query, cache_path = "mminer-cache") {
  query <- unique(query)
  cached_data <- get_volume_cache(query, cache_path)
  if (!is.null(cached_data)) {
    cached_data <- cached_data[-ncol(cached_data)]
    query_not_cached <- query |>
      purrr::discard(~ .x %in% cached_data$keyword)
  } else {
    query_not_cached <- query
  }
  message(length(query_not_cached), " queries of ", length(query), " must be fetched from API. This will cost ", length(query_not_cached) * 3, " credits.")
}

# Private -----------------------------------------------------------------


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
  if (httr::http_type(response) != "application/json") {
    stop("API did not return json.", call. = FALSE)
  }
  parsed <- httr::content(response, as = "parsed", type = "application/json")
  if (httr::http_error(response)) {
    stop(
      sprintf(
        "Marketing Miner API request failed [%s]\n%s",
        httr::status_code(response),
        parsed$message
      ), call. = FALSE
    )
  }
  parsed
}

save_volume <- function(df, path) {
  fname <- file.path(path, "volume.rds")
  dir.create(path, showWarnings = FALSE)
  time_stamp <- Sys.time()
  df <- df |>
    dplyr::mutate(time_stamp = time_stamp)

  if (file.exists(fname)) {
    df <- dplyr::bind_rows(readr::read_rds(fname), df) |>
      dplyr::group_by(.data$keyword) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()
  }
  readr::write_rds(df, fname)
}
