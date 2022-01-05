mmr_suggestion <- function(keyword, lang = NULL, api_key = Sys.getenv("MARKETING_MINER_API_KEY")) {
  endpoint <- "https://profilers-api.marketingminer.com/keywords/suggestions"
  if (length(keyword) == 1) {
    response <- httr::GET(
      url = endpoint,
      query = list(api_token = api_key, lang = lang, keyword = keyword)
    )
  } else {
    stop("Argument keyword has to be of length 1.")
  }
  httr::content(response, as = "parsed", type = "application/json")$data$keywords |>
    purrr::map(function(x) {
      list(
        keyword = x$keyword,
        search_volume = x$search_volume,
        cpc = x$cpc$value,
        difficulty = x$difficulty
      )
    }) |>
    dplyr::bind_rows()
}
