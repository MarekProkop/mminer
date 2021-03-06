save_credits <- FALSE

test_that("search_volume_api_call() works", {
  skip_if(save_credits, "Save credits!")
  expect_visible(
    search_volume_api_call(
      query = c("seo", "seo optimalizace"),
      lang = "cs"
    )
  )
})

test_that("search_volume_api_call() fails with wrong language", {
  skip_if(save_credits, "Save credits!")
  expect_error(
    object = search_volume_api_call(
      query = c("seo", "seo optimalizace"),
      lang = "xx"
    )
  )
})

test_that("search_volume_api_call() fails with a query longer than 100 chars", {
  skip_if(save_credits, "Save credits!")
  long_query <- "123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 1"
  expect_error(
    object = search_volume_api_call(
      query = c("seo", long_query),
      lang = "cs"
    )
  )
})

test_that("search_volume_api_call() works with a query 80 characters long", {
  skip_if(save_credits, "Save credits!")
  long_query <- "123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 "
  result <- search_volume_api_call(
    query = c("seo", long_query),
    lang = "cs"
  )
  expect_visible(result)
})

test_that("get_volume_data() with a single query and no cache works", {
  skip_if(save_credits, "Save credits!")
  query <- "seo"
  result <- get_volume_data(
    query,
    lang = "cs", cache = FALSE, cache_path = "not-exist"
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$keyword, query)
  expect_type(result$search_volume, "integer")
})

test_that("get_volume_data() with multiple queries and no cache works", {
  skip_if(save_credits, "Save credits!")
  query <- c("seo", "seo optimalizace", "xxx111yyy222zzz333")
  result <- get_volume_data(
    query,
    lang = "cs", cache = FALSE, cache_path = "not-exist"
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), length(query))
  expect_equal(result$keyword, query)
  expect_type(result$search_volume, "integer")
  expect_equal(result$search_volume[length(query)], NA_integer_)
})

test_that("get_volume_data() with a long query and no cache works", {
  skip_if(save_credits, "Save credits!")
  long_query <- "123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 1"
  query <- c("seo", long_query, "seo optimalizace")
  result <- get_volume_data(
    query,
    lang = "cs", cache = FALSE, cache_path = "not-exist"
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), length(query))
  expect_equal(result$keyword, query)
  expect_type(result$search_volume, "integer")
  expect_equal(result$search_volume[2], NA_integer_)
})

test_that("get_volume_data() with multiple queries, lang = sk and no cache works", {
  skip_if(save_credits, "Save credits!")
  query <- c("seo", "seo optimalizácia", "xxx111yyy222zzz333")
  result <- get_volume_data(
    query,
    lang = "sk", cache = FALSE, cache_path = "not-exist"
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), length(query))
  expect_equal(result$keyword, query)
  expect_type(result$search_volume, "integer")
  expect_equal(result$search_volume[length(query)], NA_integer_)
})

test_that("delete_volume_cache() works", {
  skip_if(save_credits, "Save credits!")
  expect_false(delete_volume_cache(cache_path = tempdir()))
  df <- get_volume_data("seo", lang = "cs", cache_path = tempdir())
  expect_true(delete_volume_cache(cache_path = tempdir()))
  expect_false(delete_volume_cache(cache_path = tempdir()))
})

test_that("get_volume_data() works with cache properly", {
  skip_if(save_credits, "Save credits!")
  query <- "seo"
  delete_volume_cache(cache_path = tempdir())
  df_fetched <- get_volume_data(query, lang = "cs", cache_path = tempdir())
  df_cached <- get_volume_cache(cache_path = tempdir())

  expect_s3_class(df_cached, "tbl_df")
  expect_equal(nrow(df_cached), 1)
  expect_equal(df_cached$keyword, query)
  expect_type(df_cached$search_volume, "integer")
  expect_s3_class(df_cached$time_stamp, "POSIXct")
  expect_equal(df_cached[, -ncol(df_cached)], df_fetched)

  query <- c("seo", "seo optimalizace", "xxx111yyy222zzz333")
  df_fetched_2 <- get_volume_data(query, lang = "cs", cache_path = tempdir())
  df_cached_2 <- get_volume_cache(cache_path = tempdir())
  expect_equal(df_cached_2[, -ncol(df_cached_2)], df_fetched_2)

  df_cached_3 <- get_volume_cache("seo", cache_path = tempdir())
  expect_equal(df_cached_2[1, -ncol(df_cached_2)], df_fetched_2[1, ])

  delete_volume_cache(cache_path = tempdir())
})
