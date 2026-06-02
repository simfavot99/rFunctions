#' Explore the structure of a dataset
#'
#' Prints a `glimpse()` of the data frame, a summary tibble with one row per
#' variable (type, unique values, missing values, and numeric statistics), and
#' frequency tables (top 15 values, ordered by frequency) for every variable.
#'
#' @param df A data frame to explore.
#'
#' @return Invisibly returns the summary tibble (one row per variable).
#' @export
#'
#' @examples
#' structure_of_dataset(mtcars)
structure_of_dataset <- function(df) {
  pacman::p_load(dplyr, purrr)

  numeric_summary <- function(x) {
    if (is.numeric(x)) {
      list(min = min(x, na.rm = TRUE), mean = mean(x, na.rm = TRUE),
           median = median(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
    } else {
      list(min = NA_real_, mean = NA_real_, median = NA_real_, max = NA_real_)
    }
  }

  summary_rows <- map(names(df), function(col) {
    x <- df[[col]]
    nums <- numeric_summary(x)
    tibble(
      variable    = col,
      type        = class(x)[1],
      n_unique    = n_distinct(x, na.rm = TRUE),
      n_missing   = sum(is.na(x)),
      pct_missing = round(mean(is.na(x)) * 100, 1),
      min         = nums$min,
      mean        = nums$mean,
      median      = nums$median,
      max         = nums$max
    )
  })

  result <- bind_rows(summary_rows)

  cat("Dimensions:", nrow(df), "rows x", ncol(df), "columns\n\n")
  glimpse(df)
  cat("\n")

  print(result, n = Inf)

  cat("\n─────────────────────────────────────────\n")
  cat("Frequency tables (top 15 values per variable)\n")
  cat("─────────────────────────────────────────\n\n")

  walk(names(df), function(col) {
    x <- df[[col]]
    freq_tbl <- tibble(value = x) |>
      count(value, name = "frequency") |>
      arrange(desc(frequency)) |>
      mutate(pct = paste0(round(frequency / sum(frequency) * 100, 1), "%"))
    cat("▸", col, "\n")
    if (n_distinct(freq_tbl$frequency) == 1) {
      cat("All", nrow(freq_tbl), "unique values appear with the same frequency (",
          freq_tbl$frequency[1], ").\n")
    }
    print(slice_head(freq_tbl, n = 15), n = Inf)
    cat("\n")
  })

  invisible(result)
}
