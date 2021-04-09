#' separate_uk_cols
#'
#' @param data data.frame or tibble
#' @param col column to be separated
#' @param pattern regular expression pattern
#' @param into_prefix prefix for new columns
#'
#' @export separate_uk_cols
#'
separate_uk_cols <- function(data, col, pattern = "[^[:alnum:]]+", into_prefix) {
  in_pattern <- pattern
  in_data <- tibble::as_tibble(data)
  in_col <- as.character(col)
  out_cols <- stringr::str_split_fixed(in_data[[in_col]],
    pattern = in_pattern,
    n = Inf
  )
  out_cols[which(out_cols == "")] <- NA
  m <- dim(out_cols)[2]
  colnames(out_cols) <- paste(into_prefix, 1:m, sep = "_")
  out_cols <- tibble::as_tibble(out_cols)
  out_tibble <- dplyr::bind_cols(in_data, out_cols)
  return(out_tibble)
}
