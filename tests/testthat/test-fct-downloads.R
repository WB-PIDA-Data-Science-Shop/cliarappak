check_quantiles <- function(column) {
  q25 <- quantile(column, 0.25, na.rm = TRUE)
  q75 <- quantile(column, 0.75, na.rm = TRUE)
  if (!is.na(q25) & !is.na(q75) & q25 == q75) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}  
