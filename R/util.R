interleave <- function(set1, set2) {

  if (is.vector(set1)) {
    return(c(set1, set2)[order(c(1:length(set1), 1:length(set2)))])
  }

  if (is.data.frame(set1))
    return(bind_rows(set1, set2)[order(c(1:nrow(set1), 1:nrow(set2))), ])
}
