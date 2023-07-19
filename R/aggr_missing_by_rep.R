#' @title aggregate missing count by replicate
#' @param df a data.frame/data.table with columns ordered by the experiment and replicates
#' @param regex string. The format of replicate in the column
#' @param reps the expected number of replicates in each experiment
#' @export
#' 
aggr_missing_by_rep <- function(df, regex = "rep[0-9]", reps = 3){
  df <- setDT(df)
  cols <- which(grepl(regex, colnames(df)))
  iter <- seq(from = cols[1], to = cols[length(cols)], by = reps)
  lst <- lapply(iter, function(i){
    ranges <- i:(i+reps-1)
    df_cond <- df[,..ranges]
    cols_cond <- colnames(df_cond)
    cols_name <- gsub(regex,"",cols_cond)
    cols_name <- gsub("(_$)|(__+)","", cols_name)
    cols_name <- unique(cols_name)
    if (length(cols_name) > 1) stop("Column names should only differ by regex!")
    df_cond <- is.na(df_cond)
    na_sums <- data.table(rowSums(df_cond))
    colnames(na_sums) <- cols_name
    return(na_sums)
  })
  combined <- do.call(cbind, lst)
  return(combined)
}