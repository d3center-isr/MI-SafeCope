mssd <- function (x, group = NULL, lag = 1, na.rm = TRUE) 
{
  if (is.null(group)) {
    if (is.vector(x)) {
      if(sum(is.na(diff(x)))==NROW(as.matrix(diff(x)))){
        result <- NA
      }else{
        result <- sum(diff(x, lag = lag, na.rm = na.rm)^2, 
                      na.rm = na.rm)/(sum(!is.na(x)) - lag)
      }
    }
    else {
      x <- as.matrix(x)
      if (NCOL(x) == 1) {
        if(sum(is.na(diff(x)))==NROW(as.matrix(diff(x)))){
          result <- NA
        }else{
          result <- sum(diff(x, lag = lag, na.rm = na.rm)^2, 
                        na.rm = na.rm)/(sum(!is.na(x)) - lag)
        }
      }
      else {
        n <- colSums(!is.na(as.matrix(x))) - 1 - lag
        result <- colSums(diff(as.matrix(x), lag = lag, na.rm = na.rm)^2, na.rm = na.rm)/n
        result[which(colSums(is.na(diff(as.matrix(x))))==NROW(diff(as.matrix(x))))] <- NA
      }
   }
  }
  else{
    x <- as.matrix(x)
    if (NROW(group) != NROW(x)) 
      group <- x[, group]
    nvar <- ncol(x)
    cname <- colnames(x)
    temp <- by(x, group, mssd, na.rm = na.rm, lag = lag)
    rownn <- lapply(temp, is.null)
    if (sum(as.integer(rownn)) > 0) {
      rown <- names(temp)[-which(rownn == TRUE)]
    }
    else {
      rown <- names(temp)
    }
    result <- t(matrix(unlist(temp), nrow = nvar))
    colnames(result) <- cname
    rownames(result) <- rown
  }
  return(result)
}

rmssd <- function (x, group = NULL, lag = 1, na.rm = TRUE) 
{
  return(sqrt(mssd(x, group = group, lag = lag, na.rm = na.rm)))
}