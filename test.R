uw1 <- rk$underwriter
rk$underwriter <- uw1
rk$underwriter <- as.character(
  sapply(rk$underwriter, function(x) {
    len <- stringr::str_length(x)
    n <- 5 - len
    if (n > 0) {
      res <- paste(rep('　', n), collapse='')
      res <- paste(x, res, sep='')
      return(res)
    } else {
      return(x)
    }
  })
)

dat_str <- sapply(rk, class)
check <- c(underwriter="character",
           qty="numeric",
           qty_pct="numeric",
           qty_rk="integer",
           vol="numeric",
           vol_pct="numeric",
           vol_rk="integer",
           qty_abs="numeric",
           qty_abs_rk="integer")
