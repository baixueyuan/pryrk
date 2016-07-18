#' Read Data From Wind Client
#'
#' Read Data From Wind Client
#'
#' Read Data From Wind Client
#'
#' @param file the file name
#'
#' @return a data.frame
#' @export
#'

getDataUsingWind <- function(file) {
  pri_list <- suppressWarnings(readxl::read_excel(file, sheet=1))
  colnames(pri_list) <- gsub('(\u4ebf)', '', colnames(pri_list), fixed=TRUE)
  colnames(pri_list) <- gsub('(\u5e74)', '', colnames(pri_list), fixed=TRUE)
  colnames(pri_list) <- gsub('.', '', colnames(pri_list), fixed=TRUE)
  col_names <- c("\u4ea4\u6613\u4ee3\u7801", "\u503a\u5238\u7b80\u79f0",
                 "\u53d1\u884c\u89c4\u6a21",
                 "\u8ba1\u5212\u53d1\u884c\u89c4\u6a21",
                 "\u4e3b\u627f\u9500\u5546", "\u53d1\u884c\u8d77\u59cb\u65e5",
                 "\u8d77\u606f\u65e5")
  res <- pri_list[, col_names]
  colnames(res) <- c('code', 'name', 'amount', 'amountplan', 'underwriter',
                     'initdate', 'carrydate')
  if (any(is.na(res$amount))) {
    res$amount[is.na(res$amount)] <- res$amountplan[is.na(res$amount)]
  }
  res$amountplan <- NULL
  res$underwriter <- organizeUnderwriter(res$underwriter)
  res <- na.omit(res)
  return(res)
}
