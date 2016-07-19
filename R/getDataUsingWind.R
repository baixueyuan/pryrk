#' Read Data From Wind Client
#'
#' Read primary bond market data from Wind client and return the processed data.
#'
#' This functions is used to read the Excel file which is exported by Wind
#' client. Only the columns useful for the further work will be maintain in the
#' result \code{data.frame}. Reading Excel file uses the package \code{readxl}.
#'
#' @param file a "character" string giving the file name path. If missing, the
#'   default file name is "bond.xlsx".
#'
#' @return A "clean" data.frame which can be used for further use.
#' @export
#' @importFrom readxl read_excel

getDataUsingWind <- function(file) {
  # 默认的文件名为当前工作目录下的“bond.xlsx”
  if (missing(file)) file <- 'bond.xlsx'
  pri_list <- suppressWarnings(readxl::read_excel(file, sheet=1))
  colnames(pri_list) <- gsub('(\u4ebf)', '', colnames(pri_list), fixed=TRUE)
  colnames(pri_list) <- gsub('(\u5e74)', '', colnames(pri_list), fixed=TRUE)
  colnames(pri_list) <- gsub('.', '', colnames(pri_list), fixed=TRUE)
  # col_names <- c('交易代码', '债券简称', '发行规模', '计划发行规模',
  #                '主承销商', '发行起始日', '起息日')
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
