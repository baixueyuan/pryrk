#' Title
#'
#' @param data
#' @param nsmall
#' @param cut
#' @param header_cn
#'
#' @return
#' @export
#'
#' @examples
printRank <- function(data, nsmall=2, cut=TRUE, header_cn=FALSE) {
  # 本函数用于使得到的结果易于观看
  # 主要是将类型为“numeric”的四舍五入，小数位数由nsmall给出，默认为2
  # 列名称中含有“pct”的百分数列乘以100，否则表达不出百分数的效果
  if (missing(data)) {

  } else {
    dat_str <- sapply(data, class)
    check <- c(underwriter="character",
               qty="numeric",
               qty_pct="numeric",
               qty_rk="integer",
               vol="numeric",
               vol_pct="numeric",
               vol_rk="integer",
               qty_abs="numeric",
               qty_abs_rk="integer")
    if (!identical(dat_str, check)) {
      stop(paste('The data given is not qualified,',
                 'please make sure that the data is returned by "uwRanking".'))
    }
  }
  num <- names(type)[type=='numeric']
  for (i in num) {
    if (length(grep('pct', i))) data[, i] <- data[, i] * 100
    data[, i] <- round(data[, i], nsmall)
  }

  # 为了使汉字列“underwriter”整齐，参数cut如果为TRUE则一律
  # 缩减为4个汉字（8个字符），suppressWarnings去掉警告信息
  if (cut) {
    data$underwriter <-
      suppressWarnings(abbreviate(data$underwriter, 8, strict=TRUE))
  }
  if (header_cn) {
    colnames(data) <- c('主承销商', '数量', '数量占比', '数量排名',
                        '规模', '规模占比', '规模排名', '绝对数量',
                        '绝对数量排名')
  }
  print(data)
}
