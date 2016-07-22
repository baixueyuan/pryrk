#' Print The Ranking Result
#'
#' Prettify and print the ranking result
#'
#' This function prettify the ranking result returned by \code{uwRanking}. First
#' of all, the data structure will be checked, if the warning message occurs,
#' make sure the data is returned by \code{uwRanking} function. Then, the
#' numeric columns are cut to 2 digits to the right of decimal. And the
#' percentage figure is multiplied 100 and pasted "%". At last, the underwriter
#' names are justified with full space, i.e., if the longest underwriter name is
#' 5 chars, then the names with 4 chars will be added one full space to the
#' right side. The column names can be set to Chinese with the parameter.
#'
#' @param data the input ranking data, returned by \code{uwRanking}
#' @param header_cn logical, if TRUE, default, the returned data.frame will have
#'   a Chinese header
#'
#' @return A prettified data.frame which is exactly the same as input data,
#'   however friendlier to be viewed.
#' @export

printRank <- function(data, header_cn=TRUE) {
  # 本函数用于使得到的结果易于观看
  # 主要是将类型为“numeric”的四舍五入，小数位数由nsmall给出，默认为2
  # 列名称中含有“pct”的百分数列乘以100，否则表达不出百分数的效果

  # 检查数据结构
  if (missing(data)) {
    stop('Please give the ranking data!')
  } else {
    dat_str <- sapply(data, class)
    check <- c(underwriter="character",
               qty="numeric",
               qty_pct="numeric",
               qty_rk="integer",
               vol="numeric",
               vol_pct="numeric",
               vol_rk="integer",
               qty_abs="integer",
               qty_abs_rk="integer")
    if (!identical(dat_str, check)) {
      stop(paste('The data given is not qualified,',
                 'please make sure that the data is returned by "uwRanking".'))
    }
  }

  # 处理数据
  ## 保留两位小数，占比数据加百分号%
  num <- names(dat_str)[dat_str=='numeric']
  for (i in num) {
    if (length(grep('pct', i))) {
      data[, i] <- round(data[, i] * 100, 2)
      data[, i] <- paste(format(data[, i], digits=2, nsmall=2), '%', sep='')
    } else {
      data[, i] <- round(data[, i], 2)
    }
  }

  ## 主承销商列用全角空格补齐
  data$underwriter <- as.character(
    sapply(data$underwriter, function(x) {
      len <- stringr::str_length(x)
      n <- max(nchar(data$underwriter)) - len
      if (n > 0) {
        res <- paste(rep('\u3000', n), collapse='')
        res <- paste(x, res, sep='')
        return(res)
      } else {
        return(x)
      }
    })
  )

  # 列名称改为中文
  if (header_cn) {
    # colnames(rk) <- c('主承销商', '数量', '数量占比', '数量排名',
    #                   '规模', '规模占比', '规模排名', '绝对数量',
    #                   '绝对数量排名')
    colnames(data) <- c("\u4e3b\u627f\u9500\u5546",
                      "\u6570\u91cf",
                      "\u6570\u91cf\u5360\u6bd4",
                      "\u6570\u91cf\u6392\u540d",
                      "\u89c4\u6a21",
                      "\u89c4\u6a21\u5360\u6bd4",
                      "\u89c4\u6a21\u6392\u540d",
                      "\u7edd\u5bf9\u6570\u91cf",
                      "\u7edd\u5bf9\u6570\u91cf\u6392\u540d")
  }
  print(data)
}
