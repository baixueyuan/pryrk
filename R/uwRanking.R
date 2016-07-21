#' Compute the Rank of Underwriter
#'
#' Using the data processed by \code{uwSplit} to compute the ranking result of
#' underwriters.
#'
#' The result includes three indexes, weighted-mean of quantity and volume, and
#' the absolute quantity of underwriting performance. The result use the
#' weighted mean as the order, however it includes all the ranking number,
#' figure and ratio.
#'
#' @param data the input data which is returned by \code{uwSplit}
#' @param header_cn logical, if \code{TRUE} then the result's column names will
#'   be in Chinese, default is \code{FALSE}. Now suspended
#' @param round logical, whether to round the figure to 2 decimal, which will
#'   make it easier to view the results, however the precision is not so
#'   important for this work, default is TRUE. Now suspended
#'
#' @return The data.frame containing the ranking result.
#' @export

uwRanking <- function(data) {
  # 本函数利用uwSplit计算出的数据来得到排名数据
  # 排名可以根据承销数量和承销金额来进行，由参数sort.by控制

  # 用aggregate计算各主承销商的承销数量和承销金额
  rk_qty <- aggregate(data$rk_qty, list(underwriter=data$underwriter), sum)
  rk_vol <- aggregate(data$rk_vol, list(underwriter=data$underwriter), sum)
  colnames(rk_qty)[2] <- 'qty'
  colnames(rk_vol)[2] <- 'vol'

  # 计算承销参与的绝对数量
  rk_qty_abs <- data.frame(t(t(table(data$underwriter))))
  rk_qty_abs[,2] <- NULL
  colnames(rk_qty_abs) <- c('underwriter', 'qty_abs')

  # 将三组承销业绩数据按照主承销商名称“underwriter”合并
  # 计算各家的承销数量和承销金额的市场占比
  rk <- merge(rk_qty, rk_vol, by='underwriter')
  rk <- merge(rk, rk_qty_abs, by='underwriter')
  colnames(rk) <- c('underwriter', 'qty', 'vol', 'qty_abs')
  rk$qty_pct <- rk$qty / sum(rk$qty)
  rk$vol_pct <- rk$vol / sum(rk$vol)

  # 对数据分别按照承销数量和金额进行排名并加入名次列
  # 数据框默认按照承销数量qty排序并重新给定rownames
  # 对于承销数量、金额和绝对数量分别用函数rankNumber给出排序值
  rk <- rk[order(rk$qty, decreasing=TRUE), ]
  rownames(rk) <- 1:nrow(rk)
  rk$qty_rk <- rankNumber(rk$qty)
  rk$vol_rk <- rankNumber(rk$vol)
  rk$qty_abs_rk <- rankNumber(rk$qty_abs)

  # 对于数量指标进行四舍五入
  # if (round) {
  #   rk$qty <- round(rk$qty, 2)
  #   rk$qty_pct <- round(rk$qty_pct, 2)
  #   rk$vol <- round(rk$vol, 2)
  #   rk$vol_pct <- round(rk$vol_pct, 2)
  #   rk$qty_abs <- round(rk$qty_abs, 2)
  # }

  # 重新整理数据并返回结果
  rk <- rk[, c('underwriter', 'qty', 'qty_pct', 'qty_rk',
               'vol', 'vol_pct', 'vol_rk', 'qty_abs', 'qty_abs_rk')]
  # if (header_cn) {
  #   # colnames(rk) <- c('主承销商', '数量', '数量占比', '数量排名',
  #   #                   '规模', '规模占比', '规模排名', '绝对数量',
  #   #                   '绝对数量排名')
  #   colnames(rk) <- c("\u4e3b\u627f\u9500\u5546",
  #                     "\u6570\u91cf",
  #                     "\u6570\u91cf\u5360\u6bd4",
  #                     "\u6570\u91cf\u6392\u540d",
  #                     "\u89c4\u6a21",
  #                     "\u89c4\u6a21\u5360\u6bd4",
  #                     "\u89c4\u6a21\u6392\u540d",
  #                     "\u7edd\u5bf9\u6570\u91cf",
  #                     "\u7edd\u5bf9\u6570\u91cf\u6392\u540d")
  # }
  return(rk)
}
