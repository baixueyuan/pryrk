#' Process the Data for Rank
#'
#' Process the original primary bond market data for the rank, especially split
#' the co-underwriting bond record into several records according to the number
#' of the underwriter.
#'
#' The data returned by \code{getDataUsingWind} is used here. This is the step
#' before ranking. It is easy to handle with the single underwriter record,
#' however the co-underwriters are "pasted" into on string. This function split
#' co-underwriters as they are seperated by ",".
#'
#' @param data the original primary bond market data, which is read by the
#'   function \code{getDataUsingWind}, and it must contain column "underwriter."
#'
#' @return A data.frame like the input one, however, every record has only one
#'   underwriter. And this data.frame will be used for ranking.
#' @export
#'

uwSplit <- function(data) {
  # 本函数返回一个可以将data.frame可用于进一步统计使用
  # 该data.frame在基础数据基础上，将主承销商分开，每只债券按主承销商分开，各单列
  # 一行，同时根据主承销商数量给出家数均值和规模均值

  # 以“,”分开主承销商，并计算每只债券的主承销商数量
  uw_list <- strsplit(data$underwriter, split=',')
  nrow <- nrow(data)
  data$uw_no <- sapply(uw_list, length)
  data$rk_qty <- 1 / data$uw_no
  data$rk_vol <- data$amount / data$uw_no
  uw_list <- unlist(uw_list)
  data_1 <- data
  data_1$underwriter <- NULL

  # 构造一个新的数据框，长度为所有主承销商数量之和
  nn <- length(uw_list)
  mm <- length(data_1)
  data_2 <- data.frame(matrix(NA, nrow=nn, ncol=mm))
  # 生成一个序列cum，是各只债券主承销商数量的累计求和序列
  cum <- cumsum(data$uw_no)
  # 构造一个循环，将原始的data数据按行写入到新的数据框中，写入行数与主承销商数量
  # 相同，如果有5个主承销商则会重复5行，这是通过cum序列和行下标实现的，参考a1和
  # a2的设定
  for (i in 1:nrow(data_1)) {
    if (i==1) a1 <- 1 else a1 <- cum[i-1] + 1
    a2 <- cum[i]
    data_2[a1:a2,] <- data_1[i,]
  }
  # 给结果数据框设置列名称并与主承销商序列绑定
  colnames(data_2) <- colnames(data_1)
  data_2$underwriter <- uw_list
  data_2$initdate <- as.Date(
    as.POSIXct(data_2$initdate, tz='UTC', origin='1970-01-01 00:00.00 UTC')
    )
  data_2$carrydate <- as.Date(
    as.POSIXct(data_2$carrydate, tz='UTC', origin='1970-01-01 00:00.00 UTC')
  )
  return(data_2)
}
