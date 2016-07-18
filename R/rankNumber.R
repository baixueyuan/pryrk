#' Give the Rank Number
#'
#' Give the rank number to input numeric vector, the same rank number to the
#' same value.
#'
#' For the same value in an ordered vector, the rank number should be the same,
#' the next value will be given its right ranking number, like, 1, 2, 3, 3, 5...
#'
#' @param vector numeric, the vector will be sorted and given the rank number
#' @param decreasing logical, default is TRUE, used in function \code{sort}
#'
#' @return The rank vector of input.
#' @export

rankNumber <- function(vector, decreasing=TRUE) {
  # 本函数的作用是根据输入的数值向量返回其排序序号
  # 其中对于并列的数值给出并列序号

  if (!is.numeric(vector)) stop('The input shuold be "numeric".')
  # 构造顺序排列的序列vec，将该序列中的数值与前一个数相减得到diff序列，
  # 数值对应的diff如果为0，则说明该数值与前数并列，因此排名序列int对应值
  # 也应当与前一值相当，例如并列第一等等
  vec <- sort(vector, decreasing=decreasing)
  int <- 1:length(vec)
  n <- length(vec)
  diff <- vec - c(vec[1], vec[-n])
  for (i in 2:n) {
    if (diff[i]==0) {
      int[i] <- int[i-1]
    }
  }
  # 用函数unique可以得到所有数值及其对应的排序值，进而得到输入数据vector对应的排
  # 序序列
  rk <- unique(data.frame(vec, int))
  rank <- integer(length=n)
  for (i in 1:n) {
    rank[i] <- rk$int[rk$vec==vector[i]]
  }
  return(rank)
}
