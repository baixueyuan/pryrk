#' The Improvement of \code{knitr::kable} Function
#'
#' The improved and customized version of the `kable` function which is used to
#' generated Latex table. Generally speaking, it add more customization options.
#'
#' The function uses \code{kable} to generate table in the **table** environment,
#' but lack of customization of the table style. This funcion adds the following
#' options:
#'
#' - The font size can be directly input to the final Latex code.
#'
#' - Set or give the alignment option of the table columns. Like "l|c|c|r".
#'
#' - Split the header to tow lines, which will solve the wide table to be
#' located inside the text area.
#'
#' @param data data.frame, the table wait to be converted to Latex code
#' @param caption character, the caption of the table
#' @param split logical, whether to split the header to tow lines
#' @param ignore numeric, which column(s) to be ignored when splitting
#' @param align character, how to align the columns, "c", "r" or "custom"
#' @param align.str if \code{align} is set to "custom", the customized the
#'   alignment should be given here
#' @param font.size the Latex code of font size can be set directly here, e.g.
#'   "\\small"
#'
#' @return The LaTex code of the table will be output using \code{cat}, thus it
#'   can be copied to Latex file or used in **knitr**, where set
#'   \code{result='asis'}, the same way as using \code{kable}.
#' @importFrom knitr kable
#' @export

kable_im <- function(data, caption, split=TRUE, ignore=1,
                     align='c', align.str='', font.size='') {
  # 本函数将kable函数生成的表格进行修改
  # 最大的就该就是将标题行由一行拆为两行
  # 另一个重要功能是将浮动的参数“t”改为“ht”，并放在“\begin{table}”后面
  # 参数data是要生成表格的数据框
  # 参数caption是表格的标题
  # 参数split表示是否拆分标题行
  # 参数ignore是指不进行标题行分割的列，为一个数值向量
  # 参数align设置对齐选项，除了最左侧栏为左对齐以外，其他可以选择居中还是靠右
  # 也可以设置为“custom”，则将设置为align.str
  # 参数font.size是字体大小，直接输入Latex代码，如“\\small”，默认为“''”
  header <- colnames(data)
  lstr <- knitr::kable(data, caption=caption, format='latex')
  lstr <- strsplit(lstr, '\n')[[1]]
  lstr[2] <- lstr[3]
  lstr[3] <- font.size
  lstr <- gsub('^(\\\\begin\\{table\\})$', '\\1[ht]', lstr)
  lstr <- gsub('(begin\\{tabular\\})\\[t\\]', '\\1', lstr)
  if (align=='c') {
    lstr[5] <- gsub('\\|[lr]', '|c', lstr[5])
  }
  if (align=='r') {
    lstr[5] <- gsub('\\|[l]', '|r', lstr[5])
  }
  if (align=='custom') {
    lstr[5] <- paste('\\begin{tabular}{', align.str, '}', sep='')
  }
  # 如果split为TRUE，则对标题行拆分成两行
  if (split) {
    lh <- sapply(header, function(x) {
      len <- nchar(x)
      if (len <= 2) {
        return(list(x, ''))
      } else {
        e1 <- ceiling(len/2)
        s2 <- e1 + 1
        res <- list(substr(x, 1,e1), substr(x, s2,len))
        return(res)
      }
    })

    lh1 <- as.character(lh[1,])
    lh2 <- as.character(lh[2,])
    for (i in ignore) {
      lh1[i] <- paste(lh1[i], lh2[i], sep='')
      lh2[i] <- ''
    }
    for (i in which(lh2=='')) {
      lh1[i] <- paste('\\multirow{2}{*}{', lh1[i], '}', sep='')
    }
    h1 <- paste(lh1, collapse = ' & ')
    h1 <- paste(h1, '\\\\', sep='')
    h2 <- paste(lh2, collapse = ' & ')
    h2 <- paste(h2, '\\\\', sep='')
    res <- c(lstr[1:6], h1, h2, lstr[8:length(lstr)])
  } else {
    res <- lstr
  }

  cat(res, sep='\n')
}
