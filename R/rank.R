#' Quick Rank Function
#'
#' Quickly obtain the ranking result with many params have been given usually
#' used values. Of course, it depends on the original Excel data file.
#'
#' This function comprehensively use other functions in the package and quickly
#' gives the underwriter result. Though, there are many params, most of which
#' are pass to the functions used, and in fact, they have the default value.
#' That is to say that if you just type \code{rank()}, it will read and
#' compute the ranking result. At this time, the result is fully based on the
#' input data file, however you are sure to allow add filter params later.
#'
#' The function will show the top 20 by default, and surely can save the result
#' to Excel file, at the same time **assign** the result to the *envir* given,
#' which can be found as named \code{uwrk} ("Underwriters Ranking").
#'
#' @param envir the name of the environment save and get the data, default is
#'   the Global Environment
#' @param file the file name of Excel data file
#' @param update logical, default is FALSE, if TRUE the function will be forced
#'   to read the Excel file to get the original bond data, otherwise, it will
#'   check if there exists the data and use it without reading again
#' @param save logical, default FALSE, when TRUE the rank result will be saved
#'   to Excel file
#' @param output the file name of Excel, used when param **save** is TRUE
#' @param print.head logical, default TRUE, when the function is called, the
#'   top 20 will be printed as referenced
#' @inheritParams filter
#' @inheritParams saveToExcel
#'
#' @return The ranking result, can be saved into an Excel file.
#' @export

rank <- function(envir=.GlobalEnv, file, update=FALSE, save=FALSE, type,
                 start, end, date_type, output, sheet='Rank', open=FALSE,
                 print.head=TRUE) {

  # 如果update为TRUE则强制更新债券发行信息列表bond_list
  # 否则检查环境中是否存在bond_list，存在则直接使用，不存在则读取数据
  # 以避免重复读取数据
  if (update) {
    if (missing(file)) {
      bond_list <- getDataUsingWind()
    } else {
      bond_list <- getDataUsingWind(file)
    }
    assign('bond_list', bond_list, envir=envir)
  } else {
    if (exists('bond_list', envir=envir)) {
      bond_list <- get('bond_list', envir=envir)
    } else {
      if (missing(file)) {
        bond_list <- getDataUsingWind()
      } else {
        bond_list <- getDataUsingWind(file)
      }
      assign('bond_list', bond_list, envir=envir)
    }
  }

  # 随后步骤类似数据读取，先检查update是否为TRUE，再检查是否存在可用数据
  if (update) {
    uw <- uwSplit(bond_list)
  } else {
    if (exists('uw', envir=envir)) {
      uw <- get('uw', envir=envir)
    } else {
      uw <- uwSplit(bond_list)
    }
  }

  # 当筛选参数存在时用相应函数对数据进行过滤
  if (!missing(type)) uw <- filterType(uw, type)
  if (!missing(start) && !missing(end)) {
    if(missing(date_type)) date_type <- 'initdate'
    uw <- filterDate(uw, start, end, date_type)
  }
  if (nrow(uw)==0) stop('There is no data after filtering, please Check...')
  assign('uw', uw, envir=envir)

  rk<- uwRanking(uw)
  res <- printRank(rk)

  # 将结果赋值到指定环境中可直接使用，如果选择保存可以存为Excel文档
  assign('uwrk', res, envir=envir)
  if (save) {
    saveToExcel(rk, output, sheet, open)
  }

  if (print.head) print(head(res, 20))
}
