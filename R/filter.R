#' Filter the Data with the Bond Type and Date
#'
#' Filter the data with the attribute of bond's type and important dates, this
#' will help to
#' customize the further work. It is the best practice to use this filter
#' function to the data returned by \code{getDataUsingWind}. But if the data
#' returned by \code{uwSplit} exists and such data is big, then use filter to it
#' is better to avoid the long-time wait by using \code{uwSplit}.
#'
#' Pay attention to that when filtering the type, only three type options can
#' be given and get the right result. When filtering date, start and end
#' dates are all given and obtain the result, otherwise two params are default
#' set to \code{NULL} which will just pass throught the input \code{data}.
#'
#' @param data data.frame, it's better to be returned by \code{getDataUsingWind}
#'   however if the data has the column *type* it will work
#' @param type character, used to choose which type of credit bond to be
#'   filtered, if the type is incorrect, the function will stop and give message
#' @param start the start date, can be string like "2016-01-01"
#' @param end the end date, can be string like "2016-01-01"
#' @param date_type the date type used to filtered data, "initdate" or
#'   "carrydate"
#'
#' @return The filtered data.frame for the further work.
#' @name filter
NULL

#' @rdname filter
#' @export
filterType <- function(data, type) {

  # 参数type仅限于“公司债”、“企业债”和“中票短融”
  types <- c("\u516c\u53f8\u503a", "\u4f01\u4e1a\u503a", "\u4e2d\u7968\u77ed\u878d")
  if(is.na(match(type, types))) {
    stop("The param type can only be one of \"\u516c\u53f8\u503a\", \"\u4f01\u4e1a\u503a\", \"\u4e2d\u7968\u77ed\u878d\".")
  }

  # 如果type是“公司债”，则搜索“一般公司债”和“私募债”，即包含证监会的公司债品种
  if (type=="\u516c\u53f8\u503a") {
    res <- data[data$type=="\u4e00\u822c\u516c\u53f8\u503a" | data$type=="\u79c1\u52df\u503a", ]
  }

  # 如果type是“企业债”，则搜索“一般企业债”和“集合企业债”
  if (type=="\u4f01\u4e1a\u503a") {
    res <- data[data$type=="\u4e00\u822c\u4f01\u4e1a\u503a" | data$type=="\u96c6\u5408\u4f01\u4e1a\u503a", ]
  }

  # 如果type是“中票短融”，则搜索“一般中期票据”、“一般短期融资券”、
  # “超短期融资债券”、“定向工具”、“集合票据”
  if (type=="\u4e2d\u7968\u77ed\u878d") {
    res <- data[data$type=="\u4e00\u822c\u4e2d\u671f\u7968\u636e" |
                  data$type=="\u4e00\u822c\u77ed\u671f\u878d\u8d44\u5238" |
                  data$type=="\u8d85\u77ed\u671f\u878d\u8d44\u503a\u5238" |
                  data$type=="\u5b9a\u5411\u5de5\u5177" |
                  data$type=="\u96c6\u5408\u7968\u636e", ]
  }

  return(res)
}

#' @rdname filter
#' @export
filterDate <- function(data, start=NULL, end=NULL, date_type='initdate') {

  if (is.null(start) || is.null(end)) {
    # 日期默认为NULL，如果不给定日期则直接返回给出的data
    return(data)
  } else {
    # 起始日期应当早于结束日期
    start <- as.Date(start)
    end <- as.Date(end)
    if (start >= end) stop('The "start" date should be earlier than "end" date.')

    # 日期类型尽可以为发行起始日或起息日
    date_types <- c('initdate', 'carrydate')
    if(is.na(match(date_type, date_types))) {
      stop('The param date_type can only be one of "initdate" or "carrydate"')
    }

    res <- data[data[[date_type]] >= start & data[[date_type]] <= end, ]
  }
  return(res)
}
