filterType <- function(data, type) {
  if (type=="\u516c\u53f8\u503a") {
    res <- data[type=="\u4e00\u822c\u516c\u53f8\u503a" | type=="\u79c1\u52df\u503a", ]
  }

  return(res)
}
