# 编译文挡，从注释到Rd文件
roxygen2::roxygenize()

# 测试脚本
rm(list=ls())
data <- getDataUsingWind('bond1.xlsx')
uw <- uwSplit(data)
rk <- uwRanking(uw)
prk <- printRank(rk)
saveToExcel(rk, open=TRUE)
View(prk)

# 将中文转为ASCII码
str <- c(
  "uw <- gsub('西藏同信证券','西藏同信',uw)",
  "uw <- gsub('北京农村商业银行','北京农商行',uw)",
  "uw <- gsub('上海农村商业银行','上海农商行',uw)"
)
cat(stringi::stri_escape_unicode(str), sep='\n')
# 结果中的引号被转义，最好复制到Notepad++中稍作修改


