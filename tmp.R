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
  "uw <- gsub('邮政储蓄银行', '邮储银行', uw)",
  "uw <- gsub('农业发展银行', '农发行', uw)",
  "uw <- gsub('农村商业银行', '农商行', uw)",
  "uw <- gsub('广东顺德', '顺德', uw)",
  "uw <- gsub('资产管理$', '资产', uw)",
  "uw <- gsub('南方电网财务', '南网财务', uw)",
  "uw <- gsub('（）', '', uw)"
)
cat(stringi::stri_escape_unicode(str), sep='\n')
# 结果中的引号被转义，最好复制到Notepad++中稍作修改


naf <- read.table('clipboard')[[1]]
naf <- organizeUnderwriter(naf)
naf <- read.csv('metadata/nafmii.csv', fileEncoding='UTF-8', stringsAsFactors=F)
naf$institution <- stringi::stri_escape_unicode(naf$institution)
naf$short <- stringi::stri_escape_unicode(naf$short)
naf$kind <- stringi::stri_escape_unicode(naf$kind)
# save(naf, file='data/nafmii.rda', compress=TRUE)
save(naf, file='R/sysdata.rda', compress=TRUE)

testdata("银行")
