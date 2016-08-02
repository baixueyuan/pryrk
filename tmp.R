# 编译文挡，从注释到rds文件
roxygen2::roxygenize()

# 测试脚本
rm(list=ls())
data <- getDataUsingWind('bond.xlsx')
dat <- filterType(data, '公司债')
dat <- filterDate(dat, '2016-01-01', '2016-06-30')
uw1 <- uwSplit(data, FALSE)
uw <- uwSplit(data)
rk <- uwRanking(uw)
prk <- printRank(rk)
saveToExcel(rk, open=TRUE)
view(prk)

# 使用综合函数rank进行查询
rank()
rank(type='企业债', start='2015-01-01', end='2015-06-30', save=T)

# 测试
rm(list=ls())
system.time(rank(print.head=FALSE))
rm(list=ls())
system.time(rank(print.head=FALSE, use.tidyr=FALSE))

# 将中文转为ASCII码
str <- c(
  "uw <- gsub('民生银行成都分行', '民生银行', uw)",
  "uw <- gsub('中金国际金融', '中金公司', uw)",
  "uw <- gsub('瑞穗实业银行', '瑞穗实业', uw)",
  "uw <- gsub('人保资本投资管理', '人保资本', uw)",
  "uw <- gsub('法国巴黎银行', '巴黎银行', uw)",
  "uw <- gsub('三菱东京日联银行', '三菱银行', uw)"
)
cat(stringi::stri_escape_unicode('Wind债券类型(二级)'), sep='\n')
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

for (i in c('name', 'type', 'underwriter')) {
  uw[i]  <- stringi::stri_unescape_unicode(uw[[i]])
}
