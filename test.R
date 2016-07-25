pct <- as.Date(as.POSIXct(uw$initdate, tz='UTC', origin='1970-01-01 00:00.00 UTC'))
date <- as.Date(pct)

# 处理含有中文的代码文件，如果有注释的行不处理，没有注释符号的代码行做转义处理
src <- readLines('R/filter.R', encoding='UTF-8')
ch <- grep('[\u4e00-\u9fa5]', src)
mk <- grep('#', src)
ind <- setdiff(ch, mk)
for (i in ind) {
  src[i] <- stringi::stri_escape_unicode(src[i])
}
cat(src, sep='\n')
write(src, file='tt.R')

data <- head(uwrk, 10)
caption <- '承销排名示例'
kable_im(head(uwrk, 10), caption = '债券承销示例')
