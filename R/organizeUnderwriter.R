#' Cut Underwriter's Name to Short Form
#'
#' Cut underwriter's name from the full form to short form, all in Chinese
#'
#' The data read from Wind Client has the full-name of underwriter, it is long
#' and hard to use or review. This function cut them to short form, usually four
#' Chinese character, maybe three or five for some special names. This function
#' can handle with the most underwriter's name. Generally, the securities
#' company has the short form "XX securities", and bank has the name "XX bank".
#' And then, some ones have the well-known short name.
#'
#' @param uw the full form of underwriters' names
#'
#' @return The character vector of shor name.
#' @export
#'

organizeUnderwriter <- function(uw) {
  # 本函数用于简化主承销商简称，适用于券商和银行
  # 老券的承销商比较复杂，有待调整和改善
  # 个别无数据的在后面“其他”当中再做处理添加
  # uw <- gsub('股份有限责任公司','',uw)
  # uw <- gsub('股份有限公司','',uw)
  # uw <- gsub('有限责任公司','',uw)
  # uw <- gsub('有限公司','',uw)
  # # 证券公司
  # uw <- gsub('中国国际金融','中金公司',uw)
  # uw <- gsub('第一创业摩根大通证券','一创摩根',uw)
  # uw <- gsub('长江证券承销保荐','长江保荐',uw)
  # uw <- gsub('长江巴黎百富勤证券','长江保荐',uw)
  # uw <- gsub('摩根士丹利华鑫证券','摩根华鑫',uw)
  # uw <- gsub('中信建投证券','中信建投',uw)
  # uw <- gsub('国泰君安证券','国泰君安',uw)
  # uw <- gsub('中银国际证券','中银国际',uw)
  # uw <- gsub('瑞信方正证券','瑞信方正',uw)
  # uw <- gsub('华泰联合证券','华泰联合',uw)
  # uw <- gsub('申银万国证券','申银万国',uw)
  # uw <- gsub('高盛高华证券','高盛高华',uw)
  # uw <- gsub('第一创业证券','第一创业',uw)
  # uw <- gsub('财富里昂证券','财富里昂',uw)
  # uw <- gsub('华欧国际证券','财富里昂',uw)
  # uw <- gsub('建银投资证券','中投证券',uw)
  # uw <- gsub('东方花旗证券','东方花旗',uw)
  # uw <- gsub('申万宏源证券承销保荐','申万宏源',uw)
  # uw <- gsub('申万宏源证券','申万宏源',uw)
  # uw <- gsub('上海华信证券','华信证券',uw)
  # uw <- gsub('西藏东方财富证券','东方财富',uw)
  # uw <- gsub('联合证券','华泰联合',uw)
  # uw <- gsub('江南证券','中航证券',uw)
  # uw <- gsub('海际大和证券','上海证券',uw)
  # # 银行
  # uw <- gsub('中国','',uw)
  # uw <- gsub('^银行$', '中国银行', uw)
  # uw <- gsub('^(银行)(,.*)', '中国银行\\2', uw)
  # uw <- gsub('(.*,)(银行)$', '\\1中国银行', uw)
  # uw <- gsub(',银行,', ',中国银行,', uw)
  # uw <- gsub('国家开发银行','国开行',uw)
  # uw <- gsub('进出口银行','进出口行',uw)
  # uw <- gsub('广东发展银行','广发银行',uw)
  # uw <- gsub('上海浦东发展银行','浦发银行',uw)
  # uw <- gsub('中信实业银行','中信银行',uw)
  # uw <- gsub('深圳发展银行','深发展',uw)
  # uw <- gsub('新华资产管理','新华资管',uw)
  # uw <- gsub('瑞穗实业银行\\(中国\\)','瑞穗银行',uw)
  # # 其他
  # uw <- gsub('经济开发信托投资公司','中经开',uw)
  # uw <- gsub('吉林化工集团财务公司','吉化财务',uw)
  # uw <- gsub('华中电力集团财务','华中电财务',uw)
  # uw <- gsub('科技国际信托投资','中科信',uw)
  # uw <- gsub('信达信托投资公司','信达信托',uw)
  # uw <- gsub('港澳国际信托投资','港澳信托',uw)
  # uw <- gsub('浙江省国际信托投资公司','浙江国投',uw)
  # # 补充-20160719
  # uw <- gsub('恒泰长财证券','恒泰长财',uw)
  # uw <- gsub('北京农村商业银行','北京农商行',uw)
  # uw <- gsub('上海农村商业银行','上海农商行',uw)

  # 以下为ASCII码的函数部分
  uw <- gsub("\u80a1\u4efd\u6709\u9650\u8d23\u4efb\u516c\u53f8","",uw)
  uw <- gsub("\u80a1\u4efd\u6709\u9650\u516c\u53f8","",uw)
  uw <- gsub("\u6709\u9650\u8d23\u4efb\u516c\u53f8","",uw)
  uw <- gsub("\u6709\u9650\u516c\u53f8","",uw)
  uw <- gsub("\u4e2d\u56fd\u56fd\u9645\u91d1\u878d","\u4e2d\u91d1\u516c\u53f8",uw)
  uw <- gsub("\u7b2c\u4e00\u521b\u4e1a\u6469\u6839\u5927\u901a\u8bc1\u5238","\u4e00\u521b\u6469\u6839",uw)
  uw <- gsub("\u957f\u6c5f\u8bc1\u5238\u627f\u9500\u4fdd\u8350","\u957f\u6c5f\u4fdd\u8350",uw)
  uw <- gsub("\u957f\u6c5f\u5df4\u9ece\u767e\u5bcc\u52e4\u8bc1\u5238","\u957f\u6c5f\u4fdd\u8350",uw)
  uw <- gsub("\u6469\u6839\u58eb\u4e39\u5229\u534e\u946b\u8bc1\u5238","\u6469\u6839\u534e\u946b",uw)
  uw <- gsub("\u4e2d\u4fe1\u5efa\u6295\u8bc1\u5238","\u4e2d\u4fe1\u5efa\u6295",uw)
  uw <- gsub("\u56fd\u6cf0\u541b\u5b89\u8bc1\u5238","\u56fd\u6cf0\u541b\u5b89",uw)
  uw <- gsub("\u4e2d\u94f6\u56fd\u9645\u8bc1\u5238","\u4e2d\u94f6\u56fd\u9645",uw)
  uw <- gsub("\u745e\u4fe1\u65b9\u6b63\u8bc1\u5238","\u745e\u4fe1\u65b9\u6b63",uw)
  uw <- gsub("\u534e\u6cf0\u8054\u5408\u8bc1\u5238","\u534e\u6cf0\u8054\u5408",uw)
  uw <- gsub("\u7533\u94f6\u4e07\u56fd\u8bc1\u5238","\u7533\u94f6\u4e07\u56fd",uw)
  uw <- gsub("\u9ad8\u76db\u9ad8\u534e\u8bc1\u5238","\u9ad8\u76db\u9ad8\u534e",uw)
  uw <- gsub("\u7b2c\u4e00\u521b\u4e1a\u8bc1\u5238","\u7b2c\u4e00\u521b\u4e1a",uw)
  uw <- gsub("\u8d22\u5bcc\u91cc\u6602\u8bc1\u5238","\u8d22\u5bcc\u91cc\u6602",uw)
  uw <- gsub("\u534e\u6b27\u56fd\u9645\u8bc1\u5238","\u8d22\u5bcc\u91cc\u6602",uw)
  uw <- gsub("\u5efa\u94f6\u6295\u8d44\u8bc1\u5238","\u4e2d\u6295\u8bc1\u5238",uw)
  uw <- gsub("\u4e1c\u65b9\u82b1\u65d7\u8bc1\u5238","\u4e1c\u65b9\u82b1\u65d7",uw)
  uw <- gsub("\u7533\u4e07\u5b8f\u6e90\u8bc1\u5238\u627f\u9500\u4fdd\u8350","\u7533\u4e07\u5b8f\u6e90",uw)
  uw <- gsub("\u7533\u4e07\u5b8f\u6e90\u8bc1\u5238","\u7533\u4e07\u5b8f\u6e90",uw)
  uw <- gsub("\u4e0a\u6d77\u534e\u4fe1\u8bc1\u5238","\u534e\u4fe1\u8bc1\u5238",uw)
  uw <- gsub("\u897f\u85cf\u4e1c\u65b9\u8d22\u5bcc\u8bc1\u5238","\u4e1c\u65b9\u8d22\u5bcc",uw)
  uw <- gsub("\u8054\u5408\u8bc1\u5238","\u534e\u6cf0\u8054\u5408",uw)
  uw <- gsub("\u6c5f\u5357\u8bc1\u5238","\u4e2d\u822a\u8bc1\u5238",uw)
  uw <- gsub("\u6d77\u9645\u5927\u548c\u8bc1\u5238","\u4e0a\u6d77\u8bc1\u5238",uw)
  uw <- gsub("\u4e2d\u56fd","",uw)
  uw <- gsub("^\u94f6\u884c$", "\u4e2d\u56fd\u94f6\u884c", uw)
  uw <- gsub("^(\u94f6\u884c)(,.*)", "\u4e2d\u56fd\u94f6\u884c\\2", uw)
  uw <- gsub("(.*,)(\u94f6\u884c)$", "\\1\u4e2d\u56fd\u94f6\u884c", uw)
  uw <- gsub(",\u94f6\u884c,", ",\u4e2d\u56fd\u94f6\u884c,", uw)
  uw <- gsub("\u56fd\u5bb6\u5f00\u53d1\u94f6\u884c","\u56fd\u5f00\u884c",uw)
  uw <- gsub("\u8fdb\u51fa\u53e3\u94f6\u884c","\u8fdb\u51fa\u53e3\u884c",uw)
  uw <- gsub("\u5e7f\u4e1c\u53d1\u5c55\u94f6\u884c","\u5e7f\u53d1\u94f6\u884c",uw)
  uw <- gsub("\u4e0a\u6d77\u6d66\u4e1c\u53d1\u5c55\u94f6\u884c","\u6d66\u53d1\u94f6\u884c",uw)
  uw <- gsub("\u4e2d\u4fe1\u5b9e\u4e1a\u94f6\u884c","\u4e2d\u4fe1\u94f6\u884c",uw)
  uw <- gsub("\u6df1\u5733\u53d1\u5c55\u94f6\u884c","\u6df1\u53d1\u5c55",uw)
  uw <- gsub("\u65b0\u534e\u8d44\u4ea7\u7ba1\u7406","\u65b0\u534e\u8d44\u7ba1",uw)
  uw <- gsub("\u745e\u7a57\u5b9e\u4e1a\u94f6\u884c\\(\u4e2d\u56fd\\)","\u745e\u7a57\u94f6\u884c",uw)
  uw <- gsub("\u7ecf\u6d4e\u5f00\u53d1\u4fe1\u6258\u6295\u8d44\u516c\u53f8","\u4e2d\u7ecf\u5f00",uw)
  uw <- gsub("\u5409\u6797\u5316\u5de5\u96c6\u56e2\u8d22\u52a1\u516c\u53f8","\u5409\u5316\u8d22\u52a1",uw)
  uw <- gsub("\u534e\u4e2d\u7535\u529b\u96c6\u56e2\u8d22\u52a1","\u534e\u4e2d\u7535\u8d22\u52a1",uw)
  uw <- gsub("\u79d1\u6280\u56fd\u9645\u4fe1\u6258\u6295\u8d44","\u4e2d\u79d1\u4fe1",uw)
  uw <- gsub("\u4fe1\u8fbe\u4fe1\u6258\u6295\u8d44\u516c\u53f8","\u4fe1\u8fbe\u4fe1\u6258",uw)
  uw <- gsub("\u6e2f\u6fb3\u56fd\u9645\u4fe1\u6258\u6295\u8d44","\u6e2f\u6fb3\u4fe1\u6258",uw)
  uw <- gsub("\u6d59\u6c5f\u7701\u56fd\u9645\u4fe1\u6258\u6295\u8d44\u516c\u53f8","\u6d59\u6c5f\u56fd\u6295",uw)
  uw <- gsub("\\(\\)", "", uw)

  uw <- gsub("\u6052\u6cf0\u957f\u8d22\u8bc1\u5238", "\u6052\u6cf0\u957f\u8d22", uw)
  uw <- gsub("\u5317\u4eac\u519c\u6751\u5546\u4e1a\u94f6\u884c", "\u5317\u4eac\u519c\u5546\u884c", uw)
  uw <- gsub("\u4e0a\u6d77\u519c\u6751\u5546\u4e1a\u94f6\u884c", "\u4e0a\u6d77\u519c\u5546\u884c", uw)

  # uw[uw==''] <- '无'
  uw[uw==''] <- '\u65e0'
  return(uw)
}
