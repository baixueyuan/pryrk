#' Save the Ranking Result to Excel File
#'
#' Save the ranking result returned by \code{uwRanking} to the Excel file. This
#' function just save the single ranking dara.frame to single worksheet. The
#' cell style will be set
#'
#' @param data the ranking data returned by \code{uwRanking}
#' @param file the output file name, default "primaryRanking.xlsx"
#' @param sheet the sheet name, default "Rank"
#' @param open logical, if TRUE, the output file will be opened, default is
#'   FALSE
#'
#' @return The input data will be saved to an Excel file.
#' @export

saveToExcel <- function(data, file, sheet='Rank', open=FALSE) {
  # 本函数将uwRanking函数得到的排名结果写入Excel文件
  # 本函数通用性较差，定制性较强
  # 参数file如不给定，则默认存为工作目录下的“primaryRanking.xlsx”

  # 设置中文标题行
  colnames(data) <- c("\u4e3b\u627f\u9500\u5546",
                      "\u6570\u91cf",
                      "\u6570\u91cf\u5360\u6bd4",
                      "\u6570\u91cf\u6392\u540d",
                      "\u89c4\u6a21",
                      "\u89c4\u6a21\u5360\u6bd4",
                      "\u89c4\u6a21\u6392\u540d",
                      "\u7edd\u5bf9\u6570\u91cf",
                      "\u7edd\u5bf9\u6570\u91cf\u6392\u540d")

  # 对Excel的操作通过xlsx包进行
  wb <- createWorkbook() # 创建 wb 对象
  sheet <- createSheet(wb, sheetName=sheet) # 创建 sheet 对象
  # 设定不同单元格格式
  header_sty <- CellStyle(wb) + Alignment(h="ALIGN_CENTER") +
    Font(wb, name='Arial', isBold=TRUE, heightInPoints=10)
  char_sty <- CellStyle(wb) + Alignment(h="ALIGN_CENTER") +
    Font(wb, name='Arial', heightInPoints=10)
  num_sty <- CellStyle(wb) + DataFormat('0.00') + Alignment(h="ALIGN_RIGHT") +
    Font(wb, name='Arial', heightInPoints=10)
  pct_sty <- CellStyle(wb) + DataFormat('0.00%') + Alignment(h="ALIGN_RIGHT") +
    Font(wb, name='Arial', heightInPoints=10)
  int_sty <- CellStyle(wb) + DataFormat('0') + Alignment(h="ALIGN_RIGHT") +
    Font(wb, name='Arial', heightInPoints=10)
  # 添加数据
  addDataFrame(data, sheet, row.names=FALSE,
               colStyle=list('1'=char_sty, '2'=num_sty, '3'=pct_sty,
                             '4'=int_sty, '5'=num_sty, '6'=pct_sty,
                             '7'=int_sty, '8'=int_sty, '9'=int_sty))
  # 获取标题行及其单元格，并设定格式
  header <- getRows(sheet, rowIndex=1)
  header_cells <- getCells(header)
  for (i in header_cells) setCellStyle(cell=i, cellStyle=header_sty)

  setColumnWidth(sheet, colIndex=c(1:9), colWidth=11)
  createFreezePane(sheet, 2, 1) # 首行冻结

  # 存为Excel文档
  if (missing(file)) file <- 'primaryRanking.xlsx'
  saveWorkbook(wb, file=file)
  if (open) shell.exec(file)
}
