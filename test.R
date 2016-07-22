pct <- as.Date(as.POSIXct(uw$initdate, tz='UTC', origin='1970-01-01 00:00.00 UTC'))
date <- as.Date(pct)


header_sty <- xlsx::CellStyle(wb) + xlsx::Alignment(h="ALIGN_CENTER") +
  xlsx::Font(wb, name='Arial', isBold=TRUE, heightInPoints=10)
