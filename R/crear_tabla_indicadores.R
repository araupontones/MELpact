#' Creates and export excel with count by achieved indicator
#'
#' @param  db A tibble with the indicators to be presented in the table.  This is the output
#' of \code{create_tibble_count()}
#' @param exit_dir A directory where excel file is exported
#' @param exit_dir A string with the name of the excel.xlsx file to be crated
#'
#' @return an excel sheet stored in exit_dir/file_xlsx


crear_tabla_indicadores = function(db = data_c, file_xlsx, exit_dir){



  wb <-openxlsx::createWorkbook()


  addWorksheet(wb, sheetName = "sheet", zoom = 100, orientation = 'landscape')

  exfile = file.path(exit_dir, file_xlsx)
  print(exfile)

  #header = names(get(data_c))[-1]
  header = names(db)[-1]

  print(header)

  ## set column widths
  setColWidths(wb,  sheet="sheet", cols=1, widths = 53.7) ## set column width for row names column
  setColWidths(wb,  sheet="sheet", cols=2:length(header), widths = 11.7) ## set column width for row names column

  #Header
  for(i in 1:length(header)) {
    writeData(wb,sheet = "sheet",startRow = 1, startCol = i, header[i]) #write data into the headers
  }


  addStyle(wb,sheet = "sheet",style_headers,rows =1,cols = 1:length(header), gridExpand = T)

  ## row group

  #rowGroup

  row = 2
  cols = length(header)

  #groupsElements = unique(get(data_c)$Element)
  groupsElements = unique(db$Element)

  for(i in 1:length(groupsElements)) {

    writeData(wb,sheet = "sheet",startRow = row, startCol = 1, groupsElements[i]) #row with the name of the indicator
    mergeCells(wb, "sheet", cols = 1:cols, rows = row)

    addStyle(wb,sheet = "sheet",style_groups,rows =row,cols = 1:cols, gridExpand = T)


    descriptions = db %>%
      filter(Element == groupsElements[i]) %>%
      select(- Element) %>%
      arrange(Indicator)

    row = row + 1
    until = row + nrow(descriptions) - 1

    writeData(wb,sheet = "sheet",startRow = row, startCol = 1, descriptions, colNames = F) #row with the name of the indicator
    addStyle(wb,sheet = "sheet",style_results,rows =row:until,cols = 1, gridExpand = T)
    addStyle(wb,sheet = "sheet",style_results_indicators,rows =row:until,cols = 2:cols, gridExpand = T)



    row = row + nrow(descriptions)

    #indicators

  }

  openxlsx::saveWorkbook(wb, file = exfile, overwrite = T)
}
