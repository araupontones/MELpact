
#'@param db A tible of Indicator, expected, and approved (run tibble_expected_vs_approved)
#' @param exdir A directory where the table will be exported
#'@return An excel file with a table formatte in OPM style with 4 columns (indicator, Expected, Approved)


crear_xlsx_expected_vs_approved <- function(db,
                                            exfile
){

  wb<-openxlsx::createWorkbook()


  addWorksheet(wb, sheetName = "sheet", zoom = 100, orientation = 'landscape')

  print(exfile)
  my_data = db ## data

  header = names(my_data)[-1]

  ## set column widths
  setColWidths(wb,  sheet="sheet", cols=1, widths = 49) ## set column width for row names column
  setColWidths(wb,  sheet="sheet", cols=2:length(header), widths = 11.7) ## set column width for row names column

  #Header
  for(i in 1:length(header)) {
    writeData(wb,sheet = "sheet",startRow = 1, startCol = i, header[i]) #write data into the headers
  }


  addStyle(wb,sheet = "sheet",style_headers,rows =1,cols = 1:length(header), gridExpand = T)

  ## row group

  row = 2
  cols = length(header)

  groupsElements = unique(my_data$Element)

  for(i in 1:length(groupsElements)) {

    writeData(wb,sheet = "sheet",startRow = row, startCol = 1, groupsElements[i]) #row with the name of the indicator
    mergeCells(wb, "sheet", cols = 1:cols, rows = row)

    addStyle(wb,sheet = "sheet",style_groups,rows =row,cols = 1:cols, gridExpand = T)


    descriptions = my_data %>%
      filter(Element == groupsElements[i]) %>%
      select(- Element) %>%
      arrange(Indicator)

    ## description de cada indicador
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




