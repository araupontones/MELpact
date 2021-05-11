
#'@param exfile A path to save the file (.xlsx)
#'@param dataSrc A tibble filtered by project
#'@param seqIndicator A tibble with sequence of indicators of the project

crear_xlsx_annex <- function(exfile,
                             dataSrc = dataSrc,
                             seqIndicator = seqIndicator
){

  wb<- openxlsx::createWorkbook()
  addWorksheet(wb, sheetName = "sheet", zoom=100, orientation = "landscape") #create sheet


  #Header
  writeData(wb,sheet = "sheet",startRow = 1, startCol = 1, "Project:") #write data into the headers
  writeData(wb,sheet = "sheet",startRow = 1, startCol = 2, dataSrc$Project[1])
  mergeCells(wb, "sheet", cols = 2:5, rows = 1)


  writeData(wb,sheet = "sheet",startRow = 2, startCol = 1, "Implementor:")
  writeData(wb,sheet = "sheet",startRow = 2, startCol = 2, dataSrc$Implementor[1])
  mergeCells(wb, "sheet", cols = 2:5, rows = 2)

  writeData(wb,sheet = "sheet",startRow = 3, startCol = 1, "Start date:")
  writeData(wb,sheet = "sheet",startRow = 3, startCol = 2, dataSrc$Start_Date[1])
  mergeCells(wb, "sheet", cols = 2:5, rows = 3)

  writeData(wb,sheet = "sheet",startRow = 4, startCol = 1, "End date:")
  writeData(wb,sheet = "sheet",startRow = 4, startCol = 2, dataSrc$End_Date[1])
  mergeCells(wb, "sheet", cols = 2:5, rows = 4)


  #style the header
  addStyle(wb,sheet = "sheet",style_headers,rows =1:4,cols = 1:5, gridExpand = T)


  setColWidths(wb,  sheet="sheet", cols=1, widths = 14.22) ## set column width for row names column
  setColWidths(wb,  sheet="sheet", cols=2, widths = 34.33)
  setColWidths(wb,  sheet="sheet", cols=3, widths = 49.44)
  setColWidths(wb,  sheet="sheet", cols=c(4:5), widths = 10)



  #Titles
  writeData(wb,sheet = "sheet",startRow = 5, startCol = 1, "Expected Milestones") #add titles to the merged columns
  mergeCells(wb, "sheet", cols = 1:2, rows = 5)

  writeData(wb,sheet = "sheet",startRow = 5, startCol = 3, "Reported Results")

  writeData(wb,sheet = "sheet",startRow = 5, startCol = 4, "Expected")
  writeData(wb,sheet = "sheet",startRow = 5, startCol = 5, "Achieved")

  addStyle(wb,sheet = "sheet",style_titles,rows =5,cols = 1:5, gridExpand = T)


  #groupRows by inidcator

  row = 6
  for(i in 1:nrow(seqIndicator)) {

    ##header of indicator
    writeData(wb,sheet = "sheet",startRow = row, startCol = 1, seqIndicator$Indicator[i]) #row with the name of the indicator
    mergeCells(wb, "sheet", cols = 1:5, rows = row)
    addStyle(wb,sheet = "sheet",style_groups,rows =row,cols = 1:5, gridExpand = T)
    row = row + 1


    #Description for each indicator
    descriptions = dataSrc %>%
      ungroup() %>%
      filter(Indicator == seqIndicator$Indicator[i]) %>%
      select(Description, Description_new, Quarter_expected,Quarter_reported) %>%
      arrange(Quarter_expected)


    for(r in 1:nrow(descriptions)){

      writeData(wb,sheet = "sheet",startRow = row, startCol = 1, descriptions$Description[r])
      mergeCells(wb, "sheet", cols = 1:2, rows = row)
      addStyle(wb,sheet = "sheet",style_milestones,rows =row,cols = 1:2, gridExpand = T)

      writeData(wb,sheet = "sheet",startRow = row, startCol = 3, descriptions$Description[r])

      writeData(wb,sheet = "sheet",startRow = row, startCol = 4, descriptions$Quarter_expected[r])
      writeData(wb,sheet = "sheet",startRow = row, startCol = 5, descriptions$Quarter_reported[r])

      addStyle(wb,sheet = "sheet",style_results,rows =row,cols = 3:5, gridExpand = T)


      row = row + 1

    }


  }

  #export

  openxlsx::saveWorkbook(wb, file = exfile, overwrite = T)
  print(exfile)
}
