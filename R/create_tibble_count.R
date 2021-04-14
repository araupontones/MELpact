#Function to create tibble that counts approved indicators
#' @param db data base to be summarised
#' @param group_by Variable to which the summarise will be conducted (i.e. count by country or by component)
#' @param  by_quarter Whether the data should be filter by the current country
#' @param by_country Whether data at the country level is to be created or not
#' @param filter_country Name of the country that the data is to be filter against
#


tibble_cuenta_achieved <- function(db,
                                   group_by,
                                   by_quarter = F,
                                   by_country = F,
                                   filter_country =" ", ...){

  #' allow user to filter by COUNTRY of interest
  if(by_country==F){

    filter_country = unique(db[["Country"]])
  } else {

    #'This has to be a country that exists in the database
    filter_country = filter_country
  }

  #' allow user to filter by QUARTER of interest
  if(by_quarter==F){

    filter_quarter = unique(db[["Quarter"]])
  } else {

    #'quarter filter is defined in the set_up script
    filter_quarter = quarter_filter
  }


  group_vars <- c(group_by, "Element", "Indicator")
  #Create chart
  data_chart = db %>%
    #These are defined by the user
    filter(Country %in% filter_country)%>%
    filter(Quarter %in% filter_quarter) %>%
    group_by(across({{group_vars}})) %>%

    #do the counting
    summarise(Total = sum(Approved, na.rm = T), .groups = "drop") %>%
    pivot_wider(id_cols = c(group_by, "Element", "Indicator"),
                names_from = all_of(group_by),
                values_from = Total
    ) %>%
    rowwise() %>%
    mutate(Total = suma(c_across(all_of(names(.)[-c(1,2)]))))

  return(data_chart)

}

