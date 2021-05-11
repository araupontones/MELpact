
#Expected vs approved (three columns: indicator, expected, approved group by dimension)

#' @param  db a tibble of reporte cuenta
#' @param by A string c("Country", "PACT") if Country, it will create a table for country,
#' if PACT it will create tables for all the programme
#' @param up_to_quarter A string that defines the latest quarter (it is usually defined in set_up.R)
#' @param previous_quarter A string that defines the second last quarter (usually defined in set_up.R)
#' @return A list with 4 tibbles (data_total, data_total_tilltoday, data_total_tillprevq, data_quarter)
#'

tibble_expected_vs_approved <- function(db = reporte_cuenta,
                                         by = c("Country", "PACT", "Component"),
                                         filter_country = "",
                                         filter_component = "",
                                         up_to_quarter = quarter_filter,
                                         previous_quarter = previous_quarter,
                                         ...){

  #keep of all countries if PACT is selected
  if(by == "PACT"){

    data <- db

    message("PACT")

  }

  #PACT and Country are filter by country
  if(by == "Country"){

    data <- db %>%
      filter(Country %in% filter_country)

    message(filter_country)

  }

  if(by == "Component"){

    data <- db %>%
      filter(component == filter_component)

    message(filter_component)

  }




  #All data till today
  data_total <- data %>%
    group_by(Element, Indicator) %>%
    summarise(across(all_of(c("Expected", "Approved")), suma), .groups ="drop")

  data_total_tilltoday <- data %>%
    dplyr::filter(Quarter <= up_to_quarter) %>%
    group_by(Element, Indicator) %>%
    summarise(across(all_of(c("Expected", "Approved")), suma), .groups ="drop")

  data_total_tillprevq <- data %>%
    dplyr::filter(Quarter <= previous_quarter) %>%
    group_by(Element, Indicator) %>%
    summarise(across(all_of(c("Expected", "Approved")), suma), .groups ="drop")


  data_quarter <- data %>%
    filter(Quarter == up_to_quarter) %>%
    group_by(Element, Indicator) %>%
    summarise(across(all_of(c("Expected", "Approved")), suma), .groups ="drop")


  return(list("data_total"= data_total,
              "data_total_tilltoday"=data_total_tilltoday,
              "data_total_tillprevq"=data_total_tillprevq,
              "data_quarter" = data_quarter)
  )



}





