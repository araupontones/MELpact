#'unlist characters
#'@param x variable listed in a character

unlist_character <- function(x){

  case_when(str_detect(x, "display_value") ~ str_extract(x, '(?<=display_value = \")(.*?)(?=\\")'),
            T ~ x
  )
}
