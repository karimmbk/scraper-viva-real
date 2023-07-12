# Web scrapping
library(rvest)
library(dplyr)
library(openxlsx)
library(stringr)

#' Sanitize string
#'
#' @param str - string that will be sanitize
#'
#' @return string that has been trimmed if the input is empty or invalid will return '--'
#' @export
#'
#' @examples sanitize_str(" TEST ")
#' @examples sanitize_str(character())
#' Trim spaces and set a default value on null values
sanitize_str <- function (str) {
  if(is.null(str) || length(str) == 0) {
    str <- "--"
  }
  str_trim(str)
}

# URL from the page we gonna run the web scrapping
days_urls <- read_html("Wayback Machine 2022.html", encoding = "utf-8") %>% html_nodes("div.calendar-day a") %>% html_attr("href")

# Empty data frame
WayBack_2018_2023 <- data.frame()

# Doing the pagination
for (x in seq_along(days_urls)) {
  # Command to read the link
  link <- days_urls[x]
  page <- read_html(link, enconding = "utf-8")

  # Reading and saving data from ads
  description <- sanitize_str(page %>% html_elements(".property-card__title") %>% html_text())
  address <- sanitize_str(page %>% html_elements("span.property-card__address") %>% html_text())
  price <- sanitize_str(page %>% html_elements(".js-property-card__price-small") %>% html_text())
  area <- sanitize_str(page %>% html_elements("span.property-card__detail-area") %>% html_text())
  bedrooms <- sanitize_str(page %>% html_elements(".property-card__detail-room span.property-card__detail-value") %>% html_text())
  toilets <- sanitize_str(page %>% html_elements(".property-card__detail-bathroom span.property-card__detail-value") %>% html_text())
  garage <- sanitize_str(page %>% html_elements(".property-card__detail-garage span.property-card__detail-value") %>% html_text())

  # Entering the ads and extracting info from the ads page
  ads_links <- page %>% html_nodes("a.property-card__content-link") %>% html_attr("href") %>% paste0("https://web.archive.org", .)
  #' Get Internal Info
  #'
  #' @param ads_link 
  #'
  #' @return Information inside a advertisement, in this case "suite", "condo" and "charct."
  #' @export
  #'
  #' @examples
  get_internal_info <- function(ads_link) {
    ads_page <- read_html(ads_link, encoding="utf-8")

    # Info from the ads
    suite <- ads_page %>% html_nodes("small") %>% html_text2() %>% unlist() %>% sanitize_str()
    condo <- ads_page %>% html_nodes("span.price__list-value.condominium") %>% html_text() %>% unlist() %>% sanitize_str()
    characteristics <- ads_page %>% html_nodes("ul.amenities__list") %>% html_text() %>% sanitize_str() %>% gsub("      ","|",.)

    c(suite, condo, characteristics)
  }

  # Data frame with info inside of ads
  # details <- sapply(ads_links, FUN = get_internal_info, USE.NAMES = FALSE)
  # table_details <- t(details) %>% as_tibble() %>% setNames(c("suite", "condo", "characteristics"))

  # Making a matrix with the info extracted from the page
  WayBack_2018_2023 <- rbind(WayBack_2018_2023, data.frame(link, description, address, price, area, bedrooms, toilets, garage))

  # # Finding the link of the next page (pagination)
  counter <- x + 1
  # link <- paste0(link, "?pagina=", counter)
  print(paste("Page", counter))
}

# Export the data frame as Excel file
write.xlsx(WayBack_2018_2023, "~/Documents/WayBack_2018_2023_venda.xlsx")
