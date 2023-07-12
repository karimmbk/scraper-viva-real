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
days_urls_2018 <- read_html("Wayback Machine 2018.html", encoding = "utf-8") %>% html_nodes("div.calendar-day a") %>% html_attr("href")
# days_urls_2019 <- read_html("Wayback Machine 2019.html", encoding = "utf-8") %>% html_nodes("div.calendar-day a") %>% html_attr("href")
# days_urls_2020 <- read_html("Wayback Machine 2020.html", encoding = "utf-8") %>% html_nodes("div.calendar-day a") %>% html_attr("href")
# days_urls_2021 <- read_html("Wayback Machine 2021.html", encoding = "utf-8") %>% html_nodes("div.calendar-day a") %>% html_attr("href")
# days_urls_2022 <- read_html("Wayback Machine 2022.html", encoding = "utf-8") %>% html_nodes("div.calendar-day a") %>% html_attr("href")
# days_urls_2023 <- read_html("Wayback Machine 2023.html", encoding = "utf-8") %>% html_nodes("div.calendar-day a") %>% html_attr("href")


# Empty data frame
WayBack_2018_2023 <- data.frame()

# Doing the pagination
for (x in 1:length(days_urls_2018)) {
  # Command to read the link
  link <- days_urls_2018[x]
  page <- read_html(link, enconding = "utf-8")

  # Reading and saving data from ads
  description <- sanitize_str(page %>% html_elements(".property-card__title") %>% html_text())
  address <- sanitize_str(page %>% html_elements("span.property-card__address") %>% html_text())
  price <- sanitize_str(page %>% html_elements(".js-property-card__price-small") %>% html_text())
  area <- sanitize_str(page %>% html_elements("span.property-card__detail-area") %>% html_text())
  bedrooms <- sanitize_str(page %>% html_elements(".property-card_detail-room span.property-card_detail-value") %>% html_text())
  toilets <- sanitize_str(page %>% html_elements(".property-card_detail-bathroom span.property-card_detail-value") %>% html_text())
  garage <- sanitize_str(page %>% html_elements(".property-card_detail-garage span.property-card_detail-value") %>% html_text())

  # Entering the ads and extracting info from the ads page
  ads_links <- page %>% html_nodes("a.property-card__content-link") %>% html_attr("href") %>% paste0(link, .)
  #' Get Internal Info
  #'
  #' @param ads_link 
  #'
  #' @return Information inside a advertisement, in this case "suite", "condo" and "charct."
  #' @export
  #'
  #' @examples
  get_internal_info <- function(ads_link) {
    download.file(ads_link, destfile = "ads_page.html", quiet=TRUE)
    ads_page <- read_html("ads_page.html", encoding="utf-8")

    # Info from the ads
    suite <- ads_page %>% html_nodes("small") %>% html_text2() %>% unlist() %>% sanitize_str()
    condo <- ads_page %>% html_nodes("span.price__list-value.condominium") %>% html_text() %>% unlist() %>% sanitize_str()
    characteristics <- ads_page %>% html_nodes("ul.amenities__list") %>% html_text() %>% sanitize_str() %>% gsub("      ","|",.)

    c(suite, condo, characteristics)
  }

  # Data frame with info inside of ads
  details <- sapply(ads_links, FUN = get_internal_info, USE.NAMES = FALSE)
  table_details <- t(details) %>% as_tibble() %>% setNames(c("suite", "condo", "characteristics"))

  # Making a matrix with the info extracted from the page
  WayBack_2018_2023 <- rbind(WayBack_2018_2023, data.frame(description, address, price, area, bedrooms, toilets, garage, table_details))

  # # Finding the link of the next page (pagination)
  # counter <- x + 1
  # link <- paste0(link, "?pagina=", counter)
  # print(paste("Page", counter))
}

# Export the data frame as Excel file
write.xlsx(WayBack_2018_2023, "~/Documents/WayBack_2018_2023_venda.xlsx")
