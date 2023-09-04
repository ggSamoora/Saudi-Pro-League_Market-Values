# load libraries
library(RSelenium)
library(tidyverse)
library(rvest)

# initiate Selenium server
rs_driver_object <- rsDriver(browser = "chrome", 
                             verbose = F)

# create the client object
remDr <- rs_driver_object$client

# open the selenium browser from the client side
remDr$open()

# navigate to the site
remDr$navigate("https://www.transfermarkt.us/saudi-professional-league/marktwerteverein/wettbewerb/SA1")

# select the "Accept All" or "Reject All" Pop-up regarding the cookies,
# then proceed with the code
remDr$findElement(using = "xpath", "//div[@class = 'chzn-container chzn-container-single']")$clickElement()

# the year intervalsw to loop through
years <- remDr$findElements(using = "xpath", "//ul[@class='chzn-results']/li")

# function dedicated to locating the data table and extracting it
extract_table <- function(x) {
  remDr$navigate("https://www.transfermarkt.us/saudi-professional-league/marktwerteverein/wettbewerb/SA1")
  Sys.sleep(2.5)
  remDr$executeScript("window.scrollTo(0, (document.body.scrollHeight/4));")
  remDr$findElement(using = "xpath", "//div[@class = 'chzn-container chzn-container-single']")$clickElement()
  Sys.sleep(1)
  years <- remDr$findElements(using = "xpath", "//ul[@class='chzn-results']/li")
  period <- years[[x]]$getElementText() %>% unlist()
  years[[x]]$clickElement()
  remDr$findElement(using = "xpath", "//input[@value='Show']")$clickElement()
  Sys.sleep(1)
  
  # return the cleaned dataset
  return(
    remDr$getPageSource() %>% 
      unlist() %>% 
      read_html() %>% 
      html_table() %>% 
      .[[2]] %>% 
      select(c(3, 5)) %>% 
      rename("club" = "Club",
             "market_value" = "League") %>% 
      slice(-1) %>% 
      mutate(market_value = str_remove_all(market_value, "[€]"),
             period = period)
  )
}

# function which loops through all the years and concatenates all data in one table
df <- map_dfr(1:length(years), extract_table)

# write the data to a csv file
write_csv(df, "data/SPL Market Value 2.csv", na = "")

# stop the selenium server
rs_driver_object$server$stop()
