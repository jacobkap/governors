setwd("data")
# This will scrape the table that has all governor name, states, years in
# office, and party.
# Each governor has one row of data.
library(rvest)
page <- read_html("https://www.nga.org/cms/FormerGovBios?begincac77e09-db17-41cb-9de0-687b843338d0=1&endcac77e09-db17-41cb-9de0-687b843338d0=3599&pagesizecac77e09-db17-41cb-9de0-687b843338d0=100&higherOfficesServed=&lastName=&sex=Any&honors=&submit=Search&college=&party=&inOffice=Any&biography=&militaryService=&religion=&firstName=&warsServed=&")

raw_governors <- page %>%
  html_node("body > main > div:nth-child(3) > div > div > div > table") %>%
  html_table(header = TRUE)
names(raw_governors) <- c("governor", "state",
                      "time_in_office", "party")
save(raw_governors, file = "raw_governors.rda")
