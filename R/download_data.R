setwd("data")
# This will scrape the table that has all governor name, states, years in
# office, and party.
# Each governor has one row of data.
library(rvest)
library(dplyr)
library(janitor)
page <- read_html("https://www.nga.org/former-governors/search/?govq_name=&govq_state=&govq_keyword=&govq_party_affiliation=&govq_birth_state=&govq_school=&govq_awards=&govq_military_service=&govq_status=&govq_profession=&govq_gender=&govq_race=&govq_natl_office_served=&govq_terms=")

raw_governors <- page %>%
  html_node(".table__container > table:nth-child(1)") %>%
  html_table(header = TRUE) %>%
  dplyr::rename_all(janitor::make_clean_names) %>%
  dplyr::rename(governor = governors_name)
save(raw_governors, file = "raw_governors.rda")
