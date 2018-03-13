setwd("data")

# This will scrape the table that has all governor name, states, years in
# office, and party.
# Each governor has one row of data.
library(rvest)
governors <- read_html("https://www.nga.org/cms/FormerGovBios?begincac77e09-db17-41cb-9de0-687b843338d0=1&endcac77e09-db17-41cb-9de0-687b843338d0=3599&pagesizecac77e09-db17-41cb-9de0-687b843338d0=100&higherOfficesServed=&lastName=&sex=Any&honors=&submit=Search&college=&party=&inOffice=Any&biography=&militaryService=&religion=&firstName=&warsServed=&")

governors <- governors %>%
  html_node("body > main > div:nth-child(3) > div > div > div > table") %>%
  html_table(header = TRUE)


# Download files
for (i in 2002:2017) {
  url <- paste0("https://www.nga.org/files/live/sites/NGA/files/",
                "pdf/directories/GovList",
                i,
                ".pdf")
  if (i == 2017) {
    url <- paste0("https://www.nga.org/files/live/sites/NGA/files",
                  "/pdf/2018/OMCT/2017%20Roster.pdf")
  }

  download.file(url,
                destfile = paste0("governor_data_", i, ".pdf"),
                mode = "wb")
}
