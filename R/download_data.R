setwd("data")
library(stringr)
# This will scrape the table that has all governor name, states, years in
# office, and party.
# Each governor has one row of data.
library(rvest)
page <- read_html("https://www.nga.org/cms/FormerGovBios?begincac77e09-db17-41cb-9de0-687b843338d0=1&endcac77e09-db17-41cb-9de0-687b843338d0=3599&pagesizecac77e09-db17-41cb-9de0-687b843338d0=100&higherOfficesServed=&lastName=&sex=Any&honors=&submit=Search&college=&party=&inOffice=Any&biography=&militaryService=&religion=&firstName=&warsServed=&")

governors <- page %>%
  html_node("body > main > div:nth-child(3) > div > div > div > table") %>%
  html_table(header = TRUE)
names(governors) <- c("governor", "state",
                      "time_in_office", "party")

governors$time_in_office <- gsub("\\n|\\t|\\s", "",
                                 governors$time_in_office)
z = governors$time_in_office[1:10]
all_times <- str_split_fixed(governors$time_in_office, "\\)\\(", 3)
governors$time_in_office1 <- all_times[, 1]
governors$time_in_office2 <- all_times[, 2]
governors$time_in_office3 <- all_times[, 3]
governors$time_in_office1 <- gsub("\\(|\\)", "", governors$time_in_office1)
governors$time_in_office2 <- gsub("\\(|\\)", "", governors$time_in_office2)
governors$time_in_office3 <- gsub("\\(|\\)", "", governors$time_in_office3)
governors$year_start1 <- as.numeric(str_split_fixed(governors$time_in_office1,
                                                    "-", 2)[, 1])
governors$year_end1 <- as.numeric(str_split_fixed(governors$time_in_office1,
                                                  "-", 2)[, 2])
governors$year_start2 <- as.numeric(str_split_fixed(governors$time_in_office2,
                                                    "-", 2)[, 1])
governors$year_end2 <- as.numeric(str_split_fixed(governors$time_in_office2,
                                                  "-", 2)[, 2])
governors$year_start3 <- as.numeric(str_split_fixed(governors$time_in_office3,
                                                    "-", 2)[, 1])
governors$year_end3 <- as.numeric(str_split_fixed(governors$time_in_office3,
                                                  "-", 2)[, 2])


# Make the new blank data.frame
num_states <- length(unique(governors$state))
num_years <- length(min(governors$year_start1):max(governors$year_start1))
results <- data.frame(matrix(ncol = 4, nrow = num_years * num_states))
names(results) <- c("governor", "state",
                    "year", "party")
results$state <- rep(unique(governors$state), num_years)
results$year <- sort(rep(min(governors$year_start1):max(governors$year_start1), num_states))


# Makes a list containing all years (as a vector) that each
# governor was governor for
all_years <- vector("list", length = nrow(governors))
for (i in 1:nrow(governors)) {
  years <- NA
  if (!is.na(governors$year_start1[i]) &
      !is.na(governors$year_end1[i])) {
    years <- governors$year_start1[i]:governors$year_end1[i]
  }
  if (!is.na(governors$year_start2[i]) &
      !is.na(governors$year_end2[i])) {
    years <- c(years, governors$year_start2[i]:governors$year_end2[i])
  }
  if (!is.na(governors$year_start3[i]) &
      !is.na(governors$year_end3[i])) {
    years <- c(years, governors$year_start3[i]:governors$year_end3[i])
  }
  all_years[[i]] <- years
}
governors$years <- all_years


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
