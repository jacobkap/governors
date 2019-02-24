library(stringr)
library(tidyverse)
library(lubridate)
setwd("data")
load("raw_governors.rda")
governors <- raw_governors
governors$time_in_office <- gsub("\\n|\\t|\\s", "",
                                 governors$time_in_office)
governors$governor <- gsub("^Gov. ", "", governors$governor)
# Splits rows with multiple times in offices to separate rows
governors = governors %>%
  mutate(time_in_office = strsplit(as.character(time_in_office),
                                   "\\)\\(")) %>%
  unnest(time_in_office)
governors$time_in_office <- gsub("\\(|\\)", "", governors$time_in_office)
governors$year_start <- as.numeric(str_split_fixed(governors$time_in_office,
                                                   "-", 2)[, 1])
governors$year_end <- as.numeric(str_split_fixed(governors$time_in_office,
                                                 "-", 2)[, 2])
governors$governor <- gsub(" +", " ", governors$governor)
# Governors still in office have NA for year_end
governors$year_end[is.na(governors$year_end)] <- year(Sys.Date())


# Sort governors data by year start.
governors <- governors %>% arrange(year_end, year_start)

# Makes a list containing all years (as a vector) that each
# governor was governor for
all_years <- vector("list", length = nrow(governors))
for (i in 1:nrow(governors)) {
  years <- NA
  if (!is.na(governors$year_start[i]) &
      !is.na(governors$year_end[i])) {
    years <- governors$year_start[i]:governors$year_end[i]
  }
  all_years[[i]] <- years
}
governors$years <- all_years



# Make the new blank data.frame
num_states <- length(unique(governors$state))
num_years <- length(min(governors$year_start):max(governors$year_start))
results <- data.frame(matrix(ncol = 4, nrow = num_years * num_states))
names(results) <- c("governor", "state",
                    "year", "party")
results$state <- rep(unique(governors$state), num_years)
results$year <- sort(rep(min(governors$year_start):max(governors$year_start), num_states))



for (i in 1:nrow(governors)) {
  gov <- governors$governor[i]
  party <- governors$party[i]

  results$governor[results$state %in% governors$state[i] &
                     results$year %in% governors$years[[i]]] <- gov
  results$party[results$state %in% governors$state[i] &
                  results$year %in% governors$years[[i]]] <- party
}
results <- results[!is.na(results$governor), ]


parties <- c("Minnesota Independence Party" = "Independent",
             "Alaska Independence Party"    = "Independent",
             "^PDP of Puerto Rico$"         = "Popular Democratic Party (Puerto Rico)",
             "^New Progressive Party of Puerto Rico \\(PNP\\)$" = "New Progressive Party (Puerto Rico)",
             "^Popular Democrat$"           = "Popular Democratic Party (Puerto Rico)",
             "^New Progressive Party$"      = "New Progressive Party (Puerto Rico)",
             "^Democratic$"                 = "Democrat",
             "^Democratic-Farmer-Labor$"    = "Democrat",
             "Minnesota Independence Party" = "Independent",
             "Republican Organizing Committee" = "Republican")

results$party[results$year >= 1950] <- str_replace_all(results$party[results$year >= 1950],
                                                      parties)
results$party[results$governor == "Robert Martinez"] <- "Republican"
results$party[results$governor == "Claude Roy Kirk"] <- "Republican"
results$party[results$governor == "Ronald Wilson Reagan"] <- "Republican"
results$party[results$governor == "John Sammon McKiernan"] <- "Democrat"
results$party[results$governor == "Alfred Eastlack Driscoll"] <- "Republican"
results$party[results$governor == "Dennis Joseph Roberts"] <- "Democrat"
results$party[results$governor == "Patrick Joseph Lucey"] <- "Democrat"


results$party[results$governor == "Forrest Hood James" &
                      results$year %in% 1979:1982] <- "Democrat"
results$party[results$governor == "Forrest Hood James" &
                      results$year %in% 1995:1998] <- "Republican"
results$party[results$governor == "Buddy Elson Roemer" &
                      results$year %in% 1988:1990] <- "Democrat"
results$party[results$governor == "Buddy Elson Roemer" &
                      results$year %in% 1991] <- "Republican"
results$party[results$governor == "Mills Edwin Godwin" &
                      results$year %in% 1966:1969] <- "Democrat"
results$party[results$governor == "Mills Edwin Godwin" &
                      results$year %in% 1974:1977] <- "Republican"

governors <- results
governors <- governors %>% arrange(desc(year), state)
rownames(governors) <- 1:nrow(governors)
save(governors, file = "governors.rda")
write_csv(governors, path = "governors.csv")
haven::write_dta(governors, path = "governors.dta")