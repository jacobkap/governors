library(stringr)
library(tidyverse)
library(lubridate)
setwd(here::here("data"))
load("raw_governors.rda")
governors <- raw_governors
governors$time_in_office <- gsub("\\n|\\t|\\s", " ",
                                 governors$time_in_office)
governors$governor <- gsub("^Gov. ", "", governors$governor)
# Splits rows with multiple times in offices to separate rows
governors = governors %>%
  mutate(time_in_office = strsplit(as.character(time_in_office),
                                   " {2,}")) %>%
  unnest(time_in_office)
governors$time_in_office <- gsub("\\(|\\)", "", governors$time_in_office)
governors$year_start <- as.numeric(str_split_fixed(governors$time_in_office,
                                                   "-", 2)[, 1])
governors$year_end <- as.numeric(str_split_fixed(governors$time_in_office,
                                                 "-", 2)[, 2])
governors$governor <- gsub(" +", " ", governors$governor)
# Governors still in office have NA for year_end
governors$year_end[is.na(governors$year_end)] <- year(Sys.Date())


final <- data.frame()
for (i in 1:nrow(governors)) {
  temp <- governors[i, ]
  temp$year <- NA
  for (n in temp$year_start:temp$year_end) {
    temp2 <- temp
    temp2$year <- n
    temp2$year_start <- NULL
    temp2$year_end <- NULL
    final <- dplyr::bind_rows(final, temp2)
  }
}


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

final$party[final$year >= 1950] <- str_replace_all(final$party[final$year >= 1950],
                                                   parties)
final$party[final$governor == "Robert Martinez"]          <- "Republican"
final$party[final$governor == "Claude Roy Kirk"]          <- "Republican"
final$party[final$governor == "Ronald Wilson Reagan"]     <- "Republican"
final$party[final$governor == "John Sammon McKiernan"]    <- "Democrat"
final$party[final$governor == "Alfred Eastlack Driscoll"] <- "Republican"
final$party[final$governor == "Dennis Joseph Roberts"]    <- "Democrat"
final$party[final$governor == "Patrick Joseph Lucey"]     <- "Democrat"


final$party[final$governor == "Forrest Hood James" &
              final$year %in% 1979:1982] <- "Democrat"
final$party[final$governor == "Forrest Hood James" &
              final$year %in% 1995:1998] <- "Republican"
final$party[final$governor == "Buddy Elson Roemer" &
              final$year %in% 1988:1990] <- "Democrat"
final$party[final$governor == "Buddy Elson Roemer" &
              final$year %in% 1991] <- "Republican"
final$party[final$governor == "Mills Edwin Godwin" &
              final$year %in% 1966:1969] <- "Democrat"
final$party[final$governor == "Mills Edwin Godwin" &
              final$year %in% 1974:1977] <- "Republican"


#area <- tabulizer::locate_areas("Governors-Roster.pdf", pages = 1)
area <- list(c(top    = 86.72847,
               left   = 34.31078,
               bottom =  708.28248,
               right  = 586.48206))
current_governors <- tabulizer::extract_tables("Governors-Roster.pdf", pages = 1, area = area,
                                               method = "stream")
current_governors <- current_governors[[1]]
current_governors <- data.frame(current_governors)
current_governors$X2[current_governors$X2 == ""] <- current_governors$X3[current_governors$X2 == ""]
current_governors$X7 <- NULL
current_governors$X6 <- NULL
current_governors$X3 <- NULL
current_governors[1, ] <- paste0(current_governors[1, ], current_governors[2, ])
current_governors <- current_governors[-2, ]
current_governors <-
  current_governors %>%
  janitor::row_to_names(row_number = 1) %>%
  dplyr::rename_all(janitor::make_clean_names) %>%
  dplyr::rename(state = state_or_jurisdiction) %>%
  dplyr::mutate(year = 2020)

current_governors$party <- gsub(".*\\(", "", current_governors$governor)
current_governors$party <- gsub("\\)", "", current_governors$party)
current_governors$party <- gsub("^D$", "Democrat", current_governors$party)
current_governors$party <- gsub("^R$", "Republican", current_governors$party)
current_governors$party <- gsub("^I$", "Independent", current_governors$party)
current_governors$party <- gsub("^PNP$", "Popular Democratic Party", current_governors$party)
current_governors$governor <- gsub(" \\(.*", "", current_governors$governor)

current_governors$present_termends  <- gsub(".*-| .*", "", current_governors$present_termends)
current_governors$present_termbegan <- gsub(".*-| .*", "", current_governors$present_termbegan)
current_governors$present_termends  <- paste0("20", current_governors$present_termends)
current_governors$present_termbegan <- paste0("20", current_governors$present_termbegan)
current_governors$time_in_office <- paste0(current_governors$present_termbegan, " - ",
                                           current_governors$present_termends)
current_governors$present_termbegan <- NULL
current_governors$present_termends <- NULL

final <-
  final %>%
  dplyr::bind_rows(current_governors) %>%
  dplyr::distinct(.keep_all = TRUE) %>%
  dplyr::arrange(desc(year),
                 state)


united_states_governors_1775_2020 <- final
rownames(united_states_governors_1775_2020) <- 1:nrow(united_states_governors_1775_2020)
save(united_states_governors_1775_2020,
     file = "united_states_governors_1775_2020.rda")
write_csv(united_states_governors_1775_2020,
          path = "united_states_governors_1775_2020.csv")
haven::write_dta(united_states_governors_1775_2020,
                 path = "united_states_governors_1775_2020.dta")
