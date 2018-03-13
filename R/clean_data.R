library(stringr)
library(pdftools)
library(dplyr)
library(fastDummies)
setwd("data")
source('C:/Users/user/Dropbox/R_project/us_governors/R/utils.R')

# Read data from PDFs
governors <- data.frame(stringsAsFactors = FALSE)
files <- list.files()
for (file in files) {
  file_year <- as.numeric(gsub(".*(....).pdf", "\\1", file))
  data <- pdftools::pdf_text(file)
  if (data[1] != "") {
    data <- data[1]
    data <- unlist(strsplit(data, split = "\n"))
    data <- trimws(data)
    data <- data.frame(str_split_fixed(data, "\\s{2,}|e\\)", 6),
                       stringsAsFactors = FALSE)
    data <- data[data$X1 %in% state.name, ]


    data[, 3:6] <- suppressWarnings(sapply(data[, 3:6], remove_paren))


    # Year 2002 has a column (regular_term_in_years) that no other year has.
    if (file_year == 2002) {
      data[, 3] <- as.numeric(data[, 3])
      start_month <- as.numeric(str_split_fixed(data[, 4], "-", 2)[, 1])
      start_year <- as.numeric(str_split_fixed(data[, 4], "-", 2)[, 2])
      end_year <- start_year + data[, 3]
      end_year[end_year > 99] <- end_year[end_year > 99] - 100
      end_year <- paste0("0", end_year)
      data[, 3] <- paste0(start_month,"-", end_year)
      data <- data[, c(1:2, 4, 3, 5:6)]
    }

    data$party <- str_split_fixed(data[, 2], "\\(", 2)[, 2]
    data$party <- gsub("\\)", "", data$party)
    data$party <- trimws(data$party)
    data$party <- gsub("^D$", "Democrat", data$party)
    data$party <- gsub("^R$", "Republican", data$party)
    data$party <- gsub(".*Ind.*|^I$", "Independent", data$party,
                       ignore.case = TRUE)
    data[, 2] <- gsub("\\(.*", "", data[, 2])
    names(data)[1:6] <- c("state",
                          "governor",
                          "present_term_began",
                          "present_term_end",
                          "num_previous_terms",
                          "max_consecutive_terms")

    data$year <- file_year
    data$present_term_began <- two_to_four_years(data$present_term_began)
    data$present_term_end <- two_to_four_years(data$present_term_end)

    governors <- bind_rows(governors, data)
  }

}

