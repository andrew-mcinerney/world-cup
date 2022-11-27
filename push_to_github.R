library(git2r)
library(rvest)
library(dplyr)
library(stringr)

setwd("C:\\Users\\andrew.mcinerney\\Documents\\world-cup")

fixtures_results <- read.csv("files/fixtures-results.csv")

results <- fixtures_results[, c(5, 6, 8, 9)]

url_result <- c("https://www.soccerbase.com/matches/results.sd")

result <- read_html(url_result)

scores <- result %>% 
  html_nodes(".score") %>%
  html_text() 

teams <-  result %>% 
  html_nodes(".team") %>%
  html_text() 

Home <- teams[seq(1, length(teams) - 1, by = 2)]
Away <- teams[seq(2, length(teams), by = 2)]
Home.score <- sapply(scores, \(x) str_sub(x, start = 1, end = 1))
Away.score <- sapply(scores, \(x) str_sub(x, start = -1, end = -1))

scraped <- data.frame(Home, Away, Home.score, Away.score)

scraped$Home.score <- ifelse(scraped$Home.score == "v", NA, scraped$Home.score)
scraped$Away.score <- ifelse(scraped$Away.score == "v", NA, scraped$Away.score)

scraped$Home[scraped$Home == "South Korea"] <- "Korea Republic"
scraped$Away[scraped$Away == "South Korea"] <- "Korea Republic"

results1 <- left_join(results, scraped,
                      by = c("Home.Team" = "Home", "Away.Team" = "Away"))

if (any(is.na(results1$Home.Score) & is.na(results1$Away.Score) &
        !is.na(results1$Home.score) & !is.na(results1$Away.score))) {

  
}


