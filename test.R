library(readxl)
library(dplyr)
library(kableExtra)
library(rvest)
library(stringi)
library(stringr)
# players names
names <- c(
  "Drew", "Bob", "Curran", "Sully", "Ray", "Shaw",
  "Coady", "Pa", "Hanley"
)
# standings ---------------------------------------------------------------
standings <- data.frame("Name" = names)
# group results -----------------------------------------------------------
group_results <- read.csv("files/group-results.csv")
# scrape results ----------------------------------------------------------
fixtures_results <- read.csv("files/fixtures-results.csv")
results <- fixtures_results[, c(5, 6, 8, 9)]
url_result <- c("https://www.soccerbase.com/matches/results.sd")
result <- read_html(url_result)
scores <- result %>%
  html_nodes(".score") %>%
  html_text()
teams <- result %>%
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
results <- left_join(results, scraped,
  by = c("Home.Team" = "Home", "Away.Team" = "Away")
)
results[is.na(results$Home.Score) & is.na(results$Away.Score) &
  !is.na(results$Home.score) & !is.na(results$Away.score), c(3, 4)] <-
  results[is.na(results$Home.Score) & is.na(results$Away.Score) &
    !is.na(results$Home.score) & !is.na(results$Away.score), c(5, 6)]
fixtures_results$Home.Score <- results$Home.Score
fixtures_results$Away.Score <- results$Away.Score
write.csv(fixtures_results, "files/fixtures-results.csv", row.names = FALSE)
# fixtures results ---------------------------------------------------------
fixtures_results <- read.csv("files/fixtures-results.csv")
results <- fixtures_results[!is.na(fixtures_results$Home.Score), c(5, 6, 8, 9)]
results$Result <- ifelse(
  results$Home.Score > results$Away.Score + 1, "H2",
  ifelse(results$Home.Score + 1 < results$Away.Score, "A2",
    ifelse(results$Home.Score > results$Away.Score, "H",
      ifelse(results$Home.Score < results$Away.Score, "A",
        ifelse(results$Home.Score == results$Away.Score, "D",
          "NA"
        )
      )
    )
  )
)
results <- results[, c(1, 2, 5)]
# group selection ----------------------------------------------
for (name in names) {
  my_data <- read_excel("files/Wolfpack World Cup Predictor 2022.xlsm",
    sheet = name, col_names = FALSE
  )
  # group match points ------------------------------------------------------
  group_selections <- my_data[, 1:8]
  remove_ind <- c(1:2, sapply(1:7, \(x) (9 * x):((9 * x) + 2)), 72:73)
  group_selections <- group_selections[-remove_ind, ]
  group_selections[, 9] <- ifelse(
    !is.na(group_selections[, 2]), "H2",
    ifelse(!is.na(group_selections[, 3]), "H",
      ifelse(!is.na(group_selections[, 4]), "D",
        ifelse(!is.na(group_selections[, 5]), "A",
          ifelse(!is.na(group_selections[, 6]), "A2",
            "NA"
          )
        )
      )
    )
  )
  group_selections <- group_selections[, c(1, 7, 9)]
  colnames(group_selections) <- c(colnames(results)[1:2], "Prediction")
  group_selections$Home.Team[group_selections$Home.Team
  == "Netrherlands"] <- "Netherlands"
  group_selections$Home.Team[group_selections$Home.Team
  == "Crotia"] <- "Croatia"
  group_selections$Home.Team[group_selections$Home.Team
  == "IR Iran"] <- "Iran"
  group_selections$Home.Team[group_selections$Home.Team
  == "Tunsia"] <- "Tunisia"
  group_selections$Home.Team[group_selections$Home.Team
  == "Switzgerald"] <- "Switzerland"
  group_selections$Away.Team[group_selections$Away.Team
  == "Netrherlands"] <- "Netherlands"
  group_selections$Away.Team[group_selections$Away.Team
  == "Crotia"] <- "Croatia"
  group_selections$Away.Team[group_selections$Away.Team
  == "IR Iran"] <- "Iran"
  group_selections$Away.Team[group_selections$Away.Team
  == "Tunsia"] <- "Tunisia"
  group_selections$Away.Team[group_selections$Away.Team
  == "Switzgerald"] <- "Switzerland"
  df <- left_join(group_selections, results,
    by = c("Home.Team", "Away.Team")
  )
  df$Points <- ifelse(
    (df$Prediction == df$Result) & (df$Prediction == "H2" | df$Prediction == "A2"), 2,
    ifelse(df$Prediction == df$Result, 1,
      ifelse((df$Prediction == "H" & df$Result == "H2") |
        (df$Prediction == "A" & df$Result == "A2"), 1,
      ifelse((df$Prediction == "H2" & df$Result == "H") |
        (df$Prediction == "A2" & df$Result == "A"), 0,
      ifelse(df$Prediction == "H2" | df$Prediction == "A2", -1, 0)
      )
      )
    )
  )
  df$MP <- ifelse(df$Prediction == df$Result, 1,
    ifelse((df$Prediction == "H" & df$Result == "H2") |
      (df$Prediction == "A" & df$Result == "A2"), 1,
    ifelse((df$Prediction == "H2" & df$Result == "H") |
      (df$Prediction == "A2" & df$Result == "A"), 1, 0)
    )
  )
  df$BP <- ifelse((df$Prediction == df$Result) &
    (df$Prediction == "H2" | df$Prediction == "A2"), 1, 0)
  df$DP <- ifelse(!(df$Prediction == df$Result) &
    (df$Prediction == "H2" | df$Prediction == "A2"), -1, 0)
  assign(name, df)
  # group rank points -------------------------------------------------------
  group_standings <- my_data[-c(
    1:2, 7:11, 16:20, 25:29, 34:38, 43:47,
    52:56, 61:65, 70:73
  ), 9:10]
  colnames(group_standings) <- c("Position", "Team")
  group_standings$Group <- rep(c("A", "B", "C", "D", "E", "F", "G", "H"),
    each = 4
  )
  group_standings$Team[group_standings$Team
  == "Crotia"] <- "Croatia"
  group_standings$Team[group_standings$Team
  == "IR Iran"] <- "Iran"
  group_standings$Team[group_standings$Team
  == "Cost Rica"] <- "Costa Rica"
  colnames(group_results)[1] <- "Result"
  df_group <- left_join(group_standings, group_results,
    by = c("Team", "Group")
  )
  df_group$score <- ifelse(df_group$Position == df_group$Result, 1, 0)
  df_group_points <- data.frame(
    Group = c("A", "B", "C", "D", "E", "F", "G", "H"),
    Points = NA
  )
  df_group_points$Points <- sapply(df_group_points$Group, \(x)
  as.numeric(sum(df_group$score[df_group$Group == x]) == 4))
  # round of 16 points ------------------------------------------------------
  R16 <- my_data[c(3, 8, 13, 18, 23, 28, 33, 38), c(13, 14)]
  R16$Prediction <- ifelse(is.na(my_data[c(4, 9, 14, 19, 24, 29, 34, 39), 13]),
    "Away Win", "Home Win"
  )
  R16$Game <- c(49:56)
  R16$Stage <- rep("R16", 8)
  QF <- my_data[c(5, 15, 25, 35), c(17, 18)]
  QF$Prediction <- ifelse(is.na(my_data[c(6, 16, 26, 36), 17]),
    "Away Win", "Home Win"
  )
  QF$Game <- c(57:60)
  QF$Stage <- rep("QF", 4)
  SF <- my_data[c(10, 30), c(21, 22)]
  SF$Prediction <- ifelse(is.na(my_data[c(11, 31), 21]),
    "Away Win", "Home Win"
  )
  SF$Game <- c(61, 62)
  SF$Stage <- rep("SF", 2)
  F_TP <- my_data[c(15, 25), c(25, 26)]
  F_TP$Prediction <- ifelse(is.na(my_data[c(16, 26), 25]),
    "Away Win", "Home Win"
  )
  F_TP$Game <- c(64, 63)
  F_TP$Stage <- c("F", "TP")
  colnames(R16) <- colnames(QF) <- colnames(SF) <- colnames(F_TP) <-
    c("Home Team", "Away Team", "Prediction", "order", "Stage")
  knockouts <- rbind(R16, QF, SF, F_TP)
  knockouts$`Home Team`[knockouts$`Home Team`
  == "Netrherlands"] <- "Netherlands"
  knockouts$`Home Team`[knockouts$`Home Team`
  == "Crotia"] <- "Croatia"
  knockouts$`Home Team`[knockouts$`Home Team`
  == "IR Iran"] <- "Iran"
  knockouts$`Home Team`[knockouts$`Home Team`
  == "Tunsia"] <- "Tunisia"
  knockouts$`Home Team`[knockouts$`Home Team`
  == "Switzgerald"] <- "Switzerland"
  knockouts$`Away Team`[knockouts$`Away Team`
  == "Netrherlands"] <- "Netherlands"
  knockouts$`Away Team`[knockouts$`Away Team`
  == "Crotia"] <- "Croatia"
  knockouts$`Away Team`[knockouts$`Away Team`
  == "IR Iran"] <- "Iran"
  knockouts$`Away Team`[knockouts$`Away Team`
  == "Tunsia"] <- "Tunisia"
  knockouts$`Away Team`[knockouts$`Away Team`
  == "Switzgerald"] <- "Switzerland"
  R16$`Home Team`[R16$`Home Team`
  == "Netrherlands"] <- "Netherlands"
  R16$`Home Team`[R16$`Home Team`
  == "Crotia"] <- "Croatia"
  R16$`Home Team`[R16$`Home Team`
  == "IR Iran"] <- "Iran"
  R16$`Home Team`[R16$`Home Team`
  == "Tunsia"] <- "Tunisia"
  R16$`Home Team`[R16$`Home Team`
  == "Switzgerald"] <- "Switzerland"
  R16$`Away Team`[R16$`Away Team`
  == "Netrherlands"] <- "Netherlands"
  R16$`Away Team`[R16$`Away Team`
  == "Crotia"] <- "Croatia"
  R16$`Away Team`[R16$`Away Team`
  == "IR Iran"] <- "Iran"
  R16$`Away Team`[R16$`Away Team`
  == "Tunsia"] <- "Tunisia"
  R16$`Away Team`[R16$`Away Team`
  == "Switzgerald"] <- "Switzerland"
  knockouts$Result <- NA
  knockouts$Points <- NA
  knockouts <- knockouts[order(knockouts$order), c(1:3, 6, 4, 7)]
  R16_results <- read.csv("files/R16_results.csv")
  R16_results$`Home.Team` <- group_results$Team[c(1, 9, 5, 13, 17, 25, 21, 29)]
  R16_results$`Away.Team` <- group_results$Team[c(6, 14, 2, 10, 22, 30, 18, 26)]
  R16_results <- left_join(R16_results, scraped,
    by = c("Home.Team" = "Home", "Away.Team" = "Away")
  )
  R16_results$Result <- ifelse(!is.na(R16_results$Result), R16_results$Result,
    ifelse(R16_results$Home.score > R16_results$Away.score,
      "Home Win",
      ifelse(R16_results$Home.score < R16_results$Away.score,
        "Away Win", "Draw"
      )
    )
  )
  R16_results <- R16_results[, c(1:4)]
  write.csv(R16_results, "files/R16_results.csv", row.names = FALSE)
  R16$Points <- ifelse(is.na(as.numeric(R16$`Home Team` == R16_results$`Home.Team`)),
    0, as.numeric(R16$`Home Team` == R16_results$`Home.Team`)
  ) +
    ifelse(is.na(as.numeric(R16$`Away Team` == R16_results$`Away.Team`)),
      0, as.numeric(R16$`Away Team` == R16_results$`Away.Team`)
    ) +
    ifelse(is.na(as.numeric(R16$`Home Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ))),
    0, as.numeric(R16$`Home Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ))
    ) +
    ifelse(is.na(as.numeric(R16$`Away Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ))),
    0, as.numeric(R16$`Away Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ))
    ) +
    ifelse(is.na(as.numeric(R16$Prediction == R16_results$Result)), 0,
      2 * as.numeric(R16$Prediction == R16_results$Result)
    )
  R16_points <- sum(R16$`Home Team` == R16_results$`Home.Team`, na.rm = TRUE) +
    sum(R16$`Away Team` == R16_results$`Away.Team`, na.rm = TRUE) +
    sum(R16$`Home Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ), na.rm = TRUE) +
    sum(R16$`Away Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ), na.rm = TRUE) +
    2 * sum(R16$Prediction == R16_results$Result, na.rm = TRUE)
  # round of 16 points ------------------------------------------------------
  R16 <- my_data[c(3, 8, 13, 18, 23, 28, 33, 38), c(13, 14)]
  
  R16$Prediction <- ifelse(is.na(my_data[c(4, 9, 14, 19, 24, 29, 34, 39), 13]),
                           "Away Win", "Home Win"
  )
  
  R16$Game <- c(49:56)
  
  R16$Stage <- rep("R16", 8)
  
  QF <- my_data[c(5, 15, 25, 35), c(17, 18)]
  
  QF$Prediction <- ifelse(is.na(my_data[c(6, 16, 26, 36), 17]),
                          "Away Win", "Home Win"
  )
  
  QF$Game <- c(57:60)
  QF$Stage <- rep("QF", 4)
  SF <- my_data[c(10, 30), c(21, 22)]
  SF$Prediction <- ifelse(is.na(my_data[c(11, 31), 21]),
                          "Away Win", "Home Win"
  )
  SF$Game <- c(61, 62)
  SF$Stage <- rep("SF", 2)
  F_TP <- my_data[c(15, 25), c(25, 26)]
  F_TP$Prediction <- ifelse(is.na(my_data[c(16, 26), 25]),
                            "Away Win", "Home Win"
  )
  F_TP$Game <- c(64, 63)
  F_TP$Stage <- c("F", "TP")
  F_TP_full <- F_TP
  colnames(R16) <- colnames(QF) <- colnames(SF) <- colnames(F_TP) <-
    c("Home Team", "Away Team", "Prediction", "order", "Stage")
  knockouts <- rbind(R16, QF, SF, F_TP)
  knockouts$`Home Team`[knockouts$`Home Team`
                        == "Netrherlands"] <- "Netherlands"
  knockouts$`Home Team`[knockouts$`Home Team`
                        == "Crotia"] <- "Croatia"
  knockouts$`Home Team`[knockouts$`Home Team`
                        == "IR Iran"] <- "Iran"
  knockouts$`Home Team`[knockouts$`Home Team`
                        == "Tunsia"] <- "Tunisia"
  knockouts$`Home Team`[knockouts$`Home Team`
                        == "Switzgerald"] <- "Switzerland"
  knockouts$`Away Team`[knockouts$`Away Team`
                        == "Netrherlands"] <- "Netherlands"
  knockouts$`Away Team`[knockouts$`Away Team`
                        == "Crotia"] <- "Croatia"
  knockouts$`Away Team`[knockouts$`Away Team`
                        == "IR Iran"] <- "Iran"
  knockouts$`Away Team`[knockouts$`Away Team`
                        == "Tunsia"] <- "Tunisia"
  knockouts$`Away Team`[knockouts$`Away Team`
                        == "Switzgerald"] <- "Switzerland"
  R16$`Home Team`[R16$`Home Team`
                  == "Netrherlands"] <- "Netherlands"
  R16$`Home Team`[R16$`Home Team`
                  == "Crotia"] <- "Croatia"
  R16$`Home Team`[R16$`Home Team`
                  == "IR Iran"] <- "Iran"
  R16$`Home Team`[R16$`Home Team`
                  == "Tunsia"] <- "Tunisia"
  R16$`Home Team`[R16$`Home Team`
                  == "Switzgerald"] <- "Switzerland"
  R16$`Away Team`[R16$`Away Team`
                  == "Netrherlands"] <- "Netherlands"
  R16$`Away Team`[R16$`Away Team`
                  == "Crotia"] <- "Croatia"
  R16$`Away Team`[R16$`Away Team`
                  == "IR Iran"] <- "Iran"
  R16$`Away Team`[R16$`Away Team`
                  == "Tunsia"] <- "Tunisia"
  R16$`Away Team`[R16$`Away Team`
                  == "Switzgerald"] <- "Switzerland"
  
  knockouts$Result <- NA
  knockouts$Points <- NA
  knockouts <- knockouts[order(knockouts$order), c(1:3, 6, 4, 7)]
  
  R16_results <- read.csv("files/R16_results.csv")
  R16_results$`Home.Team` <- group_results$Team[c(1, 9, 5, 13, 17, 25, 21, 29)]
  R16_results$`Away.Team` <- group_results$Team[c(6, 14, 2, 10, 22, 30, 18, 26)]
  R16_results <- left_join(R16_results, scraped,
                           by = c("Home.Team" = "Home", "Away.Team" = "Away")
  )
  R16_results$Result <- ifelse(!is.na(R16_results$Result), R16_results$Result,
                               ifelse(R16_results$Home.score > R16_results$Away.score,
                                      "Home Win",
                                      ifelse(R16_results$Home.score < R16_results$Away.score,
                                             "Away Win", "Draw"
                                      )
                               )
  )
  R16_results <- R16_results[, c(1:4)]
  write.csv(R16_results, "files/R16_results.csv", row.names = FALSE)
  R16$Points <- ifelse(is.na(as.numeric(R16$`Home Team` == R16_results$`Home.Team`)),
                       0, as.numeric(R16$`Home Team` == R16_results$`Home.Team`)
  ) +
    ifelse(is.na(as.numeric(R16$`Away Team` == R16_results$`Away.Team`)),
           0, as.numeric(R16$`Away Team` == R16_results$`Away.Team`)
    ) +
    ifelse(is.na(as.numeric(R16$`Home Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ))),
    0, as.numeric(R16$`Home Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ))
    ) +
    ifelse(is.na(as.numeric(R16$`Away Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ))),
    0, as.numeric(R16$`Away Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ))
    ) +
    ifelse(is.na(as.numeric(R16$Prediction == R16_results$Result)), 0,
           2 * as.numeric(R16$Prediction == R16_results$Result)
    )
  R16_points <- sum(R16$`Home Team` == R16_results$`Home.Team`, na.rm = TRUE) +
    sum(R16$`Away Team` == R16_results$`Away.Team`, na.rm = TRUE) +
    sum(R16$`Home Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ), na.rm = TRUE) +
    sum(R16$`Away Team` %in% c(
      R16_results$`Home.Team`,
      R16_results$`Away.Team`
    ), na.rm = TRUE) +
    2 * sum(R16$Prediction == R16_results$Result, na.rm = TRUE)
  # QF Points ---------------------------------------------------------------
  
  QF_results <- read.csv("files/QF_results.csv")
  
  QF_results <- left_join(QF_results, scraped,
                          by = c("Home.Team" = "Home", "Away.Team" = "Away")
  )
  QF_results$Result <- ifelse(!is.na(QF_results$Result), QF_results$Result,
    ifelse(QF_results$Home.score > QF_results$Away.score,
                              "Home Win",
                              ifelse(QF_results$Home.score < QF_results$Away.score,
                                     "Away Win", "Draw"
                              )
  ))
  QF_results <- QF_results[, c(3, 4, 2, 1)]
  
  write.csv(QF_results, "files/QF_results.csv", row.names = FALSE)
  
  
  QF$Points <- ifelse(is.na(as.numeric(QF$`Home Team` == QF_results$`Home.Team`)),
                      0, 2 * as.numeric(QF$`Home Team` == QF_results$`Home.Team`)
  ) +
    ifelse(is.na(as.numeric(QF$`Away Team` == QF_results$`Away.Team`)),
           0, 2 * as.numeric(QF$`Away Team` == QF_results$`Away.Team`)
    ) +
    ifelse(is.na(as.numeric(QF$`Home Team` %in% c(
      QF_results$`Home.Team`,
      QF_results$`Away.Team`
    ))),
    0, 2 * as.numeric(QF$`Home Team` %in% c(
      QF_results$`Home.Team`,
      QF_results$`Away.Team`
    ))
    ) +
    ifelse(is.na(as.numeric(QF$`Away Team` %in% c(
      QF_results$`Home.Team`,
      QF_results$`Away.Team`
    ))),
    0, 2 * as.numeric(QF$`Away Team` %in% c(
      QF_results$`Home.Team`,
      QF_results$`Away.Team`
    ))
    ) +
    ifelse(is.na(as.numeric(QF$Prediction == QF_results$Result)), 0,
           4 * as.numeric(QF$Prediction == QF_results$Result)
    )
  QF_points <- 2 * sum(QF$`Home Team` == QF_results$`Home.Team`, na.rm = TRUE) +
    2 * sum(QF$`Away Team` == QF_results$`Away.Team`, na.rm = TRUE) +
    2 * sum(QF$`Home Team` %in% c(
      QF_results$`Home.Team`,
      QF_results$`Away.Team`
    ), na.rm = TRUE) +
    2 * sum(QF$`Away Team` %in% c(
      QF_results$`Home.Team`,
      QF_results$`Away.Team`
    ), na.rm = TRUE) +
    4 * sum(QF$Prediction == QF_results$Result, na.rm = TRUE)
  
  
  # SF Points ---------------------------------------------------------------
  SF_results <- data.frame(order = c(61:62), Result = NA)
  SF_results$`Home Team` <- ifelse(QF_results$Result[c(1, 3)] == "Home Win",
                                   QF_results$`Home.Team`[c(1, 3)],
                                   QF_results$`Away.Team`[c(1, 3)]
  )
  SF_results$`Away Team` <- ifelse(QF_results$Result[c(2, 4)] == "Home Win",
                                   QF_results$`Home.Team`[c(2, 4)],
                                   QF_results$`Away.Team`[c(2, 4)]
  )
  
  SF_results <- read.csv("files/SF_results.csv")
  
  colnames(SF_results)[1:2] <- c("Away Team", "Home Team")
  
  SF_results <- left_join(SF_results, scraped,
                          by = c("Home Team" = "Home", "Away Team" = "Away")
  )
  

  SF_results$Result <- ifelse(!is.na(SF_results$Result), SF_results$Result,
                              ifelse(SF_results$Home.score > SF_results$Away.score,
                              "Home Win",
                              ifelse(SF_results$Home.score < SF_results$Away.score,
                                     "Away Win", "Draw"
                              )
  ))
  SF_results <- SF_results[, 1:4]
  
  write.csv(SF_results, "files/SF_results.csv", row.names = FALSE)
  
  
  SF$Points <- ifelse(is.na(as.numeric(SF$`Home Team` == SF_results$`Home Team`)),
                      0, 4 * as.numeric(SF$`Home Team` == SF_results$`Home Team`)
  ) +
    ifelse(is.na(as.numeric(SF$`Away Team` == SF_results$`Away Team`)),
           0, 4 * as.numeric(SF$`Away Team` == SF_results$`Away Team`)
    ) +
    ifelse(is.na(as.numeric(SF$`Home Team` %in% c(
      SF_results$`Home Team`,
      SF_results$`Away Team`
    ))),
    0, 4 * as.numeric(SF$`Home Team` %in% c(
      SF_results$`Home Team`,
      SF_results$`Away Team`
    ))
    ) +
    ifelse(is.na(as.numeric(SF$`Away Team` %in% c(
      SF_results$`Home Team`,
      SF_results$`Away Team`
    ))),
    0, 4 * as.numeric(SF$`Away Team` %in% c(
      SF_results$`Home Team`,
      SF_results$`Away Team`
    ))
    ) +
    ifelse(is.na(as.numeric(SF$Prediction == SF_results$Result)), 0,
           8 * as.numeric(SF$Prediction == SF_results$Result)
    )
  SF_points <- 4 * sum(SF$`Home Team` == SF_results$`Home Team`, na.rm = TRUE) +
    4 * sum(SF$`Away Team` == SF_results$`Away Team`, na.rm = TRUE) +
    4 * sum(SF$`Home Team` %in% c(
      SF_results$`Home Team`,
      SF_results$`Away Team`
    ), na.rm = TRUE) +
    4 * sum(SF$`Away Team` %in% c(
      SF_results$`Home Team`,
      SF_results$`Away Team`
    ), na.rm = TRUE) +
    8 * sum(SF$Prediction == SF_results$Result, na.rm = TRUE)
  
  # F Points ---------------------------------------------------------------
  F_results <- data.frame(order = c(64), Result = NA)
  F_results$`Home Team` <- ifelse(SF_results$Result[1] == "Home Win",
                                   SF_results$`Home Team`[c(1)],
                                   SF_results$`Away Team`[c(1)]
  )
  F_results$`Away Team` <- ifelse(SF_results$Result[c(2)] == "Home Win",
                                   SF_results$`Home Team`[c(2)],
                                   SF_results$`Away Team`[c(2)]
  )
  
  # SF_results <- read.csv("files/SF_results.csv")
  
  F_results <- left_join(F_results, scraped,
                          by = c("Home Team" = "Home", "Away Team" = "Away")
  )
  F_results$Result <- ifelse(F_results$Home.score > F_results$Away.score,
                              "Home Win",
                              ifelse(F_results$Home.score < F_results$Away.score,
                                     "Away Win", "Draw"
                              )
  )
  F_results <- F_results[, c(3, 4, 2, 1)]
  
  # write.csv(SF_results, "files/SF_results.csv", row.names = FALSE)
  colnames(F_TP_full)[1:2] <- c("Home Team", "Away Team")
  
  F_TP <- F_TP_full[1, ]
  
  F_TP$Points <- ifelse(is.na(as.numeric(F_TP$`Home Team` == F_results$`Home Team`)),
                      0, 8 * as.numeric(F_TP$`Home Team` == F_results$`Home Team`)
  ) +
    ifelse(is.na(as.numeric(F_TP$`Away Team` == F_results$`Away Team`)),
           0, 8 * as.numeric(F_TP$`Away Team` == F_results$`Away Team`)
    ) +
    ifelse(is.na(as.numeric(F_TP$`Home Team` %in% c(
      F_results$`Home Team`,
      F_results$`Away Team`
    ))),
    0, 8 * as.numeric(F_TP$`Home Team` %in% c(
      F_results$`Home Team`,
      F_results$`Away Team`
    ))
    ) +
    ifelse(is.na(as.numeric(F_TP$`Away Team` %in% c(
      F_results$`Home Team`,
      F_results$`Away Team`
    ))),
    0, 8 * as.numeric(F_TP$`Away Team` %in% c(
      F_results$`Home Team`,
      F_results$`Away Team`
    ))
    ) +
    ifelse(is.na(as.numeric(F_TP$Prediction == F_results$Result)), 0,
           16 * as.numeric(F_TP$Prediction == F_results$Result)
    )
  F_TP_points <- 8 * sum(F_TP$`Home Team` == F_results$`Home Team`, na.rm = TRUE) +
    8 * sum(F_TP$`Away Team` == F_results$`Away Team`, na.rm = TRUE) +
    8 * sum(F_TP$`Home Team` %in% c(
      F_results$`Home Team`,
      F_results$`Away Team`
    ), na.rm = TRUE) +
    8 * sum(F_TP$`Away Team` %in% c(
      F_results$`Home Team`,
      F_results$`Away Team`
    ), na.rm = TRUE) +
    16 * sum(F_TP$Prediction == F_results$Result, na.rm = TRUE)
  # standings ---------------------------------------------------------------
  standings$MP[standings$Name == name] <- sum(df$MP, na.rm = TRUE)
  standings$BP[standings$Name == name] <- sum(df$BP, na.rm = TRUE)
  standings$DP[standings$Name == name] <- sum(df$DP, na.rm = TRUE)
  standings$GR[standings$Name == name] <- sum(df_group_points$Points,
    na.rm = TRUE
  )
  standings$R16[standings$Name == name] <- sum(R16$Points, na.rm = TRUE)
  standings$QF[standings$Name == name] <- sum(QF$Points, na.rm = TRUE)
  standings$SF[standings$Name == name] <- sum(SF$Points, na.rm = TRUE)
  standings$Final[standings$Name == name] <- sum(F_TP$Points, na.rm = TRUE)
}
standings$`Total Points` <- apply(standings[, -1], 1, sum)
standings <- standings[order(standings$`Total Points`, decreasing = TRUE), ]
