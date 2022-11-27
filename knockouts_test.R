my_data <- read_excel("files/Wolfpack World Cup Predictor 2022.xlsm", 
                      sheet = name, col_names = FALSE)

R16 <- my_data[c(3, 8, 13, 18, 23, 28, 33, 38), c(13, 14)]

R16$Prediction <- ifelse(is.na(my_data[c(4, 9, 14, 19, 24, 29, 34, 39), 13]),
                        "Away Win", "Home Win")

R16$Game <- c(49:56)
R16$Stage <- rep("R16", 8)

QF <- my_data[c(5, 15, 25, 35), c(17, 18)]

QF$Prediction <- ifelse(is.na(my_data[c(6, 16, 26, 36), 17]),
                         "Away Win", "Home Win")

QF$Game <- c(57:60)
QF$Stage <- rep("QF", 4)


SF <- my_data[c(10, 30), c(21, 22)]

SF$Prediction <- ifelse(is.na(my_data[c(11, 31), 21]),
                        "Away Win", "Home Win")

SF$Game <- c(61, 62)
SF$Stage <- rep("SF", 2)

F_TP <- my_data[c(15, 25), c(25, 26)]

F_TP$Prediction <- ifelse(is.na(my_data[c(16, 26), 25]),
                        "Away Win", "Home Win")

F_TP$Game <- c(64, 63)
F_TP$Stage <- c("F", "TP")

colnames(R16) <- colnames(QF) <- colnames(SF) <- colnames(F_TP) <-
  c("Home Team", "Away Team", "Prediction", "order", "Stage")

knockouts <- rbind(R16, QF, SF, F_TP)

knockouts$Result <- NA
knockouts$Points <- NA

knockouts <- knockouts[order(knockouts$order), c(1:3, 6, 4, 7)]

full_selections <- rbind(full_selections, group_selections, knockouts)

####

full_selections$Player <- c(NA, rep(names, each = 64))
full_selections <- full_selections[-1, ]
full_selections <- full_selections[order(full_selections$order), ]

full_selections$Stage <- c(rep("Group", 48*length(names)),
                           rep("R16", 8*length(names)),
                           rep("QF", 4*length(names)),
                           rep("SF", 2*length(names)),
                           rep("TP", length(names)),
                           rep("F", length(names)))

full_selections$Game <- paste0(full_selections$`Home Team`, " v ",
                               full_selections$`Away Team`)

full_selections$Game[full_selections$order > 48] <- paste0(
  full_selections$Stage[full_selections$order > 48], " (Game ",
  full_selections$order[full_selections$order > 48], ")")



full_selections_result <- full_selections[!is.na(full_selections$Result),
                                          c(7, 1:4, 6, 8, 5)]


full_selections_fixtures <- full_selections[is.na(full_selections$Result),
                                            c(7, 1:3, 8, 5, 9)]


###

full_selections_fixtures  %>%
  select("Player", "Prediction", "Stage") %>% 
  kbl(row.names = FALSE) %>%
  kable_styling() %>%
  pack_rows(index = 
              table(gsub("[1-9].*", "", full_selections_fixtures$Game))[order(
                order(unique(cbind(full_selections_fixtures$Game,
                                   full_selections_fixtures$order))[, 1]))])
