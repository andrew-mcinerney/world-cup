library(readxl)
library(dplyr)

# players names


names <- c("Drew", "Bob", "Curran", "Sully", "Ray", "Maher", "Shaw", "Chang",
           "Coady", "Hugh", "Roy", "Pa", "Hanley")

# selections ---------------------------------------------------------------


for (name in "Drew") {
  my_data <- read_excel("files/Wolfpack World Cup Predictor 2022.xlsm", 
                        sheet = name, col_names = FALSE)
  
  
  # group match  ------------------------------------------------------
  
  
  group_selections <- my_data[, 1:8]
  
  remove_ind <- c(1:2, sapply(1:7, \(x) (9 * x):((9 * x) + 2)), 72:73)
  
  group_selections <- group_selections[-remove_ind, ]
  
  group_selections[, 9] <- ifelse(
    !is.na(group_selections[, 2]), "Home Win (+2)", 
    ifelse(!is.na(group_selections[, 3]), "Home Win",
           ifelse(!is.na(group_selections[, 4]), "Draw",
                  ifelse(!is.na(group_selections[, 5]), "Away Win",
                         ifelse(!is.na(group_selections[, 6]), "Away Win (+2)",
                                "NA"
                         )))))
  
  group_selections <- group_selections[, c(1, 7, 9)]
  
  colnames(group_selections) <- c("Home Team", "Away Team", "Prediction")
  
  group_selections$`Home Team`[group_selections$`Home Team`
                             == "Netrherlands"] = "Netherlands"
  
  group_selections$`Home Team`[group_selections$`Home Team`
                             == "Crotia"] = "Croatia"
  
  group_selections$`Home Team`[group_selections$`Home Team`
                             == "IR Iran"] = "Iran"
  
  
  group_selections$`Away Team`[group_selections$`Away Team`
                             == "Netrherlands"] = "Netherlands"
  
  group_selections$`Away Team`[group_selections$`Away Team`
                             == "Crotia"] = "Croatia"
  
  group_selections$`Away Team`[group_selections$`Away Team`
                             == "IR Iran"] = "Iran"
  
  
  assign(paste0(name, "_selection_gm"), group_selections)
  
  
  
  # group rank points -------------------------------------------------------
  
  group_standings <- my_data[-c(1:2, 7:11, 16:20, 25:29, 34:38, 43:47,
                                52:56, 61:65, 70:73), 9:10]
  colnames(group_standings) <- c("Position", "Team")
  group_standings$Group <- rep(c("A", "B", "C", "D", "E", "F", "G", "H"),
                               each = 4)
  assign(paste0(name, "_selection_gs"), group_standings)
}

