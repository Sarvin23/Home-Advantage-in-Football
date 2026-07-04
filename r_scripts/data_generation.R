library(tidyverse)

# ------------------------------
# Function for calculating score
# ------------------------------
calculate_points <- function(results) {
  sum(ifelse(results == "W", 3,
             ifelse(results == "D", 1, 0)))
}

# --------------
# 2020/21 Data
# --------------
teams_20 <- c("ARS","AVL","BHA","BUR","CHE","CRY","EVE","FUL","LEE","LEI",
              "LIV","MCI","MUN","NEW","SHU","SOU","TOT","WBA","WHU","WOL")

home_results_20 <- list(
  c("—", "L", "W", "L", "W", "D", "L", "D", "W", "L", "L", "L", "D", "W", "W", "D", "W", "W", "W", "L"),
  c("W", "—", "L", "D", "W", "W", "D", "W", "L", "L", "W", "L", "L", "W", "W", "L", "L", "D", "L", "D"),
  c("L", "D", "—", "D", "L", "L", "D", "D", "W", "L", "D", "W", "L", "W", "D", "L", "W", "D", "D", "D"),
  c("D", "W", "D", "—", "L", "W", "D", "D", "L", "D", "L", "L", "L", "L", "W", "L", "L", "D", "L", "W"),
  c("L", "D", "D", "W", "—", "W", "W", "W", "W", "W", "L", "L", "D", "W", "W", "D", "D", "L", "W", "D"),
  c("L", "W", "D", "L", "L", "—", "L", "D", "W", "D", "L", "L", "D", "L", "W", "W", "D", "W", "L", "W"),
  c("W", "L", "W", "L", "W", "D", "—", "L", "L", "D", "D", "L", "L", "L", "L", "W", "D", "W", "L", "W"),
  c("L", "L", "D", "L", "L", "L", "L", "—", "L", "L", "D", "L", "L", "L", "W", "D", "L", "W", "D", "L"),
  c("D", "L", "L", "W", "D", "W", "L", "W", "—", "L", "D", "D", "D", "W", "W", "W", "W", "W", "L", "L"),
  c("L", "L", "W", "W", "W", "W", "L", "L", "L", "—", "W", "L", "D", "L", "W", "W", "L", "W", "L", "W"),
  c("W", "W", "L", "L", "L", "W", "L", "L", "W", "W", "—", "L", "D", "D", "W", "W", "W", "D", "W", "W"),
  c("W", "W", "W", "W", "L", "W", "W", "W", "L", "L", "D", "—", "L", "W", "W", "W", "W", "D", "W", "W"),
  c("L", "W", "W", "W", "D", "L", "D", "D", "W", "L", "L", "D", "—", "W", "L", "W", "L", "W", "W", "W"),
  c("L", "D", "L", "W", "L", "L", "W", "D", "L", "L", "D", "L", "L", "—", "W", "W", "D", "W", "W", "D"),
  c("L", "W", "W", "W", "L", "L", "L", "D", "L", "L", "L", "L", "L", "W", "—", "L", "L", "W", "L", "L"),
  c("L", "L", "L", "W", "D", "W", "W", "W", "L", "D", "W", "L", "L", "W", "W", "—", "L", "W", "D", "L"),
  c("W", "L", "W", "W", "L", "W", "L", "D", "W", "L", "L", "W", "L", "D", "W", "W", "—", "W", "D", "W"),
  c("L", "L", "W", "D", "D", "L", "L", "D", "L", "L", "L", "L", "D", "D", "W", "W", "L", "—", "L", "D"),
  c("D", "W", "D", "W", "L", "D", "L", "W", "W", "W", "L", "D", "L", "L", "W", "W", "W", "W", "—", "W"),
  c("W", "L", "W", "L", "W", "W", "L", "W", "W", "D", "L", "L", "L", "D", "W", "D", "D", "L", "L", "—")
)

total_points_20 <- c(61, 55, 41, 39, 67, 44, 59, 28, 59, 66, 69, 86, 74, 45, 23, 43, 62, 26, 65, 45)

home_points_20 <- sapply(home_results_20, calculate_points)

home_adv_20 <- data.frame(
  Team = teams_20,
  Season = "2020/21",
  Home_Points = home_points_20,
  Total_Points = total_points_20
)

home_adv_20$Home_Advantage <- round(
  (home_adv_20$Home_Points / home_adv_20$Total_Points) * 100, 2
)

# ---------------
# 2022/23 Data
# ---------------
teams_22 <- c("ARS","AVL","BOU","BRE","BHA","CHE","CRY","EVE","FUL","LEE",
              "LEI","LIV","MCI","MUN","NEW","NFO","SOU","TOT","WHU","WOL")

home_results_22 <- list(
  c("-", "W", "W", "D", "L", "W", "W", "W", "W", "W", "W", "W", "L", "W", "D", "W", "D", "W", "W", "W"),
  c("L", "-", "W", "W", "W", "L", "W", "W", "W", "W", "L", "L", "D", "W", "W", "W", "W", "W", "L", "D"),
  c("L", "W", "-", "D", "L", "L", "L", "W", "W", "W", "W", "W", "L", "L", "D", "D", "L", "L", "L", "D"),
  c("L", "D", "W", "-", "W", "D", "D", "D", "W", "W", "D", "W", "W", "W", "L", "W", "W", "D", "W", "D"),
  c("L", "L", "W", "D", "-", "W", "W", "L", "L", "W", "W", "W", "D", "W", "D", "D", "W", "L", "W", "W"),
  c("L", "L", "W", "L", "L", "-", "W", "D", "D", "W", "W", "D", "L", "D", "D", "D", "L", "D", "W", "W"),
  c("L", "W", "W", "D", "D", "L", "-", "D", "L", "W", "W", "D", "L", "D", "D", "D", "W", "L", "W", "W"),
  c("W", "L", "W", "W", "L", "L", "W", "-", "L", "W", "L", "D", "L", "L", "L", "D", "L", "D", "W", "L"),
  c("L", "W", "D", "W", "W", "W", "D", "D", "-", "W", "W", "D", "L", "L", "L", "W", "W", "L", "L", "D"),
  c("L", "D", "W", "D", "D", "W", "L", "D", "L", "-", "D", "L", "L", "L", "D", "W", "W", "L", "D", "W"),
  c("L", "L", "L", "D", "D", "L", "D", "D", "L", "W", "-", "L", "L", "L", "L", "W", "L", "W", "W", "W"),
  c("D", "D", "W", "W", "D", "D", "D", "W", "W", "L", "W", "-", "W", "W", "W", "W", "W", "W", "W", "W"),
  c("W", "W", "W", "L", "W", "W", "W", "D", "W", "W", "W", "W", "-", "W", "W", "W", "W", "W", "W", "W"),
  c("W", "W", "W", "W", "L", "W", "W", "W", "W", "D", "W", "W", "W", "-", "D", "W", "D", "W", "W", "W"),
  c("L", "W", "D", "W", "W", "W", "D", "W", "W", "D", "D", "L", "D", "W", "-", "W", "W", "W", "D", "W"),
  c("W", "D", "L", "D", "W", "D", "W", "D", "L", "W", "W", "W", "D", "L", "L", "-", "W", "L", "W", "D"),
  c("D", "L", "L", "L", "L", "W", "L", "L", "L", "D", "W", "D", "L", "L", "L", "L", "-", "D", "D", "L"),
  c("L", "L", "L", "L", "W", "W", "W", "W", "W", "W", "W", "L", "W", "D", "L", "W", "W", "-", "W", "W"),
  c("D", "D", "W", "L", "L", "D", "L", "W", "W", "W", "L", "L", "L", "W", "L", "W", "W", "D", "-", "W"),
  c("L", "W", "L", "W", "L", "W", "W", "D", "D", "L", "L", "W", "L", "L", "D", "W", "W", "W", "W", "-")
)
total_points_22 <- c(84, 61, 39, 59, 62, 44, 45, 36, 52, 31, 34, 67, 89, 75, 71, 38, 25, 60, 40, 41)

home_points_22 <- sapply(home_results_22, calculate_points)

home_adv_22 <- data.frame(
  Team = teams_22,
  Season = "2022/23",
  Home_Points = home_points_22,
  Total_Points = total_points_22
)

home_adv_22$Home_Advantage <- round(
  (home_adv_22$Home_Points / home_adv_22$Total_Points) * 100, 2
)

# -------------------------
# save as csv files
# -------------------------
write_csv(home_adv_20, "../data/home_adv_2020_21.csv")
write_csv(home_adv_22, "../data/home_adv_2022_23.csv")

stacked <- bind_rows(home_adv_20, home_adv_22)

write_csv(stacked, "../data/home_adv_stacked.csv")