# Library for ploting
library(ggplot2)
library(tidyverse)

# 2020/21 Season
teams_20 <- c("ARS", "AVL", "BHA", "BUR", "CHE", "CRY", "EVE", "FUL", "LEE", "LEI",
           "LIV", "MCI", "MUN", "NEW", "SHU", "SOU", "TOT", "WBA", "WHU", "WOL")

# Home match results: "W" for Win, "D" for Draw, "L" for Loss, "-" for same team match
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

# Total gained ponits
total_points_20 <- c(61, 55, 41, 39, 67, 44, 59, 28, 59, 66, 69, 86, 74, 45, 23, 43, 62, 26, 65, 45)

# Calculating home points
# (Win is 3 points, Draw is 1 point and loss is 0)
calculate_points_20 <- function(results) {
  sum(ifelse(results == "W", 3, ifelse(results == "D", 1, 0)))
}

# Computing home points
home_points_20 <- sapply(home_results_20, calculate_points_20)

# Calculating Home Advantage
# Home gained points/ Total gained points
home_advantage_20 <- round((home_points_20 / total_points_20)*100, 2)

# result as dataframe
home_adv_df <- data.frame(Team = teams_20, Home_Points = home_points_20, Total_Points = total_points_20,
                          Home_Advantage = home_advantage_20)
home_adv_df

# Home Advantage plot for 2020/21 Season

a <-ggplot(home_adv_df, aes(x = reorder(Team, Home_Advantage), y = Home_Advantage, fill = "Home Advantage")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = "light blue") +
  labs(title = "Home Advantage in the 2020/21 Premier League Season",
       x = "Team",
       y = "Home Advantage (%)") +
  theme(axis.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none")
a

# 2022/23 season
teams <- c("ARS", "AVL", "BOU", "BRE", "BHA", "CHE", "CRY", "EVE", "FUL", "LEE",
           "LEI", "LIV", "MCI", "MUN", "NEW", "NFO", "SOU", "TOT", "WHU", "WOL")

# Home match results: "W" for Win, "D" for Draw, "L" for Loss, "-" for same team match
home_results <- list(
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

# Total gained points
total_points <- c(84, 61, 39, 59, 62, 44, 45, 36, 52, 31, 34, 67, 89, 75, 71, 38, 25, 60, 40, 41)

# Calculating home points
# (Win is 3 points, Draw is 1 point and loss is 0)
calculate_points <- function(results) {
  sum(ifelse(results == "W", 3, ifelse(results == "D", 1, 0)))
}

# Computing home points
home_points <- sapply(home_results, calculate_points)

# Calculating Home Advantage
# Home gained points/ Total gained points
home_advantage <- round((home_points / total_points)*100, 2)

# result as dataframe
home_adv_df_2 <- data.frame(Team = teams, Home_Points = home_points, Total_Points = total_points,
                            Home_Advantage = home_advantage)
home_adv_df_2


# Home Advantage plot for 2022/2023 Season

b <- ggplot(home_adv_df_2, aes(x = reorder(Team, Home_Advantage), y = Home_Advantage, fill = "Home Advantage")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = "light green") +
  labs(title = "Home Advantage in the 2022/23 Premier League Season",
       x = "Team",
       y = "Home Advantage (%)") +
  theme(axis.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none")

b

# Assumptions
# 1. Normality
home_adv_df_2$dataset <- "2022/23"
home_adv_df$dataset <- "2020/21"

# Stacking the data
data_stacked <- rbind(
  home_adv_df_2[, c("Home_Advantage", "dataset")],
  home_adv_df[, c("Home_Advantage", "dataset")]
)

# QQ plot
p <- ggplot(data_stacked, aes(sample = Home_Advantage)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~dataset, ncol = 2, scales = "free") +
  labs(
    x = "Theoretical Quantile",
    y = "Empirical Quantile"
  ) +
  theme_minimal()
p

# 2. Homogenity Variance

# Teams to exclude since they are not in both seasons
teams_to_exclude <- c("BOU", "BRE", "NFO", "SHU", "WBA", "BUR")

# Filter the teams and home advantage data for both seasons
filtered_teams_20 <- setdiff(teams_20, teams_to_exclude)
filtered_teams_22 <- setdiff(teams, teams_to_exclude)

# Match the teams that are present in both seasons
common_teams <- intersect(filtered_teams_20, filtered_teams_22)

# Filter the home advantage values for the matched teams
filtered_home_adv_20 <- home_adv_df$Home_Advantage[home_adv_df$Team %in% common_teams]
filtered_home_adv_22 <- home_adv_df_2$Home_Advantage[home_adv_df_2$Team %in% common_teams]

f_test_result <- var.test(filtered_home_adv_20, filtered_home_adv_22)
f_test_result


# Paired t-test to compare home advantage between the two seasons
t_test_result <- t.test(filtered_home_adv_20, filtered_home_adv_22, paired = TRUE)
t_test_result