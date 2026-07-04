library(ggplot2)
library(tidyverse)

# -----------------
# Loading data
# -----------------
data1 <- read.csv("data/home_adv_2020_21.csv")
data2 <- read.csv("data/home_adv_2022_23.csv")

data1$dataset <- "2020/21"
data2$dataset <- "2022/23"

# -----------
# Stack data
# ------------
data_stacked <- bind_rows(data1, data2)

# -------------------------
# QQ plot (Normality check)
# -------------------------
ggplot(data_stacked, aes(sample = Home_Advantage)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~dataset, ncol = 2) +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
    title = "Normality Check (QQ Plot)"
  ) +
  theme_minimal()

# Homogenity Variance
# Teams to exclude since they are not in both seasons
# Filter the teams and home advantage data for both seasons
teams_to_exclude <- c("BOU", "BRE", "NFO", "SHU", "WBA", "BUR")

data1_filtered <- data1 %>%
  filter(!Team %in% teams_to_exclude)

data2_filtered <- data2 %>%
  filter(!Team %in% teams_to_exclude)

# Match the teams that are present in both seasons
# Filter the home advantage values for the matched teams
common_teams <- intersect(data1_filtered$Team, data2_filtered$Team)

x <- data1_filtered %>%
  filter(Team %in% common_teams) %>%
  arrange(Team) %>%
  pull(Home_Advantage)

y <- data2_filtered %>%
  filter(Team %in% common_teams) %>%
  arrange(Team) %>%
  pull(Home_Advantage)

# --------------------
# F-test (variance)
# --------------------
f_test_result <- var.test(x, y)
f_test_result


# ----------------
# Paired t-test
# ----------------
t_test_result <- t.test(x, y, paired = TRUE)
t_test_result