library(tidyverse)
library(ggplot2)

data1 <- read.csv("data/home_adv_2020_21.csv")
data2 <- read.csv("data/home_adv_2022_23.csv")

data <- bind_rows(data1, data2)
# -------------------------
# Summary statistics
# -------------------------
data %>%
  group_by(Season) %>%
  summarise(
    mean = mean(Home_Advantage),
    sd = sd(Home_Advantage),
    min = min(Home_Advantage),
    max = max(Home_Advantage)
  )

# -------------------------
# 1. Distribution plot (VERY useful)
# -------------------------
normal_dist <- ggplot(data2, aes(x = Home_Advantage, fill = Season)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  scale_fill_manual(values = "light green")+
  labs(title = "Distribution of Home Advantage in the
2022/23 Premier League Season",
       x = "Home Advantage ",
       y = "Density")+
  theme(legend.position = "none")+
  geom_vline(xintercept = mean(data2$Home_Advantage),
             color = "red",linetype = "dotted", linewidth = 1.0)

normal_dist


# -------------------------
# 2. Boxplot comparison (key plot)
# -------------------------
boxplots <- ggplot(data, aes(x = Season, y = Home_Advantage, fill = Season)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = c("light blue", "light green")) +
  labs(title = "Home Advantage Boxplot Comparison",
       x = "Season",
       y = "Home Advantage")
boxplots


# -------------------------
# 3. Team comparison plot
# -------------------------
bar_chart <- ggplot(data2, aes(x = reorder(Team, Home_Advantage), y = Home_Advantage, fill = "Home Advantage")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = "light green") +
  labs(title = "Home Advantage in the 2020/21 Premier League Season",
       x = "Team",
       y = "Home Advantage (%)") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none")
bar_chart
