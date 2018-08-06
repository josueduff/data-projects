library(tidyverse)
library(lubridate)
library(ggplot2)

data <- Falmouth.2018...Falmouth.Flyer.2018

data$full_name <- paste(data$firstname, data$lastname)

data$Time.Start <- seconds(hms(data$Time.Start)) - seconds(hms("10:45:00"))
data$Time.Lap_1 <- seconds(hms(data$Time.Lap_1)) + data$Time.Start
data$Time.Lap_2 <- seconds(hms(data$Time.Lap_2)) + data$Time.Lap_1
data$Time.Lap_3 <- seconds(hms(data$Time.Lap_3)) + data$Time.Lap_2
data$Time.Lap_4 <- seconds(hms(data$Time.Lap_4)) + data$Time.Lap_3
data$Time.Lap_5 <- seconds(hms(data$Time.Lap_5)) + data$Time.Lap_4
data$Time.Lap_6 <- seconds(hms(data$Time.Lap_6)) + data$Time.Lap_5
data$Time.Lap_7 <- seconds(hms(data$Time.Lap_7)) + data$Time.Lap_6
data$Time.Lap_8 <- seconds(hms(data$Time.Lap_8)) + data$Time.Lap_7
data$Time.Lap_9 <- seconds(hms(data$Time.Lap_9)) + data$Time.Lap_8
data$Time.Finish <- seconds(hms(data$Time.Finish)) + data$Time.Start

data_gathered <- data %>%
  gather(Lap, Time, -full_name, -status, -Category)

data_gathered$Time <- as.numeric(gsub("S", "", data_gathered$Time))
data_gathered$Category <- as.factor(data_gathered$Category)

data_gathered$Lap <- factor(data_gathered$Lap, levels = c("Time.Finish", "Time.Lap_8", "Time.Lap_7", "Time.Lap_6", "Time.Lap_5", "Time.Lap_4", "Time.Lap_3", "Time.Lap_2", "Time.Lap_1", "Time.Start"))

finishers <- data_gathered %>%
  select(full_name, Lap, Time, Category) %>%
  filter(Lap == "Time.Finish") %>%
  group_by(Category) %>%
  filter(row_number() < 2)


ggplot() +
  geom_hline(yintercept = 0, colour = "white") +
  geom_hline(data = finishers, aes(yintercept = Time), alpha=0.3, size = 0.1, colour = "#6a3d9a") +
  geom_point(data = data_gathered, aes(x = Category, y = Time, colour = Lap), alpha = 0.5, na.rm = TRUE, size = 2.5, stroke = 0) +
  scale_color_brewer(palette = "Paired", direction = -1, labels = c("Finish", "8", "7", "6", "5", "4", "3", "2", "1", "Start")) +
  geom_text(data = finishers, aes(x = 7.8, y = Time, label = Category, vjust = 1, hjust = 1), colour = "#6a3d9a") +
  geom_text(data = data_gathered, aes(x = Category, y = Time, label = full_name), size = 0.7, alpha = 0.5) +
  labs(y = "Elapsed time (seconds)") +
  ggtitle("Timeline of finishline crossings") +
  theme(panel.grid = element_blank())



  