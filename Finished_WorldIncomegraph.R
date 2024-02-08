library(ggplot2)
library(dslabs)
library(stringr)
library(dplyr)
DIncome<- read.csv("daily_income.csv")

US_income<- DIncome %>% filter (country== "USA") 
China_income <- DIncome %>% filter(country == "China")
US_income<- gsub("X", "",US_income)
Usdf<- as.numeric(US_income)
chdf<-as.numeric(China_income)
China_income <-gsub("X", "", China_income)
Income_avg <- colMeans(DIncome[,2:ncol], na.rm = TRUE)
years <- gsub("X", "", colnames(DIncome[,2:ncol(DIncome)]))
years <- as.numeric(years)
df_income <- data.frame(years,Income_avg, chdf[-1], Usdf[-1])
data_long <- pivot_longer(df_income, cols = -years, names_to = "Variable", values_to = "Income")

ggplot(data_long, aes(x = years, y = Income, color = Variable)) +
  geom_line() +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 1945, linetype = "dashed", color = "black") +
  annotate("text", x = 2024, y = Inf, label = "Year 2024", vjust = 1, hjust = 0.5, color = "black") +
  annotate("text", x = 1945, y = Inf, label = "1945 End of WW2", vjust = 1, hjust = 0.5, color = "black") +
  labs(x = "Year", y = "Average Daily Income", title = "Trend in Global Average Daily Income Over Time") +
  scale_color_manual(values = c("blue", "red", "green"), labels = c("China","World Average","USA")) +
  theme_minimal()          

