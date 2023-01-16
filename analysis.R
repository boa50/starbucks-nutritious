library(ggplot2)
library(readr)
library(dplyr)

### Question: Can you identify the most nutritious items on the menu?

df <- read_csv("dataset/DataDNA Dataset Challenge -- January 2023.csv")
head(df)
str(df)
colnames(df)
