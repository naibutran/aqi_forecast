install.packages('rstatix')
library(rstatix)



df <- read.csv('final.csv')

# Variance in mean within group and between group


boxplot(df$pm25~factor(df$MO),
        xlab = "MO", ylab = "pm25")

mo_temper_aov <- aov(df$pm25~factor(df$MO), data=df)
# Summary of the analysis
summary(mo_temper_aov)

str(df2wanova)
df$MO <- factor(df$MO, 
                       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                       labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

df$YEAR <- factor(df$YEAR, 
                levels = c(2017, 2018, 2019, 2020, 2021),
                labels = c("2017", "2018", "2019", "2020", "2021"))

df$DY <- factor(df$DY, 
                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31),
                labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"))


df2wanova <- df

df2wanova %>% anova_test(pm25 ~ MO)


df2wanova$MO = factor(df2wanova$MO)
df2wanova$Temperature = factor(df2wanova$Temperature)

df2wanova %>% select(pm25,MO,Temperature) %>%
  anova_test(pm25 ~ MO*Temperature)

df2wanova %>% anova_test(Temperature ~ MO*Pressure)

