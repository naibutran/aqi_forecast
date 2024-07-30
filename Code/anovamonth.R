rm(list=ls())
library(dplyr)


df <- read.csv('final.csv')

# Variance in mean within group and between group


boxplot(df$pm25~factor(df$MO),
        xlab = "MO", ylab = "pm25")

month_aov <- aov(df$pm25~factor(df$MO), data=df)
# Summary of the analysis
summary(month_aov)

e345_data = df %>% filter(MO != '' & MO != 2 & MO != 3 & MO != 4 & MO != 5 & MO != 6 & MO != 7 &
                          MO != 8 & MO != 9 & MO != '' & MO != 11 & MO !='')

month_e235_aov <- aov(e345_data$pm25~factor(e345_data$MO), data=e345_data)
summary(month_e235_aov)

TukeyHSD(month_e235_aov,conf.level=0.95)
plot(TukeyHSD(month_e235_aov), las=2 , col="brown")

data = mtcars
# Step 2: Calculate test statistics using aov function
mtcars_aov <- aov(mtcars$disp~factor(mtcars$gear))
summary(mtcars_aov)
TukeyHSD(mtcars_aov,conf.level=0.95)
plot(TukeyHSD(mtcars_aov), las=2 , col="brown")

sum(data$disp)