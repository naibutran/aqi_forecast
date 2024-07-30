
library(dplyr)


df <- read.csv('final.csv')


boxplot(df$pm25~factor(df$YEAR),
        xlab = "YEAR", ylab = "pm25")

year_aov <- aov(df$pm25~factor(df$YEAR), data=df)
# Summary of the analysis
summary(year_aov)
yearoff_data = df %>% filter(YEAR != 2017 & YEAR != '' & YEAR != '' & YEAR != ''
                             & YEAR != 2021)

yearoff_anova <- aov(yearoff_data$pm25~factor(yearoff_data$YEAR), data=yearoff_data)
summary(yearoff_anova)


TukeyHSD(yearoff_anova,conf.level=0.95)
plot(TukeyHSD(yearoff_anova), las=2 , col="brown")
