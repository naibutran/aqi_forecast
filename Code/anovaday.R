df <- read.csv('final.csv')


boxplot(df$pm25~factor(df$DY),
        xlab = "DAY", ylab = "pm25")

day_aov <- aov(df$pm25~factor(df$DY), data=df)
# Summary of the analysis
summary(day_aov)

yearoff_data = df %>% filter(YEAR != 2017 & YEAR != 2018 & YEAR != 2019)

yearoff_anova <- aov(yearoff_data$pm25~factor(yearoff_data$YEAR), data=yearoff_data)
summary(yearoff_anova)


TukeyHSD(yearoff_anova,conf.level=0.95)
plot(TukeyHSD(yearoff_anova), las=2 , col="brown")