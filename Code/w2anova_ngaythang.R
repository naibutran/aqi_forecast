df <- read.csv('final.csv')

# Variance in mean within group and between group


boxplot(df$pm25~factor(df$MO),
        xlab = "MO", ylab = "pm25")

mo_dy_aov <- aov(df$pm25~factor(df$MO)*factor(df$DY), data=df)
# Summary of the analysis
summary(mo_dy_aov)