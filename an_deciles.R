
decile.df <- read.csv("data/yvw.deciles.csv")

decile.df$X <- NULL

library(ggplot2)


ggplot(decile.df, aes(x=decile, y=mean.affordability)) +
  geom_bar(stat='identity', position='dodge') +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1)) +
  scale_y_continuous(breaks = round(seq(0, 7, by = 1),1)) +
  geom_hline(yintercept = 3) + 
  xlab("Decile") + ylab("Mean Affordability %") +
  ggtitle("Mean Affordability by Decile")

ggsave("img/decile-mean-affordability.png", plot = last_plot())

ggplot(decile.df, aes(x=decile, y=percent.unaffordable)) +
  geom_bar(stat='identity', position='dodge') +
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1)) +
  scale_y_continuous(breaks = round(seq(0, 90, by = 10),1)) +
  xlab("Decile") + ylab("Affordability Stress %") +
  ggtitle("Affordability Stress by Decile") 

ggsave("img/decile-percent-unaffordable.png", plot = last_plot())

# renaming
names(decile.df)[names(decile.df)=='decile'] <- 'Decile'
names(decile.df)[names(decile.df)=='mean.affordability'] <- 'Mean.Affordability'
names(decile.df)[names(decile.df)=='percent.unaffordable'] <- 'Affordability.Stress'
names(decile.df)[names(decile.df)=='median.income'] <- 'Median.Income'
names(decile.df)[names(decile.df)=='breaks.lower'] <- 'Breaks.Lower'
names(decile.df)[names(decile.df)=='breaks.upper'] <- 'Breaks.Upper'

library(knitr)
digits <- c(0,2,2,0,0,0,0)
kable(decile.df,digits = digits)