
year.df <- read.csv("data/yvw.years.csv")

library(ggplot2)


ggplot(year.df, aes(x=year, y=mean.affordability)) +
  geom_bar(stat='identity', position='dodge') +
  scale_x_continuous(breaks = round(seq(2010, 2017, by = 1),1)) +
  xlab("Year") + ylab("Mean Affordability %") +
  ggtitle("Mean Affordability by Year (minimum quarterly income set to $10)")

ggsave("img/year-mean-affordability-10.png", plot = last_plot())

ggplot(year.df, aes(x=year, y=percent.unaffordable)) +
  geom_bar(stat='identity', position='dodge') +
  scale_x_continuous(breaks = round(seq(2010, 2017, by = 1),1)) +
  xlab("Year") + ylab("Affordability Stress %") +
  ggtitle("Affordability Stress by Year (minimum quarterly income set to $10)") 

ggsave("img/year-percent-unaffordable.png", plot = last_plot())

ggplot(year.df, aes(x=year, y=percent.unaffordable.40pct)) +
  geom_bar(stat='identity', position='dodge') +
  scale_x_continuous(breaks = round(seq(2010, 2017, by = 1),1)) +
  xlab("Year") + ylab("Affordability Stress %") +
  ggtitle("Affordability Stress Amongst Lowest 40% of Income by Year") 

ggsave("img/year-pct-unafford-40.png", plot = last_plot())

year.df$X <- NULL

# renaming
names(year.df)[names(year.df)=='year'] <- 'Year'
names(year.df)[names(year.df)=='mean.affordability'] <- 'Mean.Affordability'
names(year.df)[names(year.df)=='percent.unaffordable'] <- 'Affordability.Stress'
names(year.df)[names(year.df)=='percent.unaffordable.40pct'] <- 'Affordability.Stress.Lowest.40'

library(knitr)
kable(year.df,digits = 2)