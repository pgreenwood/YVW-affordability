
yvw.df <- read.csv("data/bills.disp.inc.afford.csv")

#add column of deciles
library(dplyr) #for mutate()
yvw.df <- mutate(yvw.df, quantile2017 = ntile(yvw.df$disp.inc.2017, 10),checkBleed=TRUE)

#compute mean affordability, number of bills who pay >3% of disposible income on water bill, 
# , for each decile of disposible income
decile.df<- data.frame()
for(i in unique(yvw.df$quantile2017)) {
  
  mean <- 100 * mean(subset(yvw.df,quantile2017 == i)$affordability.2017,na.rm=TRUE)
  
  n.3pct <- length(which(subset(yvw.df,quantile2017 == i & !is.na(affordability.2017))$affordability.2017 > 0.03))
  pct.unafford <- 100*(n.3pct/nrow(subset(yvw.df,quantile2017 == i & !is.na(affordability.2017))))
  
  median.income <- 4 * median(subset(yvw.df,quantile2017 == i)$disp.inc.2017,na.rm=TRUE)
  
  n <- nrow(subset(yvw.df,quantile2017 == i & !is.na(affordability.2017)))
  
  # build dataframe of results
  df<- data.frame(i,mean,pct.unafford, median.income, n)
  names(df)<-c("decile","mean.affordability", "percent.unaffordable", "median.income", "n")
  decile.df <- rbind(decile.df, df)

}

# drop NA, order by decile
decile.df <- decile.df[!(is.na(decile.df$decile)),]
decile.df <- decile.df[order(decile.df$decile),]

# add in column of decile brakes
breaks <- 4* quantile(yvw.df$disp.inc.2017, prob = seq(0, 1, length = 11), na.rm=TRUE)
breaks.lower <- breaks[-11]
breaks.upper <- breaks[-1]
decile.df$breaks.lower <- breaks.lower
decile.df$breaks.upper <- breaks.upper

decile.df <- decile.df[c("decile","mean.affordability", "percent.unaffordable", "median.income", "breaks.lower", "breaks.upper", "n")]


library(knitr)
kable(decile.df)

write.csv(decile.df, file = "data/yvw.deciles.csv",row.names=TRUE)