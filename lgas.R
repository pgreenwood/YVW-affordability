setwd("/Users/greenwood/Documents/YarraWater/AffordabilityAnalysis")

yvw.df <- read.csv("data/bills.disp.inc.afford.csv")

lga.df<- data.frame()
ptile.40 <- quantile(subset(yvw.df,!is.na(affordability.2017))$disp.inc.2017, probs = 0.4, na.rm=TRUE)
for(i in unique(yvw.df$GAZ_LGA)) {

  mean <- 100 * mean(subset(yvw.df,GAZ_LGA == i)$affordability.2017,na.rm=TRUE)
  
  n.3pct <- length(which(subset(yvw.df,GAZ_LGA == i & !is.na(affordability.2017))$affordability.2017 > 0.03))
  pct.unafford <- 100*(n.3pct/nrow(subset(yvw.df,GAZ_LGA == i & !is.na(affordability.2017))))
  
  ptile.40.3pct <- length(which(subset(yvw.df,disp.inc.2017 < ptile.40 & GAZ_LGA == i & !is.na(affordability.2017))$affordability.2017 > 0.03))
  n.ptile.40 <- length(which(subset(yvw.df,GAZ_LGA == i)$disp.inc.2017 < ptile.40))
  pct.unafford.40 <- 100*(ptile.40.3pct/n.ptile.40)
  
  n <- nrow(subset(yvw.df,GAZ_LGA == i & !is.na(affordability.2017)))
  
  # build dataframe of results
  df<- data.frame(i, mean, pct.unafford, pct.unafford.40, n)
  names(df)<-c("GAZ_LGA", "mean.affordability", "percent.unaffordable", "pct.unafford.40", "n")
  lga.df <- rbind(lga.df, df)
}

lga.df$X <- NULL
lga.df <- lga.df[order(as.character(lga.df$GAZ_LGA)),]

library(knitr)
kable(lga.df)

write.csv(lga.df, file = "data/yvw.lgas.csv",row.names=TRUE)