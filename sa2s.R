setwd("/Users/greenwood/Documents/YarraWater/AffordabilityAnalysis")

yvw.df <- read.csv("data/bills.disp.inc.afford.csv")

sa2.df<- data.frame()
ptile.40 <- quantile(subset(yvw.df,!is.na(affordability.2017))$disp.inc.2017, probs = 0.4, na.rm=TRUE)
for(i in unique(yvw.df$SA2_NAME)) {

  mean <- 100 * mean(subset(yvw.df,SA2_NAME == i)$affordability.2017,na.rm=TRUE)
  
  code <- mean(subset(yvw.df,SA2_NAME == i)$sa2_main11 ,na.rm=TRUE)
  
  n.3pct <- length(which(subset(yvw.df,SA2_NAME == i & !is.na(affordability.2017))$affordability.2017 > 0.03))
  pct.unafford <- 100*(n.3pct/nrow(subset(yvw.df,SA2_NAME == i & !is.na(affordability.2017))))
  
  ptile.40.3pct <- length(which(subset(yvw.df,disp.inc.2017 < ptile.40 & SA2_NAME == i & !is.na(affordability.2017))$affordability.2017 > 0.03))
  n.ptile.40 <- length(which(subset(yvw.df,SA2_NAME == i)$disp.inc.2017 < ptile.40))
  pct.unafford.40 <- 100*(ptile.40.3pct/n.ptile.40)
  
  n <- nrow(subset(yvw.df,SA2_NAME == i & !is.na(affordability.2017)))
  
  # build dataframe of results
  df<- data.frame(i, code, mean, pct.unafford, pct.unafford.40, n)
  names(df)<-c("SA2_NAME", "SA2_MAIN16", "mean.affordability", "percent.unaffordable", "pct.unafford.40", "n")
  sa2.df <- rbind(sa2.df, df)
}

sa2.df$X <- NULL
sa2.df <- sa2.df[order(as.character(sa2.df$SA2_NAME)),]

library(knitr)
kable(sa2.df)

write.csv(sa2.df, file = "data/yvw.sa2s.csv",row.names=TRUE)