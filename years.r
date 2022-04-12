setwd("/Users/greenwood/Documents/YarraWater/AffordabilityAnalysis")

yvw.df <- read.csv("data/bills.disp.inc.afford.csv")

year.df<- data.frame()
for(year in c(2010,2011,2012,2013,2014,2015,2016,2017)){
  affordability.varname <- paste("affordability.", year, sep="")
  disp.inc.varname <- paste("disp.inc.", year, sep="")
  
  p <- sprintf("                                      Year: %s", year)
  print(p)
  
  mean <- 100 * mean(yvw.df[[affordability.varname]],na.rm=TRUE)
  p <- sprintf("                           Mean affordability: %f",  mean)
  print(p)
  
  n.3pct <- length(which(subset(yvw.df,!is.na(affordability.varname))[[affordability.varname]] > 0.03))
  pct.unafford <- 100*(n.3pct/length(na.omit(yvw.df[[affordability.varname]])))
  p <- sprintf(" Percent of bills >3 pct of disposable income: %f", pct.unafford)
  print(p)
  
  ptile.40 <- quantile(subset(yvw.df,!is.na(affordability.varname))[[disp.inc.varname]], probs = 0.4, na.rm=TRUE)
  ptile.40.3pct <- length(which(subset(yvw.df,!is.na(affordability.varname))[[affordability.varname]] > 0.03 & yvw.df[[disp.inc.varname]] < ptile.40))
  n.ptile.40 <- length(which(yvw.df[[disp.inc.varname]] < ptile.40))
  pct.unafford.40 <- 100*(ptile.40.3pct/n.ptile.40)
  p <- sprintf("Percent of bills >3 pct, amongst lowest 40pct: %f", pct.unafford.40)
  print(p)
  
  #??? this gives the wrong answer ???
  #n <- nrow(subset(yvw.df,!is.na(affordability.varname)))
  n <- length(na.omit(yvw.df[[affordability.varname]]))
  p <- sprintf("                         Number of Households: %i", n)
  print(p)
  
  # build dataframe of results
  df<- data.frame(year, mean, pct.unafford, pct.unafford.40, n)
  names(df)<-c("year","mean.affordability", "percent.unaffordable", "percent.unaffordable.40pct", "n")
  year.df <- rbind(year.df, df)
  
  print("--------------------------------------------")
}

year.df <- year.df[order(-year.df$year),]

write.csv(year.df, file = "data/yvw.years.csv")

