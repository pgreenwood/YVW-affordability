library(foreign)
#read in disposible incomedata
sih2010.df <- read.csv("data/FSP10BH.csv")
myvars <- c("DISPSCH8", "STATEHBC", "METHHC")
sih2010.df <- sih2010.df[myvars]
#select Melbourne
sih2010.df <- sih2010.df[(sih2010.df$STATEHBC==2 & sih2010.df$METHHC==1),]

sih2012.df <- read.dta("data/sih11bh.dta")
myvars <- c("DISPSCH8", "STATEHBC", "METHHC")
sih2012.df <- sih2012.df[myvars]
#select Melbourne
sih2012.df <- sih2012.df[(sih2012.df$STATEHBC=="Victoria" & sih2012.df$METHHC=="Capital city"),]

sih2014.df <- read.dta("data/sih13bh.dta")
myvars <- c("DISPSCH8", "STATEHBC", "GCCSA11C")
sih2014.df <- sih2014.df[myvars]
#select Melbourne
sih2014.df <- sih2014.df[(sih2014.df$STATEHBC=="Victoria" & sih2014.df$GCCSA11C=="Greater Capital City Area"),]

sih2016.df <- read.csv("data/SIH15BH.CSV")
myvars <- c("DISPSCH8", "STATEHEC", "GCCSA11C")
sih2016.df <- sih2016.df[myvars]
#select Melbourne
sih2016.df <- sih2016.df[(sih2016.df$STATEHEC=="Victoria" & sih2016.df$GCCSA11C=="Greater Capital City Area"),]

#read model
model.df <- read.csv("data/total_disp_medians.m2.df.csv")
rownames(model.df) <- model.df[,"sa2_main11"]

library(e1071) #skewness()
get_income_distribution <- function(year){
  
  df.name <- paste("sih",year,".df",sep="")
  print(df.name)
  df <- get(df.name)
  print(class(df))
  
  # plot these distributions and their logs
  plot <- qplot(df$DISPSCH8, geom="histogram",bins=100)
  
  file.name <- paste("img/",df.name, "-DISPSCH8.png",sep="")
  print(file.name)
  
  ggsave(plot,file=file.name)
  
  # We need log transform to normalise, to take account of the skewness in income distributions
  df <- df[!df$DISPSCH8<=0,]   # non negative values only, as we are log transforming
  df$DISPSCH8log <- log(df$DISPSCH8)
  
  plot <- qplot(df$DISPSCH8log, geom="histogram",bins=100)
  file.name <- paste("img/",df.name, "-DISPSCH8log.png")
  ggsave(plot,file=file.name)
  skewness(df$DISPSCH8)
  skewness(df$DISPSCH8log)
  
  
  sih.log.mean <- mean(df$DISPSCH8log, na.rm = TRUE)
  sih.median <- median(df$DISPSCH8, na.rm = TRUE)
  sih.log.sd <- sd(df$DISPSCH8log, na.rm = TRUE)
  
  row <- data.frame(year,sih.log.mean,sih.median,sih.log.sd)
  names(row) <- c("year","log.mean", "median", "log.sd")
  
  disp_income_year.df <<- rbind(disp_income_year.df, row)
  return(disp_income_year.df) # ? can't get return() to work, so using <<- operator for now.
}



disp_income_year.df <- data.frame(year=integer(),
                                  log.mean=double(), 
                                  median=double(), 
                                  log.sd=double()) 

get_income_distribution(2010)
get_income_distribution(2012)
get_income_distribution(2014)
get_income_distribution(2016)

# --- interpolate in-between years - 2011, 2013, 2015
rownames(disp_income_year.df) <- disp_income_year.df$year

x <- c(2010,2012,2014)

for (i in x) {
  k <- i + 1
  j <- i + 2
  i <- toString(i)
  j <- toString(j)
  k <- toString(k)
  
  log.mean <- (disp_income_year.df[i,]$log.mean + disp_income_year.df[j,]$log.mean)/2
  median <- (disp_income_year.df[i,]$median + disp_income_year.df[j,]$median)/2
  log.sd <- (disp_income_year.df[i,]$log.sd + disp_income_year.df[j,]$log.sd)/2
  
  row <- data.frame(year=k,log.mean=log.mean,median=median,log.sd=log.sd)
  disp_income_year.df <- rbind(disp_income_year.df, row)
}

# 2017 is special case - best leave it out for now
# log.mean <- 2 * disp_income_year.df["2016",]$log.mean - disp_income_year.df["2014",]$log.mean
# median <-  2 * disp_income_year.df["2016",]$median - disp_income_year.df["2014",]$median
# log.sd <-  2 * disp_income_year.df["2016",]$log.sd - disp_income_year.df["2014",]$log.sd
# row <- data.frame(year=2017,log.mean=log.mean,median=median,log.sd=log.sd)
# disp_income_year.df <- rbind(disp_income_year.df, row)

plot(disp_income_year.df$year, disp_income_year.df$median)

#-------

#sort by year
rownames(disp_income_year.df) <- disp_income_year.df$year
disp_income_year.df <- disp_income_year.df[order(disp_income_year.df$year),]

write.csv(disp_income_year.df, file = "data/disp_income_year.df.csv",row.names=TRUE)

#-------

for(year in c(2010, 2011,2012,2013,2014,2015,2016,2017)) {
  
  if (year==2010) {
    inc.year <- 2012
  } else if (year==2017) {
    inc.year <- 2016
  } else if (year%%2 == 1) {
    inc.year <- year+1
  } else {
    inc.year <- year
  }
  
  disp.median.varname <- paste("disposable.",year,sep="")
  disp.trans.mean.varname <- paste("disp.trans.mean.",year,sep="")
  disp.trans.sd.varname <- paste("disp.trans.sd.",year,sep="")
  df.name <- paste("sih",inc.year,".df",sep="")
  df <- get(df.name)
  
  print(year)
  print(inc.year)
  print(disp.trans.mean.varname)
  print(df.name)
  
  library(e1071) #skewness()
  
  for(i in unique(model.df$sa2_main11)) {
    
    df <- df[!df$DISPSCH8<=0,]
    df$DISPSCH8trans<- df$DISPSCH8
    df$DISPSCH8trans <- (model.df[[disp.median.varname]][model.df$sa2_main11==i]/disp_income_year.df[toString(inc.year),]$median) * df$DISPSCH8trans
    #    print(paste("median", i, year, median(df$DISPSCH8trans)))
    df$DISPSCH8transnorm <- df$DISPSCH8trans
    #    df$DISPSCH8transnorm <- `^`(df$DISPSCH8transnorm,(1/8))
    
    switch(toString(inc.year),
           "2012"={
             df$DISPSCH8transnorm <- `^`(df$DISPSCH8transnorm,(1/3))
           },
           "2014"={
             df$DISPSCH8transnorm <- `^`(df$DISPSCH8transnorm,(1/5))
           },
           "2016"={
             df$DISPSCH8transnorm <- log(df$DISPSCH8transnorm)
           }
    )
    
    trans.mean <- mean(df$DISPSCH8transnorm)
    trans.sd <- sd(df$DISPSCH8transnorm)
    model.df[[disp.trans.mean.varname]][model.df$sa2_main11==i] <- trans.mean
    model.df[[disp.trans.sd.varname]][model.df$sa2_main11==i] <- trans.sd
    
  }
  print(paste("skewness",skewness(df$DISPSCH8transnorm)))
  print(paste("kurtosis",kurtosis(df$DISPSCH8transnorm)))
  print(paste("median",median(df$DISPSCH8transnorm)))
  print(paste("mean",mean(df$DISPSCH8transnorm)))
  print(paste("sd",sd(df$DISPSCH8transnorm)))
  
  print(paste("skewness",skewness(df$DISPSCH8)))
  print(paste("kurtosis",kurtosis(df$DISPSCH8)))
  print(paste("median",median(df$DISPSCH8)))
  print(paste("mean",mean(df$DISPSCH8)))
  print(paste("sd",sd(df$DISPSCH8)))
  
}

write.csv(model.df, file = "data/model.df.csv",row.names=TRUE)
