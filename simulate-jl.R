# Reads income and bills data
model.df <- read.csv("data/model.df.csv")
rownames(model.df) <- model.df[,"sa2_main11"]

bills.df <- read.csv("data/Anonymised_BillSummary.csv")
#logging::loginfo("Done.")

#rename sa2 codes
names(bills.df)[names(bills.df)=='SA2'] <- 'sa2_main11'

# drop rows with no location
bills.df <- bills.df[complete.cases(bills.df$sa2_main11),]

# drop rows with a  bill > $1000
bill.names <- c("X2010Q1", "X2010Q2", "X2010Q3", "X2010Q4","X2011Q1", "X2011Q2", "X2011Q3", "X2011Q4", "X2012Q1", "X2012Q2", "X2012Q3", "X2012Q4", "X2013Q1", "X2013Q2","X2013Q3", "X2013Q4", "X2014Q1", "X2014Q2", "X2014Q3", "X2014Q4", "X2015Q1", "X2015Q2", "X2015Q3", "X2015Q4","X2016Q1", "X2016Q2", "X2016Q3", "X2016Q4", "X2017Q1", "X2017Q2", "X2017Q3", "X2017Q4")
just.bills.df <- bills.df[bill.names]
bills.df <- bills.df[!rowSums(just.bills.df > 1000, na.rm = TRUE),]

for(year in c(2010,2011,2012,2013,2014,2015,2016,2017)){
  if (year==2010) {
    inc.year <- 2012
  } else if (year==2017) {
    inc.year <- 2016
  } else if (year%%2 == 1) {
    inc.year <- year+1
  } else {
    inc.year <- year
  }
  disp.inc.varname <- paste("disp.inc.",year,sep="")
  mean.varname <- paste("disp.trans.mean.",year,sep="")
  sd.varname <- paste("disp.trans.sd.",year,sep="")
  for(i in unique(bills.df$sa2_main11)){
    bills.df[[disp.inc.varname]][bills.df$sa2_main11==i] <- rnorm(length(which(bills.df$sa2_main11==i)), 
                                                                  model.df[[mean.varname]][model.df$sa2_main11==i],
                                                                  model.df[[sd.varname]][model.df$sa2_main11==i])
  }
  #  bills.df[[disp.inc.varname]] <-13 * (bills.df[[disp.inc.varname]])^8
  
  switch(toString(inc.year),
         "2012"={
           bills.df[[disp.inc.varname]] <-13 * `^`(bills.df[[disp.inc.varname]],3)
         },
         "2014"={
           bills.df[[disp.inc.varname]] <-13 * `^`(bills.df[[disp.inc.varname]],5)
         },
         "2016"={
           bills.df[[disp.inc.varname]] <-13 * exp(bills.df[[disp.inc.varname]])
         }
  )
  
  bills.df <- bills.df[complete.cases(bills.df[[disp.inc.varname]]),]
  # if the simulated value is <= 1 replace with 1
  bills.df[[disp.inc.varname]][bills.df[[disp.inc.varname]] <= 1] <- 1
  
}

#-- Note we have a few missing SA2s since they werent in the NATSEM dataset.
#> unique(na.df$sa2_main11)
#[1] 209021429 209021428 210031438 210031439 210031440 209041432 209041431 209041433 209041436 209041435 209041430 210051445 210051442 209041434 207021425 207021424 210051441
#[18] 210051443 211031451 211031452 211031450 210051444 209041437


write.csv(bills.df, file = "data/bills.disp.inc.csv",row.names=TRUE)