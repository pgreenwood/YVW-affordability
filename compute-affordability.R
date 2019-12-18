
yvw.df <- read.csv("data/bills.disp.inc.csv")

for(year in c(2010,2011,2012,2013,2014,2015,2016,2017)){
  # make columns to count missing and non-missing bills
  q1.varname <- paste("X",year,"Q1",sep="")
  q2.varname <- paste("X",year,"Q2",sep="")
  q3.varname <- paste("X",year,"Q3",sep="")
  q4.varname <- paste("X",year,"Q4",sep="")
  bill.names <- c(q1.varname, q2.varname, q3.varname, q4.varname)
  yvw.bills.df <- yvw.df[bill.names]
  na_count.varname <- paste("na_count", year, sep="")
  bill_count.varname <- paste("bill_count", year, sep="")
  yvw.df[[na_count.varname]] <- apply(yvw.bills.df, 1, function(x) sum(is.na(x)))
  yvw.df[[bill_count.varname]] <- 4 - yvw.df[[na_count.varname]]

  #replace NAs with zero
  yvw.df[[q1.varname]][is.na(yvw.df[[q1.varname]])] <- 0
  yvw.df[[q2.varname]][is.na(yvw.df[[q2.varname]])] <- 0
  yvw.df[[q3.varname]][is.na(yvw.df[[q3.varname]])] <- 0
  yvw.df[[q4.varname]][is.na(yvw.df[[q4.varname]])] <- 0
  
  #sum bills to get annual bill
  bill.varname <- paste("bill.", year, sep="")
  yvw.df[[bill.varname]] <- yvw.df[[q1.varname]] + yvw.df[[q2.varname]] + yvw.df[[q3.varname]] + yvw.df[[q4.varname]]
  
  #compute affordability: annual disposable income annual divided by water bill (note, multiply quarterly income by bill_count)
  affordability.varname <- paste("affordability.", year, sep="")
  disp.inc.varname <- paste("disp.inc.", year, sep="")
  yvw.df[[affordability.varname]] <- yvw.df[[bill.varname]] / (yvw.df[[bill_count.varname]] * yvw.df[[disp.inc.varname]])

  # set income to NA for year with zero bill count (this is so we can omit these households from the deciles).
  # Careful, NaNs are converted to NAs after saving as a CSV and re-importing.
  yvw.df[[disp.inc.varname]][is.nan(yvw.df[[affordability.varname]])] <- NA
  
}

write.csv(yvw.df, file = "data/bills.disp.inc.afford.csv",row.names=TRUE)
