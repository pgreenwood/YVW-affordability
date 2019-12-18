#------ Get medians for each SA2 in yvw for each year

natsem2011SA2.df <- read.csv("data/data6413671280028495890.csv", na.string="null")
abs.2011SA2.df <- read.csv("data/data3244659458562335933.csv", na.string="null")
abs.2016SA2.df <- read.csv("data/data2288495865824466110.csv", na.string="null")
yvwsa2s.df <- read.csv("data/yvw-sa2s.csv") # <- for the heritage of this dataset see an_anonymised_billsummary.R

# convert all names to lowercase
names(natsem2011SA2.df) <- tolower(names(natsem2011SA2.df))
names(abs.2011SA2.df) <- tolower(names(abs.2011SA2.df))
names(abs.2011SA2.df) <- tolower(names(abs.2011SA2.df))
names(yvwsa2s.df) <- tolower(names(yvwsa2s.df))

# join
natsem.abs.2011SA2.raw.df <- merge(natsem2011SA2.df, abs.2011SA2.df, by = 'sa2_main11')

# renaming
names(natsem.abs.2011SA2.raw.df)[names(natsem.abs.2011SA2.raw.df)=='median_disposable_household_income_synthetic_estimates'] <- 'disposable'
names(natsem.abs.2011SA2.raw.df)[names(natsem.abs.2011SA2.raw.df)=='median_tot_hhd_inc_weekly'] <- 'total'
names(natsem.abs.2011SA2.raw.df)[names(natsem.abs.2011SA2.raw.df)=='sa2_name11.y'] <- 'sa2_name11'

# keep only interesting vars
myvars <- c("total","disposable", "sa2_main11", "sa2_name11")
natsem.abs.2011SA2.raw.df <- natsem.abs.2011SA2.raw.df[myvars]

# complete cases only
natsem.abs.2011SA2.df <- natsem.abs.2011SA2.raw.df[complete.cases(natsem.abs.2011SA2.raw.df),]

#-- YVW SA2s only
natsem.abs.2011SA2.yvw.df <- subset(natsem.abs.2011SA2.df, natsem.abs.2011SA2.df$sa2_main11 %in% yvwsa2s.df$sa2_main11)

# correlation and regression
cor(natsem.abs.2011SA2.yvw.df[c("total", "disposable")])
lm(natsem.abs.2011SA2.yvw.df$disposable ~ natsem.abs.2011SA2.yvw.df$total)

# total disposable
# total      1.000000   0.975619
# disposable 0.975619   1.000000
# 
# > lm(natsem.abs.2011SA2.yvw.df$disposable ~ natsem.abs.2011SA2.yvw.df$total)
# 
# Call:
#   lm(formula = natsem.abs.2011SA2.yvw.df$disposable ~ natsem.abs.2011SA2.yvw.df$total)
# 
# Coefficients:
#   (Intercept)  natsem.abs.2011SA2.yvw.df$total  
# 367.3033                           0.6284 

# quick hist
hist(natsem.abs.2011SA2.yvw.df$disposable)

#--Very strong correlation hence. Lets try simple first degree polynomial equation:
fit  <- lm(natsem.abs.2011SA2.yvw.df$disposable ~ natsem.abs.2011SA2.yvw.df$total)
plot(natsem.abs.2011SA2.yvw.df$total, natsem.abs.2011SA2.yvw.df$disposable, main="YWV SA2s: NATSEM Disposable vs ABS total", xlab="ABS total", ylab="NATSEM Disposable")
abline(fit, col="red")

int <- summary(fit)$coefficients[1]
beta01 <- summary(fit)$coefficients[2]

# diagnostics
#plot(fit)

#detach(natsem.abs.2011SA2.yvw.df)
attach(natsem.abs.2011SA2.yvw.df)
#-- generate column of income ratio per SA2
natsem.abs.2011SA2.yvw.df$income_ratio = disposable/total

# yvw.missing.df <- subset(yvwsa2s.df, !(yvwsa2s.df$sa2_main11 %in% natsem.abs.2011SA2.yvw.df$sa2_main11))

#---------
#merge in 2016 income data
natsem.abs.yvw.df <- merge(natsem.abs.2011SA2.yvw.df, abs.2016SA2.df, by.x = 'sa2_main11',  by.y = 'sa2_main16')

# keep only interesting vars
myvars <- c("total", "disposable", "sa2_main11", "sa2_name11","income_ratio", "sa2_name16", "median_tot_hhd_inc_weekly")
natsem.abs.yvw.df <- natsem.abs.yvw.df[myvars]

names(natsem.abs.yvw.df)[names(natsem.abs.yvw.df)=='total'] <- 'total.2011'
names(natsem.abs.yvw.df)[names(natsem.abs.yvw.df)=='median_tot_hhd_inc_weekly'] <- 'total.2016'
names(natsem.abs.yvw.df)[names(natsem.abs.yvw.df)=='disposable'] <- 'disposable.2011'

# interpolate new vars
print("creating 2010")
natsem.abs.yvw.df$total.2010 = natsem.abs.yvw.df$total.2011 - (natsem.abs.yvw.df$total.2016-natsem.abs.yvw.df$total.2011)/5
print("created 2010")
natsem.abs.yvw.df$total.2012 = natsem.abs.yvw.df$total.2011 + (natsem.abs.yvw.df$total.2016-natsem.abs.yvw.df$total.2011)/5
natsem.abs.yvw.df$total.2013 = natsem.abs.yvw.df$total.2011 + 2*(natsem.abs.yvw.df$total.2016-natsem.abs.yvw.df$total.2011)/5
natsem.abs.yvw.df$total.2014 = natsem.abs.yvw.df$total.2011 + 3*(natsem.abs.yvw.df$total.2016-natsem.abs.yvw.df$total.2011)/5
natsem.abs.yvw.df$total.2015 = natsem.abs.yvw.df$total.2011 + 4*(natsem.abs.yvw.df$total.2016-natsem.abs.yvw.df$total.2011)/5
natsem.abs.yvw.df$total.2017 = natsem.abs.yvw.df$total.2016 +  (natsem.abs.yvw.df$total.2016-natsem.abs.yvw.df$total.2011)/5

# generate disposable income medians according to our model
for(year in c(2010,2012,2013,2014,2015,2016,2017)){
  disp.median.varname <- paste("disposable.",year,sep="")
  total.median.varname <- paste("total.",year,sep="")
  
  natsem.abs.yvw.df[[disp.median.varname]] = int + beta01 * natsem.abs.yvw.df[[total.median.varname]]
}

# reorder
natsem.abs.yvw.df<-natsem.abs.yvw.df[c('sa2_main11','sa2_name11','sa2_name16','income_ratio','total.2010','disposable.2010',
                                       'total.2011','disposable.2011','total.2012','disposable.2012',
                                       'total.2013','disposable.2013','total.2014','disposable.2014',
                                       'total.2015','disposable.2015','total.2016','disposable.2016','total.2017','disposable.2017')]

# final ratio: 
natsem.abs.yvw.df$final_income_ratio = natsem.abs.yvw.df$disposable.2017/natsem.abs.yvw.df$total.2017

write.csv(natsem.abs.yvw.df, file = "data/total_disp_medians.df.csv",row.names=TRUE)

## alternative model - just use income ratios
# generate disposable income medians according to our model
for(year in c(2010,2012,2013,2014,2015,2016,2017)){
  disp.median.varname <- paste("disposable.",year,sep="")
  total.median.varname <- paste("total.",year,sep="")
  
  natsem.abs.yvw.df[[disp.median.varname]] = natsem.abs.yvw.df$income_ratio * natsem.abs.yvw.df[[total.median.varname]]
}

# final ratio: 
natsem.abs.yvw.df$final_income_ratio = natsem.abs.yvw.df$disposable.2017/natsem.abs.yvw.df$total.2017

write.csv(natsem.abs.yvw.df, file = "data/total_disp_medians.m2.df.csv",row.names=TRUE)