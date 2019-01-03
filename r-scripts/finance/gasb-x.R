# clear workspace
rm(list=ls())

# script-specific packages
library(zoo)

# include user functions
source(file="scripts/user-functions.R")

# End of month function for FYEnd
eom <- function(date) {
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2 
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1, hour=0, tz='UTC')  #attr(date,"tz")
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}

# infile and outfile are set to the input and output file paths
spec1 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2014_15/Provisional/f1415_f1a.csv"
spec2 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2014_15/Provisional/flags2015.csv"
expense.outfile <- "C:/Temp/gasb_expensedat2015.csv"
revenue.outfile <- "C:/Temp/gasb_revenuedat2015.csv"
finpos.outfile <- "C:/Temp/gasb_finposdat2015.csv"
aid.outfile <- "C:/Temp/gasb_aiddat2015.csv"

odbcString <- "OSPIR-DEV"

# Year is set to ND's academic year, same as fall year
year <- 2014L

df1 <- readFile(spec2)
df2 <- readFile(spec1)

# make base data file
dat <- merge(df1, df2)

# housekeeping
rm(df1, df2)

# Assign data year
dat$YearId <- year

# create fiscal year and rollup fields
dat$ParentId <- dat$idx_f
dat$ParentId[!dat$ParentId>0] <- dat$unitid[!dat$ParentId>0]

dat$CleanOpinion <- NA
dat$CleanOpinion[dat$gpfs %in% c(1,2)] <- 2 - dat$gpfs[dat$gpfs %in% c(1,2)]

# expenses$FunctionalClassificationId <- as.integer(substr(expenses$field, 4, 5))
dat$FyBegin <- paste(substr(dat$fybeg,1,nchar(dat$fybeg)-4),
                     substr(dat$fybeg,nchar(dat$fybeg)-3,nchar(dat$fybeg)),
                     sep='/')
dat$FyEnd <- paste(substr(dat$fyend,1,nchar(dat$fyend)-4),
                     substr(dat$fyend,nchar(dat$fyend)-3,nchar(dat$fyend)),
                     sep='/')
# head(dat[,c('FYBegin','FyEnd')])
dat$FyBegin <- as.Date(as.yearmon(dat$FyBegin, '%m/%Y'))
dat$FyEnd <- eom(as.Date(as.yearmon(dat$FyEnd, '%m/%Y')))

dat$ParentChildIndicator <- 'Not applicable'
dat$ParentChildIndicator[dat$prch_f==1] <- 'Parent record - includes data from branch campuses'
dat$ParentChildIndicator[dat$prch_f==2] <- 'Child record - data reported with parent campus'
dat$ParentChildIndicator[dat$prch_f==3] <- 'Partial child record - reports paritial data and other data reported with parent campus'
dat$ParentChildIndicator[dat$prch_f==5] <- 'Partial child record - reports partial data but other data is included with entity that is not a postsecondary institution'

dat$GASBReportingModel <- 'Not applicable'
dat$GASBReportingModel[dat$f1gasbal==1] <- 'Business Type Activities'
dat$GASBReportingModel[dat$f1gasbal==2] <- 'Governmental Activities'
dat$GASBReportingModel[dat$f1gasbal==3] <- 'Governmental Activities with Business-Type Activities'
dat$GASBReportingModel[dat$f1gasbal==-1] <- 'Not reported'

dat$PellGrantAccounting <- 'Not applicable'
dat$PellGrantAccounting[dat$f2pell==1] <- 'Pass through (agency)'
dat$PellGrantAccounting[dat$f2pell==2] <- 'Federal grants'
dat$PellGrantAccounting[dat$f2pell==3] <- 'Does not award Pell grants'
dat$PellGrantAccounting[dat$f2pell==-1] <- 'Not reported'

dat$AthleticExpenses <- 'Not applicable'
dat$AthleticExpenses[dat$f_athltc==1] <- 'Auxiliary enterprises'
dat$AthleticExpenses[dat$f_athltc==2] <- 'Student services'
dat$AthleticExpenses[dat$f_athltc==3] <- 'Does not participate in intercollegiate athletics'
dat$AthleticExpenses[dat$f_athltc==4] <- 'Other'
dat$AthleticExpenses[dat$f_athltc==-1] <- 'Not reported'

dat$pcf_f <- as.integer(dat$pcf_f)
dat$pcf_f[is.na(dat$pcf_f) & dat$unitid==dat$ParentId] <- 100
dat$ParentChildAllocationFactor <- as.integer(dat$pcf_f) / 100

dat$HasEndowmentAssets <- 0
dat$HasEndowmentAssets[dat$f1fha==1] <- 1

# get data for each table
# set data types
# rename variables
# handle missing and zeroes
# restructure as needed
# handle parent unitid from idx_f in some way

finpos <- subset(dat,
                 select=c(unitid,
                          YearId,
                          FyBegin,
                          FyEnd,
                          ParentChildIndicator,
                          ParentId,
                          ParentChildAllocationFactor,
                          CleanOpinion,
                          GASBReportingModel,
                          PellGrantAccounting,
                          AthleticExpenses,
                          HasEndowmentAssets,
                          f1a01,
                          f1a31,
                          f1a04,
                          f1a05,
                          f1a06,
                          f1a07,
                          f1a08,
                          f1a09,
                          f1a10,
                          f1a11,
                          f1a12,
                          f1a13,
                          f1a14,
                          f1a15,
                          f1a16,
                          f1a17,
                          f1a18,
                          f1a214,
                          f1a224,
                          f1a234,
                          f1a324,
                          f1a274,
                          f1a27t4,
                          f1a284,
                          f1a334,
                          f1a344,
                          f1d01,
                          f1d02,
                          f1d03,
                          f1d04,
                          f1d05,
                          f1d06,
                          f1h01,
                          f1h02))

names(finpos) <- c('Unitid',
                   'YearId',
                   'FYBegin',
                   'FYEnd',
                   'ParentChildIndicator',
                   'ParentId',
                   'ParentChildAllocationFactor',
                   'CleanOpinion',
                   'GASBReportingModel',
                   'PellGrantAccounting',
                   'AthleticExpenses',
                   'HasEndowmentAssets',
                   'TotalCurrentAssets',
                   'CapitalAssetsNetDeprec',
                   'OtherNonCurrentAssets',
                   'TotalNoncurrentAssets',
                   'TotalAssets',
                   'CurrentLongTermDebt',
                   'OtherCurrentLiabilities',
                   'TotalCurrentLiabilities',
                   'LongTermDebt',
                   'OtherNonCurrentLiabilities',
                   'TotalNonCurrentLiabilities',
                   'TotalLiabilities',
                   'CapitalAssetsNetRelatedDebt',
                   'RestrictedExpendableNetAssets',
                   'RestrictedNonExpendableNetAssets',
                   'UnrestrictedNetAssets',
                   'TotalNetAssets',
                   'Land',
                   'Infrastructure',
                   'Buildings',
                   'Equipment',
                   'Construction',
                   'TotalPlant',
                   'AccumulatedDepreciation',
                   'IntangibleAssetsNetDeprec',
                   'OtherCapitalAssets',
                   'TotalRevenues',
                   'TotalExpenses',
                   'ChangeNetPosition',
                   'NetPositionBeginning',
                   'AdjustmentsNetPositionBeginning',
                   'NetPositionEnding',
                   'EndowmentAssetsBeginning',
                   'EndowmentAssetsEnding')

expenses <- melt(data=dat,
                 id=c('unitid',
                      'YearId'),
                 measure.vars=c('f1c012', 'f1c013', 'f1c014', 'f1c015', 'f1c016', 'f1c017',
                                'f1c022', 'f1c023', 'f1c024', 'f1c025', 'f1c026', 'f1c027',
                                'f1c032', 'f1c033', 'f1c034', 'f1c035', 'f1c036', 'f1c037',
                                'f1c052', 'f1c053', 'f1c054', 'f1c055', 'f1c056', 'f1c057',
                                'f1c062', 'f1c063', 'f1c064', 'f1c065', 'f1c066', 'f1c067',
                                'f1c072', 'f1c073', 'f1c074', 'f1c075', 'f1c076', 'f1c077',
                                'f1c082', 'f1c083', 'f1c084', 'f1c085', 'f1c086', 'f1c087',
                                'f1c105',
                                'f1c112', 'f1c113', 'f1c114', 'f1c115', 'f1c116', 'f1c117',
                                'f1c122', 'f1c123', 'f1c124', 'f1c125', 'f1c126', 'f1c127',
                                'f1c132', 'f1c133', 'f1c134', 'f1c135', 'f1c136', 'f1c137',
                                'f1c142', 'f1c143', 'f1c144', 'f1c145', 'f1c146', 'f1c147'),
                 variable.name="field",
                 value.name="Expense")

names(expenses) <- c('Unitid','YearId','field','Expense')

expenses$FunctionalClassificationId <- as.integer(substr(expenses$field, 4, 5))
expenses$NaturalClassificationId <- as.integer(substr(expenses$field, 6, 6))

expenses$Expense[is.na(expenses$Expense)] <- 0

expenses <- subset(expenses,
                   select=c(Unitid,
                            YearId,
                            FunctionalClassificationId,
                            NaturalClassificationId,
                            Expense))

aid <- melt(data=dat,
                 id=c('unitid',
                      'YearId'),
                 measure.vars=c('f1e01','f1e02','f1e03','f1e04','f1e05','f1e06'),
                 variable.name="field",
                 value.name="Allowance")

names(aid) <- c('Unitid','YearId','field','Allowance')

aid$AidSourceId <- as.integer(substr(as.character(aid$field), 4, 5))

aid$Allowance[is.na(aid$Allowance)] <- 0

aid <- subset(aid,
                   select=c(Unitid,
                            YearId,
                            AidSourceId,
                            Allowance))

revenues <- melt(data=dat,
                 id=c('unitid',
                      'YearId'),
                 measure.vars=c('f1b01', 'f1b02', 'f1b03',
                                'f1b04a','f1b04b','f1b05',
                                'f1b06','f1b26','f1b07',
                                'f1b08','f1b10','f1b11',
                                'f1b12','f1b13','f1b14',
                                'f1b15','f1b16','f1b17',
                                'f1b18'),
                 variable.name='field',
                 value.name='Revenue')

names(revenues) <- c('Unitid','YearId','field','Revenue')

revenues$RevenueClassificationId <- as.integer(substr(revenues$field, 4, 5)) * 10
revenues$RevenueTypeId <- 'Operating'
revenues$RevenueTypeId[revenues$RevenueClassificationId %in% c(10,11,12,13,14,15,16,17,18)] <- 'Non-operating'
revenues$flag <- substr(revenues$field, 6, 6)
revenues$RevenueClassificationId[revenues$flag=='a'] <- revenues$RevenueClassificationId[revenues$flag=='a'] + 1
revenues$RevenueClassificationId[revenues$flag=='b'] <- revenues$RevenueClassificationId[revenues$flag=='b'] + 2
revenues$Revenue[is.na(revenues$Revenue)] <- 0

revenues <- subset(revenues,
                   select=c(Unitid,
                            YearId,
                            RevenueClassificationId,
                            RevenueTypeId,
                            Revenue))

# Write the outfile.  Empty fields are left null
# writeFile(expenses, expense.outfile)
# writeFile(finpos, finpos.outfile)
# writeFile(aid, aid.outfile)

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
con <-odbcConnect(odbcString)

resp <- sqlQuery(con, paste0("DELETE FROM extract.GASBExpenses WHERE YearId=",as.character(year)))
resp <- sqlQuery(con, paste0("DELETE FROM extract.GASBFinancialPosition WHERE YearId=",as.character(year)))
resp <- sqlQuery(con, paste0("DELETE FROM extract.GASBStudentAid WHERE YearId=",as.character(year)))
resp <- sqlQuery(con, paste0("DELETE FROM extract.GASBRevenues WHERE YearId=",as.character(year)))

# Append values to appropriate tables.  Run ONCE or there will be a key violation
sqlSave(con,
        expenses,
        tablename='extract.GASBExpenses',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

sqlSave(con,
        finpos,
        tablename='extract.GASBFinancialPosition',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

sqlSave(con,
        aid,
        tablename='extract.GASBStudentAid',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

sqlSave(con,
        revenues,
        tablename='extract.GASBRevenues',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

# sqlExecute(con,
#            "EXEC [operations].[AddGASBStudentAid] @Unitid = ?, @YearId = ?, @AidSourceId = ?, @Allowance = ?",
#            data = aid)

# Execute Extract-to-Staging SPROC
resp <- sqlQuery(con, "EXEC operations.TransferIpedsFinanceExpensesExtractToStagedTable")

# Close the connection to the db
close(con)
con <- NULL
rm(con)
rm(resp)

# Housekeeping
# rm(list=ls())
