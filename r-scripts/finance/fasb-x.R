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

# Year is set to ND's academic year, same as fall year
year <- 2005L

# infile and outfile are set to the input and output file paths
spec1 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2005_06/Final/f0506_f2.csv"
spec2 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2005_06/Final/flags2006.csv"
expense.outfile <- paste0("C:/Temp/fasb_expensedat",year,".csv")
revenue.outfile <- paste0("C:/Temp/fasb_revenuedat",year,".csv")
finpos.outfile <- paste0("C:/Temp/fasb_finposdat",year,".csv")
aid.outfile <- paste0("C:/Temp/fasb_aiddat",year,".csv")

odbcString <- "OSPIR-DEV"

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

dat$GASBAlternativeModel <- 'Not applicable'
dat$GASBAlternativeModel[dat$f1gasbal==1] <- 'Business Type Activities'
dat$GASBAlternativeModel[dat$f1gasbal==2] <- 'Governmental Activities'
dat$GASBAlternativeModel[dat$f1gasbal==3] <- 'Governmental Activities with Business-Type Activities'
dat$GASBAlternativeModel[dat$f1gasbal==-1] <- 'Not reported'

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
                          GASBAlternativeModel,
                          PellGrantAccounting,
                          AthleticExpenses,
                          f2a01,
                          f2a19,
                          f2a20,
                          f2a02,
                          f2a03,
                          f2a03a,
                          f2a04,
                          f2a05a,
                          f2a05b,
                          f2a05,
                          f2a06,
                          f2a11,
                          f2a12,
                          f2a13,
                          f2a15,
                          f2a16,
                          f2a17,
                          f2a18,
                          f2b01,
                          f2b02,
                          f2b03,
                          f2b04,
                          f2b05,
                          f2b06,
                          f2b07,
                          f2c08,
                          f2c09,
                          f2d173,
                          f2d174,
                          f2d18,
                          f2h01,
                          f2h02))

names(finpos) <- c('Unitid',
                   'YearId',
                   'FYBegin',
                   'FYEnd',
                   'ParentChildIndicator',
                   'ParentId',
                   'ParentChildAllocationFactor',
                   'CleanOpinion',
                   'GASBAlternativeModel',
                   'PellGrantAccounting',
                   'AthleticExpenses',
                   'LongTermInvestments',
                   'PropertyNetDeprec',
                   'IntangibleAssets',
                   'TotalAssets',
                   'TotalLiabilities',
                   'PropertyDebt',
                   'UnrestrictedNetAssets',
                   'PermRestrictedNetAssets',
                   'TempRestrictedNetAssets',
                   'RestrictedNetAssets',
                   'TotalNetAssets',
                   'Land',
                   'Buildings',
                   'Equipment',
                   'Construction',
                   'OtherPlant',
                   'TotalPlant',
                   'AccumulatedDepreciation',
                   'TotalRevenuesInvReturn',
                   'TotalExpenses',
                   'OtherChangesNetAssets',
                   'ChangeNetAssets',
                   'NetAssetsBeginning',
                   'AdjustmentsNetAssetsBeginning',
                   'NetAssetsEnding',
                   'AllowancesTuition',
                   'AllowancesAuxiliaryEnterprise',
                   'NetAssetsReleasedTempRestriction',
                   'NetAssetsReleasedPermRestriction',
                   'NetRevenuesAfterAssetsReleased',
                   'EndowmentAssetsBeginning',
                   'EndowmentAssetsEnding')

expenses <- melt(data=dat,
             id=c('unitid',
                       'YearId'),
             measure.vars=c('f2e012', 'f2e013', 'f2e014', 'f2e015', 'f2e016', 'f2e017',
                            'f2e022', 'f2e023', 'f2e024', 'f2e025', 'f2e026', 'f2e027',
                            'f2e032', 'f2e033', 'f2e034', 'f2e035', 'f2e036', 'f2e037',
                            'f2e042', 'f2e043', 'f2e044', 'f2e045', 'f2e046', 'f2e047',
                            'f2e052', 'f2e053', 'f2e054', 'f2e055', 'f2e056', 'f2e057',
                            'f2e062', 'f2e063', 'f2e064', 'f2e065', 'f2e066', 'f2e067',
                            'f2e072', 'f2e073', 'f2e074', 'f2e075', 'f2e076', 'f2e077',
                            'f2e087',
                            'f2e092', 'f2e093', 'f2e094', 'f2e095', 'f2e096', 'f2e097',
                            'f2e102', 'f2e103', 'f2e104', 'f2e105', 'f2e106', 'f2e107',
                            'f2e112', 'f2e113', 'f2e114', 'f2e115', 'f2e116', 'f2e117',
                            'f2e122', 'f2e123', 'f2e124', 'f2e125', 'f2e126', 'f2e127'),
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
                 measure.vars=c('f2c01','f2c02','f2c03','f2c04','f2c05','f2c06'),
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
                 measure.vars=c('f2d012', 'f2d013', 'f2d014',
                                'f2d022', 'f2d023', 'f2d024',
                                'f2d032', 'f2d033', 'f2d034',
                                'f2d042', 'f2d043', 'f2d044',
                                'f2d052', 'f2d053', 'f2d054',
                                'f2d062', 'f2d063', 'f2d064',
                                'f2d072', 'f2d073', 'f2d074',
                                'f2d082a', 'f2d083a', 'f2d084a', 
                                'f2d082b', 'f2d083b', 'f2d084b',
                                'f2d092', 'f2d093', 'f2d094',
                                'f2d102', 'f2d103', 'f2d104',
                                'f2d112', 'f2d122', 'f2d132',
                                'f2d142', 'f2d143', 'f2d144',
                                'f2d152', 'f2d153', 'f2d154',
                                'f2d173', 'f2d174'),
                 variable.name='field',
                 value.name='Revenue')

names(revenues) <- c('Unitid','YearId','field','Revenue')

revenues$RevenueClassificationId <- as.integer(substr(revenues$field, 4, 5)) * 10
revenues$RestrictionTypeId <- as.integer(substr(revenues$field, 6, 6))
revenues$flag <- substr(revenues$field, 7, 7)
revenues$RevenueClassificationId[revenues$flag=='a'] <- revenues$RevenueClassificationId[revenues$flag=='a'] + 1
revenues$RevenueClassificationId[revenues$flag=='b'] <- revenues$RevenueClassificationId[revenues$flag=='b'] + 2
revenues$Revenue[is.na(revenues$Revenue)] <- 0

revenues <- subset(revenues,
                   select=c(Unitid,
                            YearId,
                            RevenueClassificationId,
                            RestrictionTypeId,
                            Revenue))

# Write the outfile.  Empty fields are left null
# writeFile(expenses, expense.outfile)
# writeFile(finpos, finpos.outfile)
# writeFile(aid, aid.outfile)

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
con <-odbcConnect(odbcString)

resp <- sqlQuery(con, paste0("DELETE FROM extract.FASBExpenses WHERE YearId=",as.character(year)))
resp <- sqlQuery(con, paste0("DELETE FROM extract.FASBFinancialPosition WHERE YearId=",as.character(year)))
resp <- sqlQuery(con, paste0("DELETE FROM extract.FASBStudentAid WHERE YearId=",as.character(year)))
resp <- sqlQuery(con, paste0("DELETE FROM extract.FASBRevenues WHERE YearId=",as.character(year)))

# Append values to appropriate tables.  Run ONCE or there will be a key violation
sqlSave(con,
        expenses,
        tablename='extract.FASBExpenses',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

sqlSave(con,
        finpos,
        tablename='extract.FASBFinancialPosition',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

sqlSave(con,
        aid,
        tablename='extract.FASBStudentAid',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

sqlSave(con,
        revenues,
        tablename='extract.FASBRevenues',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

# sqlExecute(con,
#            "EXEC [operations].[AddFASBStudentAid] @Unitid = ?, @YearId = ?, @AidSourceId = ?, @Allowance = ?",
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
