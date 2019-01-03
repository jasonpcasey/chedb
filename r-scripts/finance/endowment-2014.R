library(reshape2)
library(plyr)

# clear workspace
rm(list=ls())

# include user functions
source(file="scripts/user-functions.R")

# infile and outfile are set to the input and output file paths
spec1 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2013_14/Provisional/f1314_f2.csv"
spec2 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2013_14/Provisional/f1314_f1a.csv"
outfile <- "C:/Temp/end_dat2014.csv"
odbcString <- "OSPIR-DEV"

# Year is set to ND's academic year, same as fall year
year <- 2013L

dat <- readFile(spec1)
df <- readFile(spec2)

# Assign data year
dat$YearId <- year
df$YearId <- year

dat <- subset(dat,
              f2h02 >= 0,
              select=c('unitid',
                       'YearId',
                       'f2h01',
                       'f2h02'))

df <- subset(df,
             f1h02 >= 0,
             select=c('unitid',
                      'YearId',
                      'f1h01',
                      'f1h02'))

names(dat) <- c('Unitid','YearId','EndowmentBOY','EndowmentEOY')
names(df) <- c('Unitid','YearId','EndowmentBOY','EndowmentEOY')

# Combine frames
endow <- rbind(dat, df)

# Cleanup memory
rm(dat)
rm(df)

# Fix missing
# endow$EndowmentBOY[is.na(endow$EndowmentBOY)] <- 0
# endow$EndowmentEOY[is.na(endow$EndowmentEOY)] <- 0

# Write the outfile.  Empty fields are left null
writeFile(endow, outfile)

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
con <-odbcConnect(odbcString)

resp <- sqlQuery(con, paste0("DELETE FROM extract.Endowment WHERE YearId=",as.character(year)))

# Append value to FallEnrollment table.  Run ONCE or there will be a key violation
sqlSave(con,
        endow,
        tablename='extract.Endowment',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

# Execute Extract-to-Staging SPROC
# resp <- sqlQuery(con, "EXEC operations.TransferEndowmentExtractToStagedTable")

# Close the connection to the db
close(con)
con <- NULL
rm(con)
rm(resp)

# Housekeeping
rm(list=ls())
