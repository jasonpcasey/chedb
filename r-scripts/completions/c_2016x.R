#
# Author: Casey, Jason P.
# Create Date: 30NOV2016
# Description: Reader script for IPEDS Completions, AY2014-2015
# Notes:
# CIP codes are 2010 taxonomy.
#

# Load necessary add-ins

# clear workspace
rm(list=ls())

library(odbc)
library(tidyverse)

# access data via net
netLoad <- function(fileUrl, fileName)
{
  #Download data file
  temp <- tempfile()
  
  download.file(fileUrl,
                temp)
  
  data <- read_csv(unz(temp, fileName))
  
  unlink(temp)
  # rm(temp)
  
  names(data) <- tolower(names(data))
  return(data)
}

# "U:/Data/EX/z_external_dapaste0("C:/Temp/comp_dat", year, ".csv")ta/IPEDS_Completions/2014_15/Provisional/c2015_a.csv"
odbcString <- "cdw"

urlStub <- "http://nces.ed.gov/ipeds/datacenter/data/"

# Assign academic year
year <- 2016
archive <- 'c2016_a.zip'
infile <- 'c2016_a.csv'

# Unzip and read the datca
dat <- netLoad(paste0(urlStub, archive), infile)

# Assing YearId value
dat$YearId <- year




c <- dat %>%
  mutate(Unitid = unitid,
         ReportedCipCode = cipcode,
         MajorNumber = majornum,
         AwardLevelCode = awlevel) %>%
  select(Unitid,
         YearId,
         ReportedCipCode,
         MajorNumber,
         AwardLevelCode,
         caianm,
         caianw,
         casiam,
         casiaw,
         cbkaam,
         cbkaaw,
         chispm,
         chispw,
         cnhpim,
         cnhpiw,
         cwhitm,
         cwhitw,
         c2morm,
         c2morw,
         cunknm,
         cunknw,
         cnralm,
         cnralw) %>%
  gather(caianm,
         caianw,
         casiam,
         casiaw,
         cbkaam,
         cbkaaw,
         chispm,
         chispw,
         cnhpim,
         cnhpiw,
         cwhitm,
         cwhitw,
         c2morm,
         c2morw,
         cunknm,
         cunknw,
         cnralm,
         cnralw,
         key = 'variable',
         value = 'Awards') %>%
  separate(variable, c('surv','code'), 1)

  
# Rename fields to proper nomenclature
names(dat)[names(dat) == 'unitid'] <- c('Unitid')
names(dat)[names(dat) == 'cipcode'] <- c('ReportedCipCode')
names(dat)[names(dat) == 'majornum'] <- c('MajorNumber')
names(dat)[names(dat) == 'awlevel'] <- c('AwardLevelCode')

# Subset the data, including only needed variables
dat <- subset(dat,select=c("Unitid",
                           "YearId",
                           "ReportedCipCode",
                           "MajorNumber",
                           "AwardLevelCode",
                           "caianm",
                           "caianw",
                           "casiam",
                           "casiaw",
                           "cbkaam",
                           "cbkaaw",
                           "chispm",
                           "chispw",
                           "cnhpim",
                           "cnhpiw",
                           "cwhitm",
                           "cwhitw",
                           "c2morm",
                           "c2morw",
                           "cunknm",
                           "cunknw",
                           "cnralm",
                           "cnralw"))

# Convert from wide to long data format
c <- melt(dat, id.vars=c('Unitid','YearId','ReportedCipCode','MajorNumber','AwardLevelCode'), variable.name='Variable',value.name='Awards')  

# Cleanup
rm(dat)

# Insert new CipCode column set to equal reported for now.
c$CipCode <- c$ReportedCipCode

# Assign ND Race/Ethnicity codes to RaceEthnicityCode factor
# N - Nonresident Alien
# A - American Indian or Alaska Native
# B - Black or African-American
# O - Asian
# S - Hispanic
# P - Native Hawaiian or Other Pacific Islander
# C - White
# T - Two or More Races
# U - Unknown
c$RaceEthnicityCode <- factor(mapvalues(c$Variable, 
                             c("caianm",
                               "caianw",
                               "casiam",
                               "casiaw",
                               "cbkaam",
                               "cbkaaw",
                               "chispm",
                               "chispw",
                               "cnhpim",
                               "cnhpiw",
                               "cwhitm",
                               "cwhitw",
                               "c2morm",
                               "c2morw",
                               "cunknm",
                               "cunknw",
                               "cnralm",
                               "cnralw"),
                             c("A",
                               "A",
                               "O",
                               "O",
                               "B",
                               "B",
                               "S",
                               "S",
                               "P",
                               "P",
                               "C",
                               "C",
                               "T",
                               "T",
                               "U",
                               "U",
                               "N",
                               "N")))

# Assign ND Gender codes to SexCode factor
# F - Female (or Women)
# M - Male (Or Men)
c$SexCode <- factor(mapvalues(c$Variable, 
                                      c("caianm",
                                        "caianw",
                                        "casiam",
                                        "casiaw",
                                        "cbkaam",
                                        "cbkaaw",
                                        "chispm",
                                        "chispw",
                                        "cnhpim",
                                        "cnhpiw",
                                        "cwhitm",
                                        "cwhitw",
                                        "c2morm",
                                        "c2morw",
                                        "cunknm",
                                        "cunknw",
                                        "cnralm",
                                        "cnralw"),
                                      c("M",
                                        "F",
                                        "M",
                                        "F",
                                        "M",
                                        "F",
                                        "M",
                                        "F",
                                        "M",
                                        "F",
                                        "M",
                                        "F",
                                        "M",
                                        "F",
                                        "M",
                                        "F",
                                        "M",
                                        "F")))

# Remove empty fields and totals
c <- subset(c, Awards > 0 & CipCode < 99, select=c("Unitid","YearId","ReportedCipCode","CipCode","MajorNumber","AwardLevelCode","RaceEthnicityCode","SexCode", "Awards"))

# Write the outfile.  Empty fields are left null
writeFile(c, outfile)

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
con <-odbcConnect(odbcString)

resp <- sqlQuery(con, paste0("DELETE FROM extract.DegreeCompletions WHERE YearId=",as.character(year)))

# Append value to FallEnrollment table.  Run ONCE or there will be a key violation
sqlSave(con,
        c,
        tablename='extract.DegreeCompletions',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

# Execute Extract-to-Staging SPROC
resp <- sqlQuery(con, "EXEC operations.TransferDegreeCompletionsExtractToStagedTable")

# Close the connection to the db
close(con)
con <- NULL
rm(con)
rm(resp)

# Housekeeping
rm(list=ls())

