#
# Author: Casey, Jason P.
# Create Date: 01DEC2016
# Description: Reader script for IPEDS Completions, AY1998-1999
# Notes:
# CIP codes are 2000 taxonomy.
# Column classes had to be explicity defined because of periods (.) for missing values.
#

# Load necessary add-ins
library(reshape2)
library(plyr)
library(RODBC)

# clear workspace
rm(list=ls())

# include user functions
source(file="scripts/user-functions.R")

# infile and outfile are set to the input and output file paths
spec1 <- "U:/Data/EX/z_external_data/IPEDS_Completions/1999_00/Final/c2000_a.csv"
outfile <- "C:/Temp/comp_dat2000.csv"
odbcString <- "OSPIR-Dev"

# Assign academic year
year <- 1999L

# Unzip and read the datca
dat <- readFile(spec1)

# Assing YearId value
dat$YearId <- year

# Select six-digit CIP
dat <- subset(dat, nchar(cipcode)==7)

#Convert CIP to numeric
dat$cipcode <- as.numeric(dat$cipcode)

# Rename fields to proper nomenclature
names(dat)[names(dat) == 'unitid'] <- c('Unitid')
names(dat)[names(dat) == 'cipcode'] <- c('ReportedCipCode')
names(dat)[names(dat) == 'awlevel'] <- c('AwardLevelCode')

# Add MajorNumber, set to first major
dat$MajorNumber = 1L

# Recode specific award levels to present taxonomy
dat$AwardLevelCode[dat$AwardLevelCode == 9] <- 17L
dat$AwardLevelCode[dat$AwardLevelCode == 10] <- 18L
dat$AwardLevelCode[dat$AwardLevelCode == 11] <- 6L

# Calculate unknowns to ensure integrity
dat$crace13 <- dat$crace15 - (dat$crace01 + dat$crace03 + dat$crace05 + dat$crace07 + dat$crace09 + dat$crace11) 
dat$crace14 <- dat$crace16 - (dat$crace02 + dat$crace04 + dat$crace06 + dat$crace08 + dat$crace10 + dat$crace12) 

# Subset the data, including only needed variables
dat <- subset(dat,
              AwardLevelCode %in% c(1,2,3,4,5,6,7,8,17,18,19),
              select=c("Unitid",
                           "YearId",
                           "ReportedCipCode",
                           "MajorNumber",
                           "AwardLevelCode",
                           "crace05",
                           "crace06",
                           "crace07",
                           "crace08",
                           "crace03",
                           "crace04",
                           "crace09",
                           "crace10",
                           "crace11",
                           "crace12",
                           "crace13",
                           "crace14",
                           "crace01",
                           "crace02"))

# Convert from wide to long data format
c <- melt(dat, id.vars=c('Unitid','YearId','ReportedCipCode','MajorNumber','AwardLevelCode'), variable.name='Variable',value.name='Awards')  

# Cleanup
rm(dat)

# Cast the Awards field to integer
c$Awards <- as.integer(c$Awards)

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
                             c("crace05",
                               "crace06",
                               "crace07",
                               "crace08",
                               "crace03",
                               "crace04",
                               "crace09",
                               "crace10",
                               "crace11",
                               "crace12",
                               "crace13",
                               "crace14",
                               "crace01",
                               "crace02"),
                             c("A",
                               "A",
                               "O",
                               "O",
                               "B",
                               "B",
                               "S",
                               "S",
                               "C",
                               "C",
                               "U",
                               "U",
                               "N",
                               "N")))

# Assign ND Gender codes to SexCode factor
# F - Female (or Women)
# M - Male (Or Men)
c$SexCode <- factor(mapvalues(c$Variable, 
                                      c("crace05",
                                        "crace06",
                                        "crace07",
                                        "crace08",
                                        "crace03",
                                        "crace04",
                                        "crace09",
                                        "crace10",
                                        "crace11",
                                        "crace12",
                                        "crace13",
                                        "crace14",
                                        "crace01",
                                        "crace02"),
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
                                        "F")))

# Add CipCode Field
c$CipCode <- c$ReportedCipCode

# Remove empty fields and totals
c <- subset(c, Awards > 0 & ReportedCipCode < 99,
            select=c("Unitid",
                     "YearId",
                     "ReportedCipCode", 
                     "CipCode", 
                     "MajorNumber",
                     "AwardLevelCode",
                     "RaceEthnicityCode",
                     "SexCode", 
                     "Awards"))

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
