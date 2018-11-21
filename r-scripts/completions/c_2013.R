#
# Author: Casey, Jason P.
# Create Date: 15MAY2015
# Description: Reader script for IPEDS Completions, AY2012-2013
# Notes:
# CIP codes are 2010 taxonomy.
#

# Load necessary add-ins
library(reshape2)
library(plyr)
library(RODBC)

# infile and outfile are set to the input and output file paths
infile <- "http://nces.ed.gov/ipeds/datacenter/data/C2013_A.zip"
outfile <- "output-data/completions/c_2013_dat.csv"

# Assign academic year
year <- 2013L

# Create a temporary file to hold the zipped datafile
temp <- tempfile()

# Download the zipped file
download.file(infile,temp)

# Unzip and read the data
dat <- read.table(file=unz(temp, 'c2013_a.csv'),
                  header=TRUE,
                  sep=",",
                  quote="\"",
                  skip=0,
                  row.names=NULL,
                  stringsAsFactors=FALSE,
                  fileEncoding="utf-8")

# Cleanup memory
unlink(temp)
rm(infile)
rm(temp)

# Convert names to lower case for consistency
names(dat) <- tolower(names(dat))

# Assing DataYear value
dat$DataYear <- year

# Cleanup
rm(year)

# Rename fields to proper nomenclature
names(dat)[names(dat) == 'unitid'] <- c('Unitid')
names(dat)[names(dat) == 'cipcode'] <- c('Cip')
names(dat)[names(dat) == 'majornum'] <- c('MajorNumber')
names(dat)[names(dat) == 'awlevel'] <- c('AwardLevel')

# Subset the data, including only needed variables
dat <- subset(dat,select=c("Unitid",
                           "DataYear",
                           "Cip",
                           "MajorNumber",
                           "AwardLevel",
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
c <- melt(dat, id.vars=c('Unitid','DataYear','Cip','MajorNumber','AwardLevel'), variable.name='Variable',value.name='Awards')  

# Cleanup
rm(dat)

# Assign ND Race/Ethnicity codes to RaceEthnicity factor
# N - Nonresident Alien
# A - American Indian or Alaska Native
# B - Black or African-American
# O - Asian
# S - Hispanic
# P - Native Hawaiian or Other Pacific Islander
# C - White
# T - Two or More Races
# U - Unknown
c$RaceEthnicity <- factor(mapvalues(c$Variable, 
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

# Assign ND Gender codes to Sex factor
# F - Female (or Women)
# M - Male (Or Men)
c$Sex <- factor(mapvalues(c$Variable, 
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
c <- subset(c, Awards > 0 & Cip < 99, select=c("Unitid","DataYear","Cip","MajorNumber","AwardLevel","RaceEthnicity","Sex", "Awards"))

# Write the outfile.  Empty fields are left null. (use in testing)
write.table(c,
            file=outfile,
            append=FALSE,
            quote=TRUE,
            sep=",",
            row.names=FALSE,       # If you want to create row numbers, set this to TRUE
            col.names=TRUE,
            na = "")               # Set the missing values to blanks

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
con <-odbcConnect("OSPIR-Dev")

# Append value to DegreeCompletions table.  Run ONCE or there will be a key violation
sqlSave(con,
        c,
        tablename='stg.DegreeCompletions',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

# Close the connection to the db
close(con)

# Cleanup memory
rm(con)
rm(outfile)
rm(c)
