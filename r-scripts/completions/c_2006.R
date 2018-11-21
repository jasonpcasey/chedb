#
# Author: Casey, Jason P.
# Create Date: 15MAY2015
# Description: Reader script for IPEDS Completions, AY2005-2006
# Notes:
# CIP codes are 2000 taxonomy.
# Column classes had to be explicity defined because of periods (.) for missing values.
#

# Load necessary add-ins
library(reshape2)
library(plyr)
# library(RODBC)

# infile and outfile are set to the input and output file paths
infile <- "http://nces.ed.gov/ipeds/datacenter/data/C2006_A.zip"
outfile <- "output-data/completions/c_2006_dat.csv"

# Assign academic year
year <- 2006L

# Create temporary file to hold downloaded archived datafile
temp <- tempfile()

# Download the archive
download.file(infile,temp)

# Read the data
dat <- read.table(file=unz(temp, 'c2006_a.csv'),
                  header=TRUE,
                  sep=",",
                  quote="\"",
                  skip=0,
                  colClasses = c('integer',
                                 'character',
                                 'integer',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer',
                                 'character',
                                 'integer'),
                  row.names=NULL,
                  stringsAsFactors=FALSE,
                  fileEncoding="utf-8")

# Cleanup
unlink(temp)
rm(infile)
rm(temp)

# Convert names to lower case for consistency
names(dat) <- tolower(names(dat))

# Assing DataYear value
dat$DataYear <- year

# Cleanup
rm(year)

# Select six-digit CIP
dat <- subset(dat, nchar(cipcode)==7)

#Convert CIP to numeric
dat$cipcode <- as.numeric(dat$cipcode)

# Rename fields to proper nomenclature
names(dat)[names(dat) == 'unitid'] <- c('Unitid')
names(dat)[names(dat) == 'cipcode'] <- c('Cip')
names(dat)[names(dat) == 'majornum'] <- c('MajorNumber')
names(dat)[names(dat) == 'awlevel'] <- c('AwardLevel')

# Recode specific award levels to present taxonomy
dat$AwardLevel[dat$AwardLevel == 9] <- 17L
dat$AwardLevel[dat$AwardLevel == 10] <- 18L
dat$AwardLevel[dat$AwardLevel == 11] <- 6L

# Calculate unknowns to ensure integrity
dat$crace13 <- dat$crace15 - (dat$crace01 + dat$crace03 + dat$crace05 + dat$crace07 + dat$crace09 + dat$crace11) 
dat$crace14 <- dat$crace16 - (dat$crace02 + dat$crace04 + dat$crace06 + dat$crace08 + dat$crace10 + dat$crace12) 

# Subset the data, including only needed variables
dat <- subset(dat,
              AwardLevel %in% c(1,2,3,4,5,6,7,8,17,18,19),
              select=c("Unitid",
                           "DataYear",
                           "Cip",
                           "MajorNumber",
                           "AwardLevel",
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
c <- melt(dat, id.vars=c('Unitid','DataYear','Cip','MajorNumber','AwardLevel'), variable.name='Variable',value.name='Awards')  

# Cleanup
rm(dat)

# Cast the Awards field to integer
c$Awards <- as.integer(c$Awards)

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

# Assign ND Gender codes to Sex factor
# F - Female (or Women)
# M - Male (Or Men)
c$Sex <- factor(mapvalues(c$Variable, 
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

# Add NDCip Field
c$NdCip <- c$Cip

# Remove empty fields and totals
c <- subset(c, Awards > 0 & Cip < 99, select=c("Unitid","DataYear","Cip","MajorNumber","AwardLevel","RaceEthnicity","Sex", "Awards", "NdCip"))

# Write the outfile.  Empty fields are left null.  (use in testing)
write.table(c,
            file=outfile,
            append=FALSE,
            quote=TRUE,
            sep=",",
            row.names=FALSE,       # If you want to create row numbers, set this to TRUE
            col.names=TRUE,
            na = "")               # Set the missing values to blanks

# Open a connection to ipeds database (needs 32-bit ODBC source named ipeds-32 to work)
# con <-odbcConnect("ipeds-32")

# Append value to DegreeCompletions table.  Run ONCE or there will be a key violation
# sqlSave(con, dat=c, tablename='DegreeCompletions', append=TRUE)

# Close the connection to the db
# close(con)

# Cleanup memory
# rm(con)
rm(outfile)
rm(c)
