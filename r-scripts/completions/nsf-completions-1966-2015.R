#
# Author: Casey, Jason P.
# Create Date: 01DEC2016
# Description: Reader script for Bacc Origins from NSF SED
# Notes:
# 
#

# clear the workspace
rm(list=ls())

# include user functions
source(file="scripts/user-functions.R")

# set these for later use
spec <- 'U:/Data/EX/z_external_data/NSF_bac_origins/nsf-comp.csv'

odbcString <- "OSPIR-DEV"
outfile <- paste0("C:/Temp/nsf-comp.csv")

dat <- read_csv(spec,
                 col_names = c('YearId',
                               'institution',
                               'RdFice',
                               'field',
                               'level',
                               'Awards'))


dat$YearId <- as.integer(dat$YearId)
dat <- subset(dat,
              YearId > 0)
dat$YearId <- dat$YearId - 1

dat$Awards <- as.integer(dat$Awards)
dat$field <- tolower(dat$field)
dat$level <- tolower(dat$level)

dat$Awards[is.na(dat$Awards)] <- 0

dat$RdFice <- as.integer(dat$RdFice)
dat$RdFice[is.na(dat$RdFice)] <- 0

dat <- subset(dat,
              RdFice > 9)

# dat$SexCode <- 'U'
# dat$SexCode[dat$sex=='Female'] <- 'F'
# dat$SexCode[dat$sex=='Male'] <- 'M'

# dat$RaceEthnicityCode <- 'U'
# dat$RaceEthnicityCode[dat$ethnic=='American Indian/Alaska Native'] <- 'A'
# dat$RaceEthnicityCode[dat$ethnic=='Asian'] <- 'O'
# dat$RaceEthnicityCode[dat$ethnic=='Black/African American (non-Hispanic/Latino)'] <- 'B'
# dat$RaceEthnicityCode[dat$ethnic=='Hispanic/Latino'] <- 'S'
# dat$RaceEthnicityCode[dat$ethnic=='More than one race (non-Hispanic/Latino)'] <- 'T'
# dat$RaceEthnicityCode[dat$ethnic=='Native Hawaiian/Other Pacific Islander'] <- 'P'
# dat$RaceEthnicityCode[dat$ethnic=='Not Available'] <- 'U'
# dat$RaceEthnicityCode[dat$ethnic=='Temporary Visa Holders'] <- 'N'
# dat$RaceEthnicityCode[dat$ethnic=='Unknown race/ethnicity or not stated'] <- 'U'
# dat$RaceEthnicityCode[dat$ethnic=='White (non-Hispanic/Latino)'] <- 'C'
# dat$RaceEthnicityCode[dat$citizenship=='Temporary Residents'] <- 'N'

dat$SedDetailedAcademicFieldCode <- '00'
dat$SedDetailedAcademicFieldCode[dat$field=='aerospace engineering'] <- '11'
dat$SedDetailedAcademicFieldCode[dat$field=='chemical engineering'] <- '12'
dat$SedDetailedAcademicFieldCode[dat$field=='civil engineering'] <- '13'
dat$SedDetailedAcademicFieldCode[dat$field=='electrical engineering'] <- '14'
dat$SedDetailedAcademicFieldCode[dat$field=='mechanical engineering'] <- '15'
dat$SedDetailedAcademicFieldCode[dat$field=='materials engineering'] <- '16'
dat$SedDetailedAcademicFieldCode[dat$field=='industrial engineering'] <- '17'
dat$SedDetailedAcademicFieldCode[dat$field=='other engineering'] <- '19'
dat$SedDetailedAcademicFieldCode[dat$field=='astronomy'] <- '21'
dat$SedDetailedAcademicFieldCode[dat$field=='chemistry'] <- '22'
dat$SedDetailedAcademicFieldCode[dat$field=='physics'] <- '23'
dat$SedDetailedAcademicFieldCode[dat$field=='other physical sciences'] <- '29'
dat$SedDetailedAcademicFieldCode[dat$field=='atmospheric sciences'] <- '31'
dat$SedDetailedAcademicFieldCode[dat$field=='earth sciences'] <- '32'
dat$SedDetailedAcademicFieldCode[dat$field=='oceanography'] <- '33'
dat$SedDetailedAcademicFieldCode[dat$field=='other geosciences'] <- '39'
dat$SedDetailedAcademicFieldCode[dat$field=='mathematics and statistics'] <- '41'
dat$SedDetailedAcademicFieldCode[dat$field=='computer science'] <- '42'
dat$SedDetailedAcademicFieldCode[dat$field=='other math sciences'] <- '49'
dat$SedDetailedAcademicFieldCode[dat$field=='agricultural sciences'] <- '51'
dat$SedDetailedAcademicFieldCode[dat$field=='biological sciences'] <- '52'
dat$SedDetailedAcademicFieldCode[dat$field=='medical sciences'] <- '53'
dat$SedDetailedAcademicFieldCode[dat$field=='other life sciences'] <- '59'
dat$SedDetailedAcademicFieldCode[dat$field=='psychology'] <- '60'
dat$SedDetailedAcademicFieldCode[dat$field=='economics'] <- '71'
dat$SedDetailedAcademicFieldCode[dat$field=='political science and public administration'] <- '72'
dat$SedDetailedAcademicFieldCode[dat$field=='sociology'] <- '73'
dat$SedDetailedAcademicFieldCode[dat$field=='anthropology'] <- '74'
dat$SedDetailedAcademicFieldCode[dat$field=='linguistics'] <- '75'
dat$SedDetailedAcademicFieldCode[dat$field=='history of science'] <- '76'
dat$SedDetailedAcademicFieldCode[dat$field=='area and ethnic studies'] <- '77'
dat$SedDetailedAcademicFieldCode[dat$field=='other social sciences'] <- '79'
dat$SedDetailedAcademicFieldCode[dat$field=='science technologies'] <- '81'
dat$SedDetailedAcademicFieldCode[dat$field=='engineering technologies'] <- '82'
dat$SedDetailedAcademicFieldCode[dat$field=='health technologies'] <- '83'
dat$SedDetailedAcademicFieldCode[dat$field=='other science and engineering technologies'] <- '89'
dat$SedDetailedAcademicFieldCode[dat$field=='interdisciplinary or other sciences'] <- '99'
dat$SedDetailedAcademicFieldCode[dat$field=='history'] <- 'B1'
dat$SedDetailedAcademicFieldCode[dat$field=='english and literature'] <- 'B2'
dat$SedDetailedAcademicFieldCode[dat$field=='foreign languages'] <- 'B3'
dat$SedDetailedAcademicFieldCode[dat$field=='other humanities'] <- 'B9'
dat$SedDetailedAcademicFieldCode[dat$field=='religion and theology'] <- 'D0'
dat$SedDetailedAcademicFieldCode[dat$field=='arts and music'] <- 'E0'
dat$SedDetailedAcademicFieldCode[dat$field=='architecture and environmental design'] <- 'F0'
dat$SedDetailedAcademicFieldCode[dat$field=='science education'] <- 'G1'
dat$SedDetailedAcademicFieldCode[dat$field=='mathematics education'] <- 'G2'
dat$SedDetailedAcademicFieldCode[dat$field=='social science education'] <- 'G3'
dat$SedDetailedAcademicFieldCode[dat$field=='other science/technical education'] <- 'G4'
dat$SedDetailedAcademicFieldCode[dat$field=='non-science education'] <- 'G5'
dat$SedDetailedAcademicFieldCode[dat$field=='business and management'] <- 'H0'
dat$SedDetailedAcademicFieldCode[dat$field=='communication and librarianship'] <- 'J0'
dat$SedDetailedAcademicFieldCode[dat$field=='law'] <- 'K0'
dat$SedDetailedAcademicFieldCode[dat$field=='social service professions'] <- 'L0'
dat$SedDetailedAcademicFieldCode[dat$field=='vocational studies and home economics'] <- 'M0'
dat$SedDetailedAcademicFieldCode[dat$field=='other non-sciences or unknown disciplines'] <- 'Z9'

dat$AwardLevelCode[dat$level=="1 but less than 2 year certificates"] <- 2
dat$AwardLevelCode[dat$level=="1 but less than 4 year certificates"] <- 2
dat$AwardLevelCode[dat$level=="2 but less than 4 year certificates"] <- 4
dat$AwardLevelCode[dat$level=="associate's degrees"] <- 3
dat$AwardLevelCode[dat$level=="bachelor's degrees"] <- 5
dat$AwardLevelCode[dat$level=="doctorate degree-other"] <- 19
dat$AwardLevelCode[dat$level=="doctorate degree-professional practice"] <- 18
dat$AwardLevelCode[dat$level=="doctorate degree-research/scholarship"] <- 17
dat$AwardLevelCode[dat$level=="doctorate degrees"] <- 17
dat$AwardLevelCode[dat$level=="first professional certificates (post-degree)"] <- 8
dat$AwardLevelCode[dat$level=="first professional degrees"] <- 18
dat$AwardLevelCode[dat$level=="less than 1 year certificates"] <- 1
dat$AwardLevelCode[dat$level=="master's degrees"] <- 7
dat$AwardLevelCode[dat$level=="post-baccalaureate certificates"] <- 6
dat$AwardLevelCode[dat$level=="post-master's certificates"] <- 8

dat$RdFice <- factor(dat$RdFice)
dat$YearId <- factor(dat$YearId)
dat$SedDetailedAcademicFieldCode <- factor(dat$SedDetailedAcademicFieldCode)

# Aggregate the data on keyed fields
dat <- ddply(dat,
             .(RdFice,
               YearId,
               SedDetailedAcademicFieldCode,
               AwardLevelCode),
             summarise,
             Awards=sum(Awards))

# save a reference copy
writeFile(dat, outfile)

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
# con <-odbcConnect(odbcString)

# Delete old values
# params <- data.frame(charges$Unitid, charges$YearId)

# delete.query <- paste0("DELETE FROM extract.NsfBaccOrigins WHERE YearId = ", year)
# delete.query <- "DELETE FROM extract.NsfBaccOrigins"

# resp <- sqlExecute(channel = con,
#                    query = delete.query,
#                    fetch = FALSE)

# Append new values.
# sqlSave(con,
#         dat,
#         tablename='extract.NsfBaccOrigins',
#         append=TRUE,
#         rownames=FALSE,
#         verbose=TRUE,
#         safer=TRUE,
#         fast=TRUE,
#         test=FALSE)

# Execute Extract-to-Staging SPROC
# resp <- sqlExecute(con,
#                    query="EXEC operations.TransferNsfBaccOriginsExtractToStagedTable",
#                    fetch = FALSE)

# Close the connection to the db
# close(con)
# con <- NULL

# Housekeeping
# rm(list=ls())
