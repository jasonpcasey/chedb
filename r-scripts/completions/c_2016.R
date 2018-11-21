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

# include user functions
source(file="scripts/user-functions.R")

# Assign academic year
year <- 2015

# infile and outfile are set to the input and output file paths
spec1 <- "U:/Data/EX/z_external_data/IPEDS_Completions/2015_16/Provisional/c2016_a_dist.sav"
outfile <- paste0("C:/Temp/comp_dat", year, ".csv")
odbcString <- "OSPIR-DEV"

c <- readSPSSFile(spec1) %>%
  select(unitid,
         cipcode,
         majornum,
         awlevel,
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
         cnralw, key = 'variable', value = 'Awards') %>%
  # filter(Awards > 0) %>%
  mutate(Unitid = unitid,
         YearId = year,
         ReportedCipCode = cipcode,
         CipCode = cipcode,
         MajorNumber = majornum,
         AwardLevelCode = awlevel,
         RaceEthnicityCode = recode(variable,
                                    'caianm'='A',
                                    'caianw'='A',
                                    'casiam'='O',
                                    'casiaw'='O',
                                    'cbkaam'='B',
                                    'cbkaaw'='B',
                                    'chispm'='S',
                                    'chispw'='S',
                                    'cnhpim'='P',
                                    'cnhpiw'='P',
                                    'cwhitm'='C',
                                    'cwhitw'='C',
                                    'c2morm'='T',
                                    'c2morw'='T',
                                    'cunknm'='U',
                                    'cunknw'='U',
                                    'cnralm'='N',
                                    'cnralw'='N'),
         SexCode = recode(variable,
                          'caianm'='M',
                          'caianw'='F',
                          'casiam'='M',
                          'casiaw'='F',
                          'cbkaam'='M',
                          'cbkaaw'='F',
                          'chispm'='M',
                          'chispw'='F',
                          'cnhpim'='M',
                          'cnhpiw'='F',
                          'cwhitm'='M',
                          'cwhitw'='F',
                          'c2morm'='M',
                          'c2morw'='F',
                          'cunknm'='M',
                          'cunknw'='F',
                          'cnralm'='M',
                          'cnralw'='F')) %>%
  select(Unitid,
         YearId,
         ReportedCipCode,
         CipCode,
         MajorNumber,
         AwardLevelCode,
         RaceEthnicityCode,
         SexCode,
         Awards)
  
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

