#
# Author: Casey, Jason P.
# Create Date: 16JAN2017
# Description: Reader script for NSF RD
# Notes:
# 
#

# clear the workspace
rm(list=ls())

# include user functions
# source(file="scripts/user-functions.R")
library(odbc)
library(readxl)
library(haven)
library(tidyverse)

# exp2015 <- read_sas("https://www.nsf.gov/statistics/herd/data/exp2015.sas7bdat", 
#                     NULL)

# assign academic year
year <- 2015L

# infile and outfile are set to the input and output file paths
spec <- "https://www.nsf.gov/statistics/herd/data/exp2015.csv"
#spec <- "https://www.nsf.gov/statistics/herd/data/exp2015.xlsx"

# odbcString <- "OSPIR-DEV"

outfile <- paste0("C:/Temp/herdi", year, ".csv")

# read institutional identifiers
# Open a connection to ipeds database (needs ODBC source named ipeds to work)
# con <-odbcConnect(odbcString)

# inst.bridge <- sqlExecute(channel = con,
#                    query = 'SELECT RDFICE as RdFice, NsfSurveyId FROM staged.NsfIpedsBridge',
#                    fetch = TRUE)

# Close the connection to the db
# close(con)
# con <- NULL

# read the data
dat <- read.csv(spec,
                stringsAsFactors = FALSE)
dat <- as_data_frame(dat) %>%
  mutate(data = ifelse(is.na(data), 0, data * 1000))

head(dat)

# assign zeroes to missing to simplify calculations
dat$data[is.na(dat$data)] <- 0

# Assing YearId value
dat$YearId <- year



# assign/fix institutional identifiers
dat$NsfSurveyId <- as.integer(as.character(dat$inst_id))

dat$NsfSurveyId[dat$NsfSurveyId == 4508] <- 674508
dat$NsfSurveyId[dat$NsfSurveyId == 102045] <- 11678
dat$NsfSurveyId[dat$NsfSurveyId == 102044] <- 10674
dat$NsfSurveyId[dat$NsfSurveyId == 100116] <- 10116
dat$NsfSurveyId[dat$NsfSurveyId == 102023] <- 100159
dat$NsfSurveyId[dat$NsfSurveyId == 102059] <- 8764
dat$NsfSurveyId[dat$NsfSurveyId == 102007] <- 3652

dat <- merge(inst.bridge,
             dat,
             by="NsfSurveyId")

dat$questionnaire_no <- tolower(dat$questionnaire_no)
dat$row <- tolower(dat$row)
dat$column <- tolower(dat$column)

# subset and rename
rd <- subset(dat,
             select=c(NsfSurveyId,
                      RdFice,
                      YearId,
                      questionnaire_no,
                      row,
                      column,
                      data))

rd$data <- rd$data * 1000

# cleanup
rm(dat)
rm(inst.bridge)



# add audit code here

NoFice <- rd[is.na(rd$RdFice)]
head(NoFice)
rm(NoFice)


# create institutional data file
inst <- rd

inst$TotalExpenditures <- 0
inst$ScienceEngineeringExpenditures <- 0
inst$NonScienceEngineeringExpenditures <- 0
inst$FederalFunds <- 0
inst$StateLocalFunds <- 0
inst$BusinessFunds <- 0
inst$NonprofitFunds <- 0
inst$InstitutionalFunds <- 0
inst$OtherFunds <- 0
inst$ExternalFunds <- 0
inst$ForeignFunds <- 0
inst$Contracts <- 0
inst$Grants <- 0
inst$MedicalSchoolExpenditures <- 0
inst$FederalHumanClinicalTrialsExpenditures <- 0
inst$NonFederalHumanClinicalTrialsExpenditures <- 0
inst$TotalBasicResearchExpenditures <- 0
inst$FederalBasicResearchExpenditures <- 0
inst$NonFederalBasicResearchExpenditures <- 0
inst$TotalAppliedResearchExpenditures <- 0
inst$FederalAppliedResearchExpenditures <- 0
inst$NonFederalAppliedResearchExpenditures <- 0
inst$TotalDevelopmentExpenditures <- 0
inst$FederalDevelopmentExpenditures <- 0
inst$NonFederalDevelopmentExpenditures <- 0
inst$SalariesWagesFringeBenefitsExpenditures <- 0
inst$SoftwareExpenditures <- 0
inst$CapitalizedEquipmentExpenditures <- 0
inst$PassThroughExpenditures <- 0
inst$OtherDirectCostsExpenditures <- 0
inst$IndirectCostsExpenditures <- 0
inst$ARRAFunds <- 0
inst$PrincipalInvestigatorsHeadcount <- 0
inst$OtherPersonnelHeadcount <- 0
inst$TotalPersonnelHeadcount <- 0
inst$PostdocsHeadcount <- 0

inst$TotalExpenditures[inst$questionnaire_no=='01.g'] <- inst$data[inst$questionnaire_no=='01.g']
inst$ScienceEngineeringExpenditures[inst$questionnaire_no %in% c('09a9',
                                                                '09b5',
                                                                '09c5',
                                                                '09d',
                                                                '09e',
                                                                '09f5',
                                                                '09g',
                                                                '09h5',
                                                                '09i',
                                                                '11a1',
                                                                '11a3',
                                                                '11a4',
                                                                '11a5',
                                                                '11a7',
                                                                '11a9',
                                                                '11b2',
                                                                '11b3',
                                                                '11b5',
                                                                '11d',
                                                                '11e',
                                                                '11f2',
                                                                '11f5',
                                                                '11g',
                                                                '11h1',
                                                                '11h2',
                                                                '11h3',
                                                                '11h4',
                                                                '11h5',
                                                                '11i') &
                                          inst$column=='total'] <- inst$data[inst$questionnaire_no %in% c('09a9',
                                                                                           '09b5',
                                                                                           '09c5',
                                                                                           '09d',
                                                                                           '09e',
                                                                                           '09f5',
                                                                                           '09g',
                                                                                           '09h5',
                                                                                           '09i',
                                                                                           '11a1',
                                                                                           '11a3',
                                                                                           '11a4',
                                                                                           '11a5',
                                                                                           '11a7',
                                                                                           '11a9',
                                                                                           '11b2',
                                                                                           '11b3',
                                                                                           '11b5',
                                                                                           '11d',
                                                                                           '11e',
                                                                                           '11f2',
                                                                                           '11f5',
                                                                                           '11g',
                                                                                           '11h1',
                                                                                           '11h2',
                                                                                           '11h3',
                                                                                           '11h4',
                                                                                           '11h5',
                                                                                           '11i') &
                                                                     inst$column=='total']

inst$NonScienceEngineeringExpenditures[inst$questionnaire_no %in% c('09j9',
                                                                    '11j9') &
                                      inst$column=='total'] <- inst$data[inst$questionnaire_no %in% c('09j9',
                                                                                                      '11j9') &
                                                                           inst$column=='total']

inst$FederalFunds[inst$questionnaire_no=='01.a'] <- inst$data[inst$questionnaire_no=='01.a']
inst$StateLocalFunds[inst$questionnaire_no=='01.b'] <- inst$data[inst$questionnaire_no=='01.b']
inst$BusinessFunds[inst$questionnaire_no=='01.c'] <- inst$data[inst$questionnaire_no=='01.c']
inst$NonprofitFunds[inst$questionnaire_no=='01.d'] <- inst$data[inst$questionnaire_no=='01.d']
inst$InstitutionalFunds[inst$questionnaire_no=='01.e'] <- inst$data[inst$questionnaire_no=='01.e']
inst$OtherFunds[inst$questionnaire_no=='01.f'] <- inst$data[inst$questionnaire_no=='01.f']

inst$ExternalFunds[inst$questionnaire_no=='03' & inst$row=='total'] <- inst$data[inst$questionnaire_no=='03' & inst$row=='total']

inst$ForeignFunds[inst$questionnaire_no=='02'] <- inst$data[inst$questionnaire_no=='02']

inst$Contracts[inst$questionnaire_no=='03' & inst$row=='contracts'] <- inst$data[inst$questionnaire_no=='03' & inst$row=='contracts']
inst$Grants[inst$questionnaire_no=='03' & inst$row=='grants, reimbursements, and other agreements'] <- inst$data[inst$questionnaire_no=='03' & inst$row=='grants, reimbursements, and other agreements']

inst$MedicalSchoolExpenditures[inst$questionnaire_no=='04'] <- inst$data[inst$questionnaire_no=='04']

inst$FederalHumanClinicalTrialsExpenditures[inst$questionnaire_no=='05' & inst$row=='federal'] <- inst$data[inst$questionnaire_no=='05' & inst$row=='federal']
inst$NonFederalHumanClinicalTrialsExpenditures[inst$questionnaire_no=='05' & inst$row=='nonfederal'] <- inst$data[inst$questionnaire_no=='05' & inst$row=='nonfederal']

inst$TotalBasicResearchExpenditures[inst$questionnaire_no=='06.a' & inst$column=='total'] <- inst$data[inst$questionnaire_no=='06.a' & inst$column=='total']
inst$FederalBasicResearchExpenditures[inst$questionnaire_no=='06.a' & inst$column=='federal'] <- inst$data[inst$questionnaire_no=='06.a' & inst$column=='federal']
inst$NonFederalBasicResearchExpenditures[inst$questionnaire_no=='06.a' & inst$column=='nonfederal'] <- inst$data[inst$questionnaire_no=='06.a' & inst$column=='nonfederal']

inst$TotalAppliedResearchExpenditures[inst$questionnaire_no=='06.b' & inst$column=='total'] <- inst$data[inst$questionnaire_no=='06.b' & inst$column=='total']
inst$FederalAppliedResearchExpenditures[inst$questionnaire_no=='06.b' & inst$column=='federal'] <- inst$data[inst$questionnaire_no=='06.b' & inst$column=='federal']
inst$NonFederalAppliedResearchExpenditures[inst$questionnaire_no=='06.b' & inst$column=='nonfederal'] <- inst$data[inst$questionnaire_no=='06.b' & inst$column=='nonfederal']

inst$TotalDevelopmentExpenditures[inst$questionnaire_no=='06.c' & inst$column=='total'] <- inst$data[inst$questionnaire_no=='06.c' & inst$column=='total']
inst$FederalDevelopmentExpenditures[inst$questionnaire_no=='06.c' & inst$column=='federal'] <- inst$data[inst$questionnaire_no=='06.c' & inst$column=='federal']
inst$NonFederalDevelopmentExpenditures[inst$questionnaire_no=='06.c' & inst$column=='nonfederal'] <- inst$data[inst$questionnaire_no=='06.c' & inst$column=='nonfederal']

inst$SalariesWagesFringeBenefitsExpenditures[inst$questionnaire_no=='12.a'] <- inst$data[inst$questionnaire_no=='12.a']
inst$SoftwareExpenditures[inst$questionnaire_no=='12.b'] <- inst$data[inst$questionnaire_no=='12.b']
inst$CapitalizedEquipmentExpenditures[inst$questionnaire_no=='12.c'] <- inst$data[inst$questionnaire_no=='12.c']
inst$PassThroughExpenditures[inst$questionnaire_no=='12.d'] <- inst$data[inst$questionnaire_no=='12.d']
inst$OtherDirectCostsExpenditures[inst$questionnaire_no=='12.e'] <- inst$data[inst$questionnaire_no=='12.e']
inst$IndirectCostsExpenditures[inst$questionnaire_no=='12.f'] <- inst$data[inst$questionnaire_no=='12.f']

inst$PrincipalInvestigatorsHeadcount[inst$questionnaire_no=='15' & inst$row=='principal investigators'] <- inst$data[inst$questionnaire_no=='15' & inst$row=='principal investigators']/1000
inst$OtherPersonnelHeadcount[inst$questionnaire_no=='15' & inst$row=='other personnel'] <- inst$data[inst$questionnaire_no=='15' & inst$row=='other personnel']/1000
inst$TotalPersonnelHeadcount[inst$questionnaire_no=='15' & inst$row=='total'] <- inst$data[inst$questionnaire_no=='15' & inst$row=='total']/1000
inst$PostdocsHeadcount[inst$questionnaire_no=='16'] <- inst$data[inst$questionnaire_no=='16']/1000

# inst.data <- ddply(rd, c('RdFice', 'YearId'), summarize,
#                        Mean=mean(rating),
#                        sd=sd(rating),
#                        n=sum(!is.na(rating)),
#                        se=sd/sqrt(n))
# 
# rm.data.stats$ci <- rm.data.stats$se * qt(.975, rm.data.stats$n - 1)

inst <- ddply(inst, c('RdFice', 'YearId'), summarize,
                   TotalExpenditures=sum(TotalExpenditures),
                   ScienceEngineeringExpenditures=sum(ScienceEngineeringExpenditures),
                   NonScienceEngineeringExpenditures=sum(NonScienceEngineeringExpenditures),
                   FederalFunds=sum(FederalFunds),
                   StateLocalFunds=sum(StateLocalFunds),
                   BusinessFunds=sum(BusinessFunds),
                   NonprofitFunds=sum(NonprofitFunds),
                   InstitutionalFunds=sum(InstitutionalFunds),
                   OtherFunds=sum(OtherFunds),
                   ExternalFunds=sum(ExternalFunds),
                   ForeignFunds=sum(ForeignFunds),
                   Contracts=sum(Contracts),
                   Grants=sum(Grants),
                   MedicalSchoolExpenditures=sum(MedicalSchoolExpenditures),
                   FederalHumanClinicalTrialsExpenditures=sum(FederalHumanClinicalTrialsExpenditures),
                   NonFederalHumanClinicalTrialsExpenditures=sum(NonFederalHumanClinicalTrialsExpenditures),
                   TotalBasicResearchExpenditures=sum(TotalBasicResearchExpenditures),
                   FederalBasicResearchExpenditures=sum(FederalBasicResearchExpenditures),
                   NonFederalBasicResearchExpenditures=sum(NonFederalBasicResearchExpenditures),
                   TotalAppliedResearchExpenditures=sum(TotalAppliedResearchExpenditures),
                   FederalAppliedResearchExpenditures=sum(FederalAppliedResearchExpenditures),
                   NonFederalAppliedResearchExpenditures=sum(NonFederalAppliedResearchExpenditures),
                   TotalDevelopmentExpenditures=sum(TotalDevelopmentExpenditures),
                   FederalDevelopmentExpenditures=sum(FederalDevelopmentExpenditures),
                   NonFederalDevelopmentExpenditures=sum(NonFederalDevelopmentExpenditures),
                   SalariesWagesFringeBenefitsExpenditures=sum(SalariesWagesFringeBenefitsExpenditures),
                   SoftwareExpenditures=sum(SoftwareExpenditures),
                   CapitalizedEquipmentExpenditures=sum(CapitalizedEquipmentExpenditures),
                   PassThroughExpenditures=sum(PassThroughExpenditures),
                   OtherDirectCostsExpenditures=sum(OtherDirectCostsExpenditures),
                   IndirectCostsExpenditures=sum(IndirectCostsExpenditures),
                   ARRAFunds=sum(ARRAFunds),
                   PrincipalInvestigatorsHeadcount=sum(PrincipalInvestigatorsHeadcount),
                   OtherPersonnelHeadcount=sum(OtherPersonnelHeadcount),
                   TotalPersonnelHeadcount=sum(TotalPersonnelHeadcount),
                   PostdocsHeadcount=sum(PostdocsHeadcount))


# Write the outfile.  Empty fields are left null
writeFile(inst, outfile)

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
con <-odbcConnect(odbcString)

# Delete old values
delete.query <- paste0("DELETE FROM extract.NsfRdExpendituresInstitution WHERE YearId = ", year)

resp <- sqlExecute(channel = con,
                   query = delete.query,
                   fetch = FALSE)

# Append new values.
sqlSave(con,
        inst,
        tablename='extract.NsfRdExpendituresInstitution',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

# Execute Extract-to-Staging SPROC
resp <- sqlExecute(con,
                   query="EXEC operations.TransferNsfRdExpendituresInstitutionToStagedTable",
                   fetch = FALSE)

# Close the connection to the db
close(con)
con <- NULL
rm(con)

# Housekeeping
rm(inst)

# create detail data
detail <- rd
outfile <- paste0("C:/Temp/herdd", year, ".csv")

detail$NsfDetailedAcademicFieldCode <- toupper(substr(detail$questionnaire_no, 3, 4))
detail$RowNum <- as.integer(substr(detail$questionnaire_no, 1, 2))

detail <- subset(detail,
                 NsfDetailedAcademicFieldCode %in% c('A1',
                                                     'A2',
                                                     'A3',
                                                     'A4',
                                                     'A5',
                                                     'A6',
                                                     'A7',
                                                     'A8',
                                                     'B1',
                                                     'B2',
                                                     'B3',
                                                     'B4',
                                                     'C1',
                                                     'C2',
                                                     'C3',
                                                     'C4',
                                                     'D',
                                                     'E',
                                                     'F1',
                                                     'F2',
                                                     'F3',
                                                     'F4',
                                                     'G',
                                                     'H1',
                                                     'H2',
                                                     'H3',
                                                     'H4',
                                                     'I',
                                                     'J1',
                                                     'J2',
                                                     'J3',
                                                     'J4',
                                                     'J5',
                                                     'J6',
                                                     'J7',
                                                     'J8') &
                   column %in% c('dod',
                                 'doe',
                                 'hhs',
                                 'nasa',
                                 'nsf',
                                 'other agencies',
                                 'usda',
                                 'all other sources',
                                 'business',
                                 'institution funds',
                                 'nonprofit organziations',
                                 'state and local government',
                                 'federal',
                                 'nonfederal'))


detail$FundingTypeCode <- -1L
detail$FundingTypeCode[detail$RowNum==9] <- 1L
detail$FundingTypeCode[detail$RowNum==11] <- 2L
detail$FundingTypeCode[detail$RowNum==14 & detail$column=='federal'] <- 1L
detail$FundingTypeCode[detail$RowNum==14 & detail$column=='nonfederal'] <- 2L

detail$FundingSourceCode <- 0
detail$FundingSourceCode[detail$column=='dod'] <- 102L
detail$FundingSourceCode[detail$column=='doe'] <- 103L
detail$FundingSourceCode[detail$column=='hhs'] <- 104L
detail$FundingSourceCode[detail$column=='nasa'] <- 105L
detail$FundingSourceCode[detail$column=='nsf'] <- 106L
detail$FundingSourceCode[detail$column=='other agencies'] <- 107L
detail$FundingSourceCode[detail$column=='usda'] <- 101L
detail$FundingSourceCode[detail$column=='all other sources'] <- 205L
detail$FundingSourceCode[detail$column=='business'] <- 202L
detail$FundingSourceCode[detail$column=='institution funds'] <- 204L
detail$FundingSourceCode[detail$column=='nonprofit organziations'] <- 203L
detail$FundingSourceCode[detail$column=='state and local government'] <- 201L

detail$ScienceEngineeringField <- 0
detail$ScienceEngineeringField[detail$FundingSourceCode > 0 & detail$FundingSourceCode < 200] <- 1

detail$IsExternal <- 1
detail$IsExternal[detail$FundingSourceCode==204L] <- 0

detail$Expenditures <- 0
detail$Expenditures[detail$RowNum < 14] <- detail$data[detail$RowNum < 14]

detail$CapitalExpenditures <- 0
detail$CapitalExpenditures[detail$RowNum==14] <- detail$data[detail$RowNum==14]

detail <- ddply(detail, c('RdFice',
                          'YearId',
                          'FundingTypeCode',
                          'FundingSourceCode',
                          'NsfDetailedAcademicFieldCode',
                          'ScienceEngineeringField',
                          'IsExternal'),
                summarize,
                Expenditures=sum(Expenditures),
                CapitalExpenditures=sum(CapitalExpenditures))


# detail <- subset(detail,
#                  Expenditures > 0 | CapitalExpenditures > 0)

# Write the outfile.  Empty fields are left null
writeFile(detail, outfile)

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
con <-odbcConnect(odbcString)

# Delete old values
delete.query <- paste0("DELETE FROM extract.NsfRdExpendituresDetail WHERE YearId = ", year)

resp <- sqlExecute(channel = con,
                   query = delete.query,
                   fetch = FALSE)

# Append new values.
sqlSave(con,
        detail,
        tablename='extract.NsfRdExpendituresDetail',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

# Execute Extract-to-Staging SPROC
resp <- sqlExecute(con,
                   query="EXEC operations.TransferNsfRdExpendituresDetailToStagedTable",
                   fetch = FALSE)

# Close the connection to the db
close(con)
con <- NULL
rm(con)

# housekeeping
# rm(list=ls())
