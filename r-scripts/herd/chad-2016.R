#
# Author: Casey, Jason P.
# Create Date: 01DEC2016
# Description: Reader script for NASF fields from NSF Facilities
# Notes:
# 
#

# clear the workspace
rm(list=ls())

# include user functions
source(file="scripts/user-functions.R")

# set these for later use
spec <- 'U:/Data/EX/z_external_data/NSF_RD/EX014/2016/ND2016.xlsx'
year <- 2015
odbcString <- "OSPIR-DEV"
outfile1 <- paste0("C:/Temp/rd-detail-chad", year, ".csv")
outfile2 <- paste0("C:/Temp/rd-inst-chad", year, ".csv")

dat <- read_excel(spec, skip = 3)

names(dat) <- c('detail',
                'division',
                'broad',
                'broad2',
                'field',
                'usda',
                'dod',
                'energy',
                'hhs',
                'nasa',
                'nsf',
                'otherfed',
                'fed',
                'state',
                'business',
                'nonprofit',
                'institution',
                'othernon',
                'totnon',
                'total',
                'dud1',
                'dud2',
                'dud3')

dat$RdFice <- 1840L
dat$YearId <- year

dat$NsfDetailedAcademicFieldCode <- 'X'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Aeronautical/Astronautical'] <- 'A1'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Bioengineering/Biomedical eng.'] <- 'A2'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Chemical'] <- 'A3'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Civil'] <- 'A4'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Electrical'] <- 'A5'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Mechanical'] <- 'A6'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Metallurgical/Materials'] <- 'A7'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Other engineering'] <- 'A8'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Astronomy'] <- 'B1'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Chemistry'] <- 'B2'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Physics'] <- 'B3'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Other physical sciences'] <- 'B4'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Atmospheric sciences'] <- 'C1'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Earth sciences'] <- 'C2'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Oceanography'] <- 'C3'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Other environmental sciences'] <- 'C4'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Mathematical Sciences'] <- 'D '
dat$NsfDetailedAcademicFieldCode[dat$detail=='Computer Sciences'] <- 'E '
dat$NsfDetailedAcademicFieldCode[dat$detail=='Agricultural sciences'] <- 'F1'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Biological Sciences'] <- 'F2'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Medical sciences'] <- 'F3'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Other life sciences'] <- 'F4'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Psychology'] <- 'G '
dat$NsfDetailedAcademicFieldCode[dat$detail=='Economics'] <- 'H1'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Political science'] <- 'H2'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Sociology'] <- 'H3'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Other social sciences'] <- 'H4'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Other Sciences'] <- 'I '
dat$NsfDetailedAcademicFieldCode[dat$detail=='Education'] <- 'J1'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Law'] <- 'J2'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Humanities'] <- 'J3'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Visual and performing arts'] <- 'J4'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Business and management'] <- 'J5'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Communication, journalism, and library science'] <- 'J6'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Social Work'] <- 'J7'
dat$NsfDetailedAcademicFieldCode[dat$detail=='Other non-S&E fields'] <- 'J8'

detail <- subset(dat,
                 select=c(RdFice,
                          YearId,
                          NsfDetailedAcademicFieldCode,
                          usda,
                          dod,
                          energy,
                          hhs,
                          nasa,
                          nsf,
                          otherfed,
                          state,
                          business,
                          nonprofit,
                          institution,
                          othernon))

detail <- melt(detail,
               id.vars=c('RdFice',
                         'YearId',
                         'NsfDetailedAcademicFieldCode'),
               variable.name='Variable',
               value.name='Expenditures')

detail$FundingSourceCode <- 0
detail$FundingSourceCode[detail$Variable=='usda'] <- 101
detail$FundingSourceCode[detail$Variable=='dod'] <- 102
detail$FundingSourceCode[detail$Variable=='energy'] <- 103
detail$FundingSourceCode[detail$Variable=='hhs'] <- 104
detail$FundingSourceCode[detail$Variable=='nasa'] <- 105
detail$FundingSourceCode[detail$Variable=='nsf'] <- 106
detail$FundingSourceCode[detail$Variable=='otherfed'] <- 107
detail$FundingSourceCode[detail$Variable=='state'] <- 201
detail$FundingSourceCode[detail$Variable=='business'] <- 202
detail$FundingSourceCode[detail$Variable=='nonprofit'] <- 203
detail$FundingSourceCode[detail$Variable=='institution'] <- 204
detail$FundingSourceCode[detail$Variable=='othernon'] <- 205

detail$FundingTypeCode <- 2L
detail$FundingTypeCode[detail$FundingSourceCode < 200 & detail$FundingSourceCode > 0] <- 1L

detail$ScienceEngineeringField <- 1
detail$ScienceEngineeringField[detail$NsfDetailedAcademicFieldCode %in% c('J1','J2','J3','J4','J5','J6','J7','J8','X')] <- 0

detail$IsExternal <- 1
detail$IsExternal[detail$FundingSourceCode %in% c(204,0)] <- 0

detail$CapitalExpenditures <- NA

detail <- subset(detail,
                 Expenditures > 0,
                 select=c(RdFice,
                          YearId,
                          FundingTypeCode,
                          FundingSourceCode,
                          NsfDetailedAcademicFieldCode,
                          ScienceEngineeringField,
                          IsExternal,
                          Expenditures,
                          CapitalExpenditures))

head(detail, 10)

detail$Expenditures <- detail$Expenditures * 1000
detail$CapitalExpenditures <- detail$CapitalExpenditures * 1000

# Write the outfile.  Empty fields are left null
writeFile(detail, outfile1)

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
con <-odbcConnect(odbcString)

resp <- sqlQuery(con, paste0("DELETE FROM extract.NsfRdExpendituresDetail WHERE YearId=",as.character(year)))

# Append value to NsfRdExpendituresDetail table.  Run ONCE or there will be a key violation
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
resp <- sqlQuery(con, "EXEC operations.TransferNsfRdExpendituresDetailToStagedTable")

# Close the connection to the db
close(con)
con <- NULL
rm(con)
rm(resp)

# Housekeeping
# rm(list=ls())

inst <- subset(dat,
               select=c(RdFice,
                        YearId,
                        NsfDetailedAcademicFieldCode,
                        total,
                        fed,
                        state,
                        business,
                        nonprofit,
                        institution,
                        othernon))

inst$total[is.na(inst$total)] <- 0
inst$fed[is.na(inst$fed)] <- 0
inst$state[is.na(inst$state)] <- 0
inst$business[is.na(inst$business)] <- 0
inst$nonprofit[is.na(inst$nonprofit)] <- 0
inst$institution[is.na(inst$institution)] <- 0
inst$othernon[is.na(inst$othernon)] <- 0

inst$total <- inst$total * 1000
inst$fed <- inst$fed * 1000
inst$state <- inst$state * 1000
inst$business <- inst$business * 1000
inst$nonprofit <- inst$nonprofit * 1000
inst$institution <- inst$institution * 1000
inst$othernon <- inst$othernon * 1000

inst$ScienceEngineeringExpenditures <- 0
inst$ScienceEngineeringExpenditures[!inst$NsfDetailedAcademicFieldCode %in% c('J1','J2','J3','J4','J5','J6','J7','J8','X')] <- inst$total[!inst$NsfDetailedAcademicFieldCode %in% c('J1','J2','J3','J4','J5','J6','J7','J8','X')]

inst$NonScienceEngineeringExpenditures <- 0
inst$NonScienceEngineeringExpenditures[inst$NsfDetailedAcademicFieldCode %in% c('J1','J2','J3','J4','J5','J6','J7','J8','X')] <- inst$total[inst$NsfDetailedAcademicFieldCode %in% c('J1','J2','J3','J4','J5','J6','J7','J8','X')]

inst$ExternalFunds <- inst$total - inst$institution

# Aggregate the data on keyed fields
inst <- ddply(inst,.(RdFice,
                   YearId),
              summarise,
              TotalExpenditures=sum(total),
              ScienceEngineeringExpenditures=sum(ScienceEngineeringExpenditures),
              NonScienceEngineeringExpenditures=sum(NonScienceEngineeringExpenditures),
              FederalFunds=sum(fed),
              StateLocalFunds=sum(state),
              BusinessFunds=sum(business),
              NonprofitFunds=sum(nonprofit),
              InstitutionalFunds=sum(institution),
              OtherFunds=sum(othernon),
              ExternalFunds=sum(ExternalFunds))

inst$ForeignFunds <- NA
inst$Contracts <- NA
inst$Grants <- NA
inst$MedicalSchoolExpenditures <- 0
inst$FederalHumanClinicalTrialsExpenditures <- NA
inst$NonFederalHumanClinicalTrialsExpenditures <- NA
inst$TotalBasicResearchExpenditures <- NA
inst$FederalBasicResearchExpenditures <- NA
inst$NonFederalBasicResearchExpenditures <- NA
inst$TotalAppliedResearchExpenditures <- NA
inst$FederalAppliedResearchExpenditures <- NA
inst$NonFederalAppliedResearchExpenditures <- NA
inst$TotalDevelopmentExpenditures <- NA
inst$FederalDevelopmentExpenditures <- NA
inst$NonFederalDevelopmentExpenditures <- NA
inst$SalariesWagesFringeBenefitsExpenditures <- NA
inst$SoftwareExpenditures <- NA
inst$CapitalizedEquipmentExpenditures <- NA
inst$PassThroughExpenditures <- NA
inst$OtherDirectCostsExpenditures <- NA
inst$IndirectCostsExpenditures <- NA
inst$ARRAFunds <- NA
inst$PrincipalInvestigatorsHeadcount <- NA
inst$OtherPersonnelHeadcount <- NA
inst$TotalPersonnelHeadcount <- NA
inst$PostdocsHeadcount <- NA

head(inst, 10)

# Write the outfile.  Empty fields are left null
writeFile(inst, outfile2)

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
con <-odbcConnect(odbcString)

resp <- sqlQuery(con, paste0("DELETE FROM extract.NsfRdExpendituresInstitution WHERE YearId=",as.character(year)))

# Append value to NsfRdExpendituresInstitution table.  Run ONCE or there will be a key violation
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
resp <- sqlQuery(con, "EXEC operations.TransferNsfRdExpendituresInstitutionToStagedTable")

# Close the connection to the db
close(con)
con <- NULL
rm(con)
rm(resp)

# Housekeeping
# rm(list=ls())

