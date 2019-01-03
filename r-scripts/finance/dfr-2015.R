library(reshape2)
library(plyr)

# clear workspace
rm(list=ls())

# include user functions
source(file="scripts/user-functions.R")

# infile and outfile are set to the input and output file paths
spec <- c("C:/Users/jcasey4/Downloads/fin/CSV_3312017-837.csv",
          "C:/Users/jcasey4/Downloads/fin/CSV_3312017-145.csv",
          "C:/Users/jcasey4/Downloads/fin/CSV_3312017-702.csv",
          "C:/Users/jcasey4/Downloads/fin/CSV_3312017-807.csv",
          "C:/Users/jcasey4/Downloads/fin/CSV_3312017-163.csv",
          "C:/Users/jcasey4/Downloads/fin/CSV_3312017-239.csv",
          "C:/Users/jcasey4/Downloads/fin/CSV_3312017-109.csv",
          "C:/Users/jcasey4/Downloads/fin/CSV_3312017-776.csv",
          "C:/Users/jcasey4/Downloads/fin/CSV_3312017-460.csv",
          "C:/Users/jcasey4/Downloads/fin/CSV_3312017-985.csv")

outfile <- "C:/Temp/dfr.csv"
odbcString <- "OSPIR-DEV"

dat <- data_frame()

for (i in 1:length(spec))
{
  df <- readFile(spec[i])
  dat <- rbind.fill(dat, df)
}


# publics
dfr <- subset(dat,
              !is.na(dat[[4]]),
              select=c(1:10,
                       25:34,
                       55,
                       57))

names(dfr) <- c('Unitid',
                'InstitutionName',
                'YearId',
                'InstructionExpensesPerFte',
                'ResearchExpensesPerFte',
                'PublicServiceExpensesPerFte',
                'AcademicSupportExpensesPerFte',
                'StudentServiceExpensesPerFte',
                'InstitutionalSupportExpensesPerFte',
                'AllOtherCoreExpensesPerFte',
                'SalariesWagesAndBenefitExpensesForCoreExpensesAsAPercentOfTotalCoreExpenses',
                'SalariesWagesAndBenefitExpensesForInstructionAsAPercentOfInstructionExpenses',
                'SalariesWagesAndBenefitExpensesForResearchAsAPercentOfResearchExpenses',
                'SalariesWagesAndBenefitExpensesForPublicServiceAsAPercentOfPublicServiceExpenses',
                'SalariesWagesAndBenefitExpensesForAcademicSupportAsAPercentOfAcademicSupportExpenses',
                'SalariesWagesAndBenefitExpensesForStudentServicesAsAPercentOfStudentServicesExpenses',
                'SalariesWagesAndBenefitExpensesForInstitutionalSupportAsAPercentOfInstitutionalSupportExpenses',
                'SalariesWagesAndBenefitExpensesForOtherCoreExpenseFunctionsAsAPercentOfOtherCoreFunctionsExpenses',
                'TotalSalariesWagesAndBenefitExpensesAsAPercentOfTotalExpenses',
                'TotalSalariesAndWageExpensesAsAPercentOfTotalExpenses',
                'EndowmentAssetsYearEnd',
                'AcademicInstitutionalSupportStudentServicesExpensesPerFte')

# non-profits
df <- subset(dat,
             !is.na(dat[[11]]),
             select=c(1:3,
                      11:17,
                      35:44,
                      56,
                      57))

names(df) <- c('Unitid',
                'InstitutionName',
                'YearId',
                'InstructionExpensesPerFte',
                'ResearchExpensesPerFte',
                'PublicServiceExpensesPerFte',
                'AcademicSupportExpensesPerFte',
                'StudentServiceExpensesPerFte',
                'InstitutionalSupportExpensesPerFte',
                'AllOtherCoreExpensesPerFte',
                'SalariesWagesAndBenefitExpensesForCoreExpensesAsAPercentOfTotalCoreExpenses',
                'SalariesWagesAndBenefitExpensesForInstructionAsAPercentOfInstructionExpenses',
                'SalariesWagesAndBenefitExpensesForResearchAsAPercentOfResearchExpenses',
                'SalariesWagesAndBenefitExpensesForPublicServiceAsAPercentOfPublicServiceExpenses',
                'SalariesWagesAndBenefitExpensesForAcademicSupportAsAPercentOfAcademicSupportExpenses',
                'SalariesWagesAndBenefitExpensesForStudentServicesAsAPercentOfStudentServicesExpenses',
                'SalariesWagesAndBenefitExpensesForInstitutionalSupportAsAPercentOfInstitutionalSupportExpenses',
                'SalariesWagesAndBenefitExpensesForOtherCoreExpenseFunctionsAsAPercentOfOtherCoreFunctionsExpenses',
                'TotalSalariesWagesAndBenefitExpensesAsAPercentOfTotalExpenses',
                'TotalSalariesAndWageExpensesAsAPercentOfTotalExpenses',
                'EndowmentAssetsYearEnd',
                'AcademicInstitutionalSupportStudentServicesExpensesPerFte')

dfr <- rbind(dfr, df)

# for-profits
df <- subset(dat,
             !is.na(dat[[18]]),
             select=c(1:3,
                      18:24,
                      45:54,
                      56,
                      57))

names(df) <- c('Unitid',
               'InstitutionName',
               'YearId',
               'InstructionExpensesPerFte',
               'ResearchExpensesPerFte',
               'PublicServiceExpensesPerFte',
               'AcademicSupportExpensesPerFte',
               'StudentServiceExpensesPerFte',
               'InstitutionalSupportExpensesPerFte',
               'AllOtherCoreExpensesPerFte',
               'SalariesWagesAndBenefitExpensesForCoreExpensesAsAPercentOfTotalCoreExpenses',
               'SalariesWagesAndBenefitExpensesForInstructionAsAPercentOfInstructionExpenses',
               'SalariesWagesAndBenefitExpensesForResearchAsAPercentOfResearchExpenses',
               'SalariesWagesAndBenefitExpensesForPublicServiceAsAPercentOfPublicServiceExpenses',
               'SalariesWagesAndBenefitExpensesForAcademicSupportAsAPercentOfAcademicSupportExpenses',
               'SalariesWagesAndBenefitExpensesForStudentServicesAsAPercentOfStudentServicesExpenses',
               'SalariesWagesAndBenefitExpensesForInstitutionalSupportAsAPercentOfInstitutionalSupportExpenses',
               'SalariesWagesAndBenefitExpensesForOtherCoreExpenseFunctionsAsAPercentOfOtherCoreFunctionsExpenses',
               'TotalSalariesWagesAndBenefitExpensesAsAPercentOfTotalExpenses',
               'TotalSalariesAndWageExpensesAsAPercentOfTotalExpenses',
               'EndowmentAssetsYearEnd',
               'AcademicInstitutionalSupportStudentServicesExpensesPerFte')

dfr <- rbind(dfr, df)
rm(df)
rm(dat)

# adjust percentages
dfr$SalariesWagesAndBenefitExpensesForCoreExpensesAsAPercentOfTotalCoreExpenses <- dfr$SalariesWagesAndBenefitExpensesForCoreExpensesAsAPercentOfTotalCoreExpenses/100
dfr$SalariesWagesAndBenefitExpensesForInstructionAsAPercentOfInstructionExpenses <- dfr$SalariesWagesAndBenefitExpensesForInstructionAsAPercentOfInstructionExpenses/100
dfr$SalariesWagesAndBenefitExpensesForResearchAsAPercentOfResearchExpenses <- dfr$SalariesWagesAndBenefitExpensesForResearchAsAPercentOfResearchExpenses/100
dfr$SalariesWagesAndBenefitExpensesForPublicServiceAsAPercentOfPublicServiceExpenses <- dfr$SalariesWagesAndBenefitExpensesForPublicServiceAsAPercentOfPublicServiceExpenses/100
dfr$SalariesWagesAndBenefitExpensesForAcademicSupportAsAPercentOfAcademicSupportExpenses <- dfr$SalariesWagesAndBenefitExpensesForAcademicSupportAsAPercentOfAcademicSupportExpenses/100
dfr$SalariesWagesAndBenefitExpensesForStudentServicesAsAPercentOfStudentServicesExpenses <- dfr$SalariesWagesAndBenefitExpensesForStudentServicesAsAPercentOfStudentServicesExpenses/100
dfr$SalariesWagesAndBenefitExpensesForInstitutionalSupportAsAPercentOfInstitutionalSupportExpenses <- dfr$SalariesWagesAndBenefitExpensesForInstitutionalSupportAsAPercentOfInstitutionalSupportExpenses/100
dfr$SalariesWagesAndBenefitExpensesForOtherCoreExpenseFunctionsAsAPercentOfOtherCoreFunctionsExpenses <- dfr$SalariesWagesAndBenefitExpensesForOtherCoreExpenseFunctionsAsAPercentOfOtherCoreFunctionsExpenses/100
dfr$TotalSalariesWagesAndBenefitExpensesAsAPercentOfTotalExpenses <- dfr$TotalSalariesWagesAndBenefitExpensesAsAPercentOfTotalExpenses/100
dfr$TotalSalariesAndWageExpensesAsAPercentOfTotalExpenses <- dfr$TotalSalariesAndWageExpensesAsAPercentOfTotalExpenses/100

# Write the outfile.  Empty fields are left null
writeFile(dfr, outfile)

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
# con <-odbcConnect(odbcString)

# resp <- sqlQuery(con, paste0("DELETE FROM extract.FASBExpenses WHERE YearId=",as.character(year)))

# Append value to FallEnrollment table.  Run ONCE or there will be a key violation
# sqlSave(con,
#         fasb,
#         tablename='extract.FASBExpenses',
#         append=TRUE,
#         rownames=FALSE,
#         verbose=TRUE,
#         safer=TRUE,
#         fast=TRUE,
#         test=FALSE)

# Execute Extract-to-Staging SPROC
# resp <- sqlQuery(con, "EXEC operations.TransferFASBExpensesExtractToStagedTable")

# Close the connection to the db
# close(con)
# con <- NULL
# rm(con)
# rm(resp)

# Housekeeping
# rm(list=ls())
