library(reshape2)
library(plyr)

# clear workspace
rm(list=ls())

# include user functions
source(file="scripts/user-functions.R")

# infile and outfile are set to the input and output file paths
spec <- c("C:/Users/jcasey4/Downloads/fin/full/CSV_4152017-83.csv",
          "C:/Users/jcasey4/Downloads/fin/full/CSV_4152017-141.csv",
          "C:/Users/jcasey4/Downloads/fin/full/CSV_4152017-214.csv",
          "C:/Users/jcasey4/Downloads/fin/full/CSV_4152017-322.csv",
          "C:/Users/jcasey4/Downloads/fin/full/CSV_4152017-413.csv",
          "C:/Users/jcasey4/Downloads/fin/full/CSV_4152017-433.csv",
          "C:/Users/jcasey4/Downloads/fin/full/CSV_4152017-503.csv",
          "C:/Users/jcasey4/Downloads/fin/full/CSV_4152017-808.csv",
          "C:/Users/jcasey4/Downloads/fin/full/CSV_4152017-809.csv",
          "C:/Users/jcasey4/Downloads/fin/full/CSV_4152017-877.csv")

outfile <- "C:/Temp/dfr.csv"
odbcString <- "OSPIR-DEV"

dat <- data_frame()

for (i in 1:length(spec))
{
  df <- readFile(spec[i])
  year <- df$year[1]
  names(df) <- gsub(paste0('^drvf',year,'.'),'',names(df))
  
  dat <- rbind.fill(dat, df)
  rm(df, year)
}

header <- dat[,1:3]
names(header) <- c('Unitid','InstitutionName','FiscalYear')

# find sector-specific fields for each sector
pnames <- names(dat)[grep('(gasb)',names(dat))]
nnames <- names(dat)[grep('(fasb)',names(dat))]
fnames <- names(dat)[grep('profit',names(dat))]

pub <- cbind(header, subset(dat,
                            select=pnames))
pri <- cbind(header, subset(dat,
                            select=nnames))
pro <- cbind(header, subset(dat,
                            select=fnames))

rm(header)

pub <- subset(pub,
              !is.na(pub[,4]))
pri <- subset(pri,
              !is.na(pri[,4]))
pro <- subset(pro,
              !is.na(pro[,4]))

current <- c('Unitid',
             'InstitutionName',
             'FiscalYear',
             'core revenues, total dollars (gasb)',
             'tuition and fees as a percent of core revenues (gasb)',
             'state appropriations as percent of core revenues  (gasb)',
             'local appropriations as a percent of core revenues (gasb)',
             'government grants and contracts as a percent of core revenues (gasb)',
             'private gifts, grants, and contracts as a percent of core revenues (gasb)',
             'investment return as a percent of core revenues (gasb)',
             'other revenues as a percent of core revenues (gasb)',
             'core revenues, total dollars (fasb)',
             'tuition and fees as a percent of core revenues (fasb)',
             'government grants and contracts as a percent of core revenues (fasb)',
             'private gifts, grants, contracts/contributions from affiliated entities as a percent of core revenues (fasb)',
             'investment return as a percent of core revenues (fasb)',
             'other revenues as a percent of core revenues (fasb)',
             'core revenues, total dollars (for-profit institutions)',
             'tuition and fees as a percent of core revenues (for-profit institutions)',
             'government appropriations, grants, and contracts as a percent of core revenues (for-profit institutions)',
             'private gifts, grants, contracts as a percent of core revenues (for-profit institutions)',
             'investment return as a percent of core revenues (for-profit institutions)',
             'sales and services of educational activities as a percent of core revenues (for-profit institutions)',
             'other revenues as a percent of core revenues (for-profit institutions)',
             'revenues from tuition and fees per fte (gasb)',
             'revenues from state appropriations per fte (gasb)',
             'revenues from local appropriations per fte (gasb)',
             'revenues from government grants and contracts per fte (gasb)',
             'revenues from private gifts, grants, and contracts per fte (gasb)',
             'revenues from investment return per fte (gasb)',
             'other core revenues per fte (gasb)',
             'revenues from tuition and fees per fte (fasb)',
             'revenues from government grants and contracts per fte (fasb)',
             'revenues from private gifts, grants, contracts/contributions from affiliated entities per fte (fasb)',
             'revenues from investment return per fte (fasb)',
             'other core revenues per fte (fasb)',
             'revenues from tuition and fees per fte (for-profit institutions)',
             'revenues from government appropriations, grants and contracts per fte (for-profit institutions)',
             'revenues from private gifts, grants, contracts per fte (for-profit institutions)',
             'revenues from investment return per fte (for-profit institutions)',
             'revenues from sales and services of educational activities per fte  (for-profit institutions)',
             'other core revenues per fte (for-profit institutions)',
             'core expenses, total dollars (gasb)',
             'instruction expenses as a percent of total core expenses (gasb)',
             'research expenses as a percent of total core expenses (gasb)',
             'public service expenses as a percent of total core expenses (gasb)',
             'academic support expenses as a percent of total core expenses (gasb)',
             'student service expenses as a percent of total core expenses (gasb)',
             'institutional support expenses as a percent of total core expenses (gasb)',
             'other core expenses as a percent of total core expenses (gasb)',
             'core expenses, total dollars (fasb)',
             'instruction expenses as a percent of total core expenses (fasb)',
             'research expenses as a percent of total core expenses (fasb)',
             'public service expenses as a percent of total core expenses (fasb)',
             'academic support expenses as a percent of total core expenses (fasb)',
             'student service expenses as a percent of total core expenses (fasb)',
             'institutional support expenses as a percent of total core expenses (fasb)',
             'other core expenses as a percent of total core expenses (fasb)',
             'core expenses, total dollars (for-profit institutons)',
             'instruction expenses as a percent of total core expenses (for-profit institutions)',
             'research expenses as a percent of total core expenses (for-profit institutions)',
             'public service expenses as a percent of total core expenses (for-profit institutions)',
             'academic support expenses as a percent of total core expenses (for-profit institutions)',
             'student service expenses as a percent of total core expenses (for-profit institutions)',
             'institutional support expenses as a percent of total core expenses (for-profit institutions)',
             'other core expenses as a percent of total  core expenses (for-profit institutions)',
             'instruction expenses per fte  (gasb)',
             'research expenses per fte  (gasb)',
             'public service expenses per fte (gasb)',
             'academic support expenses per fte (gasb)',
             'student service expenses per fte (gasb)',
             'institutional support expenses per fte (gasb)',
             'all other core expenses per fte (gasb)',
             'instruction expenses per fte  (fasb)',
             'research expenses per fte (fasb)',
             'public service expenses per fte (fasb)',
             'academic support expenses per fte (fasb)',
             'student service expenses per fte (fasb)',
             'institutional support expenses per fte (fasb)',
             'all other core expenses per fte (fasb)',
             'instruction expenses per fte (for-profit institutions)',
             'research expenses per fte (for-profit institutions)',
             'public service expenses per fte (for-profit institutions)',
             'academic support expenses per fte (for-profit institutions)',
             'student service expenses per fte (for-profit institutions)',
             'institutional support expenses per fte (for-profit institutions)',
             'all other core expenses per fte (for-profit institutions)',
             'salaries, wages, and benefit expenses for core expenses as a percent of total core expenses (gasb)',
             'salaries, wages, and benefit expenses for instruction as a percent of total expenses for instruction (gasb)',
             'salaries, wages, and benefit expenses for research as a percent of total expenses for research (gasb)',
             'salaries, wages, and benefit expenses for public service as a percent of total expenses for public service (gasb)',
             'salaries, wages, and benefit expenses for academic support as a percent of total expenses for academic support (gasb)',
             'salaries, wages, and benefit expenses for student services as a percent of total expenses for student services (gasb)',
             'salaries, wages, and benefit expenses for institutional support as a percent of total expenses for institutional support (gasb)',
             'salaries, wages, and benefit expenses for other core expense functions  as a percent of total expenses for other core expense functions (gasb)',
             'total salaries, wages, and benefit expenses as a percent of total expenses (gasb)',
             'total salaries and wage expenses as a percent of total expenses (gasb)',
             'salaries, wages, and benefit expenses for core expenses as a percent of total core expenses (fasb)',
             'salaries, wages, and benefit expenses for instruction as a percent of total expenses for instruction (fasb)',
             'salaries, wages, and benefit expenses for research as a percent of total expenses for research (fasb)',
             'salaries, wages, and benefit expenses for public service as a percent of total expenses for public service (fasb)',
             'salaries, wages, and benefit expenses for academic support as a percent of total expenses for academic support (fasb)',
             'salaries, wages, and benefit expenses for student services as a percent of total expenses for student services (fasb)',
             'salaries, wages, and benefit expenses for institutional support as a percent of total expenses for institutional support (fasb)',
             'salaries, wages, and benefit expenses for other core expense functions  as a percent of total expenses for other core expense functions (fasb)',
             'total salaries, wages, and benefit expenses as a percent of total expenses (fasb)',
             'total salaries and wage expenses as a percent of total expenses (fasb)',
             'salaries, wages, and benefit expenses for core expenses as a percent of total core expenses (for-profit institutions)',
             'salaries, wages, and benefit expenses for instruction as a percent of total expenses for instruction (for-profit institutions)',
             'salaries, wages, and benefit expenses for research as a percent of total expenses for research (for-profit institutions)',
             'salaries, wages, and benefit expenses for public service as a percent of total expenses for public service (for-profit institutions)',
             'salaries, wages, and benefit expenses for academic support as a percent of total expenses for academic support (for-profit institutions)',
             'salaries, wages, and benefit expenses for student services as a percent of total expenses for student services (for-profit institutions)',
             'salaries, wages, and benefit expenses for institutional support as a percent of total expenses for institutional support (for-profit institutions)',
             'salaries, wages, and benefit expenses for other core expense functions  as a percent of total expenses for other core expense functions (for-profit)',
             'total salaries, wages, and benefit expenses as a percent of total expenses (for-profit institutions)',
             'total salaries and wage expenses as a percent of total expenses (for-profit institutions)',
             'endowment assets (year end) per fte enrollment (gasb)',
             'endowment assets (year end) per fte enrollment (fasb)',
             'equity ratio (gasb)',
             'equity ratio (fasb)',
             'equity ratio (for-profit institutions)',
             'private gifts, grants, and contracts as a percent of core revenues (fasb)',
             'revenues from private gifts, grants, and contracts per fte (fasb)',
             'private gifts, grants,contracts/contributions from affiliated entities as a percent of core revenues (fasb)',
             'revenues from private gifts, grants,contracts/contributions from affiliated entities per fte (fasb)',
             'government grants and contracts as a percent of core revenues (for-profit institutions)',
             'revenues from government grants and contracts per fte (for-profit institutions)',
             'academic and institutional support, and student service expenses as a percent of total core expenses (for-profit institutions)',
             'academic and institutional support, and student services  expense per fte (for-profit institutions)')

replacement <- c('Unitid',
                 'InstitutionName',
                 'FiscalYear',
                 'CoreRevenues',
                 'TuitionAsPctCoreRevenues',
                 'StateAppropriationsAsPctCoreRevenues',
                 'LocalAppropriationsAsPctCoreRevenues',
                 'GovtGrantsAndContractsAsPctCoreRevenues',
                 'PrivateGiftsAndContractsAsPctCoreRevenues',
                 'InvestmentReturnAsPctCoreRevenues',
                 'OtherRevenuesAsPctCoreRevenues',
                 'CoreRevenues',
                 'TuitionAsPctCoreRevenues',
                 'GovtGrantsAndContractsAsPctCoreRevenues',
                 'PrivateGiftsAndContractsAsPctCoreRevenues',
                 'InvestmentReturnAsPctCoreRevenues',
                 'OtherRevenuesAsPctCoreRevenues',
                 'CoreRevenues',
                 'TuitionAsPctCoreRevenues',
                 'GovtGrantsAndContractsAsPctCoreRevenues',
                 'PrivateGiftsAndContractsAsPctCoreRevenues',
                 'InvestmentReturnAsPctCoreRevenues',
                 'SalesEducActivitiesAsPctCoreRevenues',
                 'OtherRevenuesAsPctCoreRevenues',
                 'TuitionRevenuePerFte',
                 'StateAppropriationsRevenuePerFte',
                 'LocalAppropriationsRevenuePerFte',
                 'GovtGrantsAndContractsRevenuePerFte',
                 'PrivateGiftsAndContractsRevenuePerFte',
                 'InvestmentReturnRevenuePerFte',
                 'OtherCoreRevenuePerFte',
                 'TuitionRevenuePerFte',
                 'GovtGrantsAndContractsRevenuePerFte',
                 'PrivateGiftsAndContractsRevenuePerFte',
                 'InvestmentReturnRevenuePerFte',
                 'OtherCoreRevenuePerFte',
                 'TuitionRevenuePerFte',
                 'GovtGrantsAndContractsRevenuePerFte',
                 'PrivateGiftsAndContractsRevenuePerFte',
                 'InvestmentReturnRevenuePerFte',
                 'SalesEducActivitiesRevenuePerFte',
                 'OtherCoreRevenuePerFte',
                 'CoreExpenses',
                 'InstructionAsPctCoreExpenses',
                 'ResearchAsPctCoreExpenses',
                 'PublicServiceAsPctCoreExpenses',
                 'AcademicSupportAsPctCoreExpenses',
                 'StudentServiceAsPctCoreExpenses',
                 'InstitutionalSupportAsPctCoreExpenses',
                 'OtherCoreAsPctCoreExpenses',
                 'CoreExpenses',
                 'InstructionAsPctCoreExpenses',
                 'ResearchAsPctCoreExpenses',
                 'PublicServiceAsPctCoreExpenses',
                 'AcademicSupportAsPctCoreExpenses',
                 'StudentServiceAsPctCoreExpenses',
                 'InstitutionalSupportAsPctCoreExpenses',
                 'OtherCoreAsPctCoreExpenses',
                 'CoreExpenses',
                 'InstructionAsPctCoreExpenses',
                 'ResearchAsPctCoreExpenses',
                 'PublicServiceAsPctCoreExpenses',
                 'AcademicSupportAsPctCoreExpenses',
                 'StudentServiceAsPctCoreExpenses',
                 'InstitutionalSupportAsPctCoreExpenses',
                 'OtherCoreAsPctCoreExpenses',
                 'InstructionExpensesPerFte',
                 'ResearchExpensesPerFte',
                 'PublicServiceExpensesPerFte',
                 'AcademicSupportExpensesPerFte',
                 'StudentServiceExpensesPerFte',
                 'InstitutionalSupportExpensesPerFte',
                 'OtherCoreExpensesPerFte',
                 'InstructionExpensesPerFte',
                 'ResearchExpensesPerFte',
                 'PublicServiceExpensesPerFte',
                 'AcademicSupportExpensesPerFte',
                 'StudentServiceExpensesPerFte',
                 'InstitutionalSupportExpensesPerFte',
                 'OtherCoreExpensesPerFte',
                 'InstructionExpensesPerFte',
                 'ResearchExpensesPerFte',
                 'PublicServiceExpensesPerFte',
                 'AcademicSupportExpensesPerFte',
                 'StudentServiceExpensesPerFte',
                 'InstitutionalSupportExpensesPerFte',
                 'OtherCoreExpensesPerFte',
                 'CoreExpensesSalariesAndBenefitsAsPctCoreExpenses',
                 'InstructionSalariesAndBenefitsAsPctCoreExpenses',
                 'ResearchSalariesAndBenefitsAsPctCoreExpenses',
                 'PublicServiceSalariesAndBenefitsAsPctCoreExpenses',
                 'AcademicSupportSalariesAndBenefitsAsPctCoreExpenses',
                 'StudentServicesSalariesAndBenefitsAsPctCoreExpenses',
                 'InstitutionalSupportSalariesAndBenefitsAsPctCoreExpenses',
                 'OtherCoreExpensesSalariesAndBenefitsAsPctCoreExpenses',
                 'TotalSalariesAndBenefitsAsPctTotalExpenses',
                 'TotalSalariesAndWagesAsPctTotalExpenses',
                 'CoreExpensesSalariesAndBenefitsAsPctCoreExpenses',
                 'InstructionSalariesAndBenefitsAsPctCoreExpenses',
                 'ResearchSalariesAndBenefitsAsPctCoreExpenses',
                 'PublicServiceSalariesAndBenefitsAsPctCoreExpenses',
                 'AcademicSupportSalariesAndBenefitsAsPctCoreExpenses',
                 'StudentServicesSalariesAndBenefitsAsPctCoreExpenses',
                 'InstitutionalSupportSalariesAndBenefitsAsPctCoreExpenses',
                 'OtherCoreExpensesSalariesAndBenefitsAsPctCoreExpenses',
                 'TotalSalariesAndBenefitsAsPctTotalExpenses',
                 'TotalSalariesAndWagesAsPctTotalExpenses',
                 'CoreExpensesSalariesAndBenefitsAsPctCoreExpenses',
                 'InstructionSalariesAndBenefitsAsPctCoreExpenses',
                 'ResearchSalariesAndBenefitsAsPctCoreExpenses',
                 'PublicServiceSalariesAndBenefitsAsPctCoreExpenses',
                 'AcademicSupportSalariesAndBenefitsAsPctCoreExpenses',
                 'StudentServicesSalariesAndBenefitsAsPctCoreExpenses',
                 'InstitutionalSupportSalariesAndBenefitsAsPctCoreExpenses',
                 'OtherCoreExpensesSalariesAndBenefitsAsPctCoreExpenses',
                 'TotalSalariesAndBenefitsAsPctTotalExpenses',
                 'TotalSalariesAndWagesAsPctTotalExpenses',
                 'EndowmentAssetsEOYPerFte',
                 'EndowmentAssetsEOYPerFte',
                 'EquityRatio',
                 'EquityRatio',
                 'EquityRatio',
                 'PrivateGiftsAndContractsAsPctCoreRevenues',
                 'PrivateGiftsAndContractsAsPctCoreRevenues',
                 'PrivateGiftsAndContractsAsPctCoreRevenues',
                 'PrivateGiftsAndContractsAsPctCoreRevenues',
                 'GovtGrantsAndContractsAsPctCoreRevenues',
                 'GovtGrantsAndContractsAsPctCoreRevenues',
                 'x',
                 'y')

df<-data.frame(current, replacement)

colnames(pub) = df$replacement[which(df$current %in% colnames(pub))]
colnames(pri) = df$replacement[which(df$current %in% colnames(pri))]
colnames(pro) = df$replacement[which(df$current %in% colnames(pro))]

pro$x[is.na(pro$x)] <- 0
pro$y[is.na(pro$y)] <- 0
pro$OtherCoreAsPctCoreExpenses[is.na(pro$OtherCoreAsPctCoreExpenses)] <- 0
pro$OtherCoreExpensesPerFte[is.na(pro$OtherCoreExpensesPerFte)] <- 0
pro$OtherCoreAsPctCoreExpenses <- pro$OtherCoreAsPctCoreExpenses + pro$x
pro$OtherCoreExpensesPerFte <- pro$OtherCoreExpensesPerFte + pro$y

pro <- subset(pro,
              select=c(-x,
                       -y))

# combine sectors in single data frame
dfr <- rbind.fill(pub,pri)
dfr <- subset(dfr,
              select=c(-InstitutionName))

rm(dat, pub, pri, pro, df)

# fix null fields

# adjust percentages
namez <- names(dfr)[grep('Pct',names(dfr))]
# x <- dfr[, names(dfr)[grep('Pct',names(dfr))]]

for (i in 1:length(namez))
{
  dfr[[namez[i]]] <- dfr[[namez[i]]] / 100
  dfr[[namez[i]]][abs(dfr[[namez[i]]])  > 1] <- NA
}
#dfr[, namez] <- dfr[, namez] / 100

dfr$EquityRatio <- dfr$EquityRatio / 100

rm(namez, fnames, nnames, pnames)

# Write the outfile.  Empty fields are left null
writeFile(dfr, outfile)

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
con <-odbcConnect(odbcString)

resp <- sqlQuery(con, "DELETE FROM dev.dfr")

# Append value to FallEnrollment table.  Run ONCE or there will be a key violation
sqlSave(con,
        dfr,
        tablename='dev.dfr',
        append=TRUE,
        rownames=FALSE,
        verbose=TRUE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)

# Execute Extract-to-Staging SPROC
# resp <- sqlQuery(con, "EXEC operations.TransferFASBExpensesExtractToStagedTable")

# Close the connection to the db
close(con)
con <- NULL
rm(con)
# rm(resp)

# Housekeeping
# rm(list=ls())
