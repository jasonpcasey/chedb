# clear workspace
rm(list=ls())

# include user functions
source(file="scripts/user-functions.R")

# infile and outfile are set to the input and output file paths
spec1 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2015_16/Provisional/f1516_f1a_dist.sav"
spec2 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2015_16/Provisional/flags2016_dist.sav"
outdir <- 'C:/Temp/'
odbcString <- "OSPIR-DEV"

# Year is set to ND's academic year, same as fall year
year <- 2015L

df1 <- readSPSSFile(spec1)
df2 <- readSPSSFile(spec2)

dat <- left_join(df1, df2, by = 'unitid') %>%
  mutate(Unitid = unitid,
         YearId = year,
         f1c017 = f1c011 - f1c012,
         f1c027 = f1c021 - f1c022,
         f1c037 = f1c031 - f1c032,
         f1c057 = f1c051 - f1c052,
         f1c067 = f1c061 - f1c062,
         f1c077 = f1c071 - f1c072,
         # f1c087 = f1c081 - f1c082,
         f1c107 = f1c101,
         f1c117 = f1c111 - f1c112,
         f1c127 = f1c121 - f1c122,
         f1c137 = f1c131 - f1c132,
         f1c147 = f1c141 - f1c142) %>%
  gather(f1c012,f1c017,
         f1c022,f1c027,
         f1c032,f1c037,
         f1c052,f1c057,
         f1c062,f1c067,
         f1c072,f1c077,
         # f1c087,f1c082,
         f1c107,
         f1c112,f1c117,
         f1c122,f1c127,
         f1c132,f1c137,
         f1c142,f1c147,
         key = 'field', value = 'Expense') %>%
  mutate(field = substr(field, 4, 6)) %>%
  separate(field, c('FunctionalClassificationId','NaturalClassificationId'), 2) %>%
  mutate(FunctionalClassificationId = as.integer(FunctionalClassificationId),
         NaturalClassificationId = as.integer(NaturalClassificationId),
         Expense = ifelse(is.na(Expense),0,Expense)) %>%
  select(Unitid,
         YearId,
         FunctionalClassificationId,
         NaturalClassificationId,
         Expense)

rm(df1, df2)

# Write the outfile.  Empty fields are left null
writeFile(dat, paste0(paste0(outdir,'gasb_expensedat'), year, ".csv"))

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
# con <-odbcConnect(odbcString)

# resp <- sqlQuery(con, paste0("DELETE FROM extract.GASBExpenses WHERE YearId=",as.character(year)))

# Append value to FallEnrollment table.  Run ONCE or there will be a key violation
# sqlSave(con,
#         expenses,
#         tablename='extract.GASBExpenses',
#         append=TRUE,
#         rownames=FALSE,
#         verbose=TRUE,
#         safer=TRUE,
#         fast=TRUE,
#         test=FALSE)

# Execute Extract-to-Staging SPROC
# resp <- sqlQuery(con, "EXEC operations.TransferIpedsFinanceExpensesExtractToStagedTable")

# Close the connection to the db
# close(con)
# con <- NULL
# rm(con)
# rm(resp)

# Housekeeping
# rm(list=ls())
