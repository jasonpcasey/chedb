# clear workspace
rm(list=ls())

# include user functions
source(file="scripts/user-functions.R")

# infile and outfile are set to the input and output file paths
spec1 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2014_15/Provisional/f1415_f1a.csv"
spec2 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2014_15/Provisional/flags2015.csv"
outfile <- "C:/Temp/gasb_dat2015.csv"
odbcString <- "OSPIR-DEV"

# Year is set to ND's academic year, same as fall year
year <- 2014L

dat <- readFile(spec1)
df <- readFile(spec2)

# Assign data year
dat$YearId <- year
df$YearId <- year

# GASB reporting models:
#  Business Type Activities (B)
#  Governmental Activities (G)
#  Governmental Activities with Business-Type Activities (X)

expenses <- melt(data=dat,
             id=c('unitid',
                       'YearId'),
             measure.vars=c('f1c012', 'f1c013', 'f1c014', 'f1c015', 'f1c016', 'f1c017',
                            'f1c022', 'f1c023', 'f1c024', 'f1c025', 'f1c026', 'f1c027',
                            'f1c032', 'f1c033', 'f1c034', 'f1c035', 'f1c036', 'f1c037',
                            'f1c052', 'f1c053', 'f1c054', 'f1c055', 'f1c056', 'f1c057',
                            'f1c062', 'f1c063', 'f1c064', 'f1c065', 'f1c066', 'f1c067',
                            'f1c072', 'f1c073', 'f1c074', 'f1c075', 'f1c076', 'f1c077',
                            'f1c082', 'f1c083', 'f1c084', 'f1c085', 'f1c086', 'f1c087',
                            'f1c105',
                            'f1c112', 'f1c113', 'f1c114', 'f1c115', 'f1c116', 'f1c117',
                            'f1c122', 'f1c123', 'f1c124', 'f1c125', 'f1c126', 'f1c127',
                            'f1c132', 'f1c133', 'f1c134', 'f1c135', 'f1c136', 'f1c137',
                            'f1c142', 'f1c143', 'f1c144', 'f1c145', 'f1c146', 'f1c147'),
             variable.name="field",
             value.name="Expense")

names(expenses) <- c('Unitid','YearId','field','Expense')

expenses$FunctionalClassificationId <- as.integer(substr(expenses$field, 4, 5))
expenses$NaturalClassificationId <- as.integer(substr(expenses$field, 6, 6))

expenses$Expense[is.na(expenses$Expense)] <- 0

expenses <- subset(expenses,
               select=c(Unitid,
                        YearId,
                        FunctionalClassificationId,
                        NaturalClassificationId,
                        Expense))

# Write the outfile.  Empty fields are left null
writeFile(expenses, outfile)

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
