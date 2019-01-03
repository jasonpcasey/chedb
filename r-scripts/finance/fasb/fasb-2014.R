# clear the workspace
rm(list=ls())

library(odbc)
library(tidyverse)

year <- 2014
archive <- c('f1314_f2.zip','flags2014.zip')
infile <- c('f1314_f2_rv.csv','flags2014.csv')

odbcString <- "cdw"
urlStub <- "http://nces.ed.gov/ipeds/datacenter/data/"

# access data via net
netLoad <- function(fileUrl, fileName)
{
  #Download data file
  temp <- tempfile()
  
  download.file(fileUrl,
                temp)
  
  data <- read_csv(unz(temp, fileName))
  
  unlink(temp)
  # rm(temp)
  
  names(data) <- tolower(names(data))
  return(data)
}

raw.dat <- netLoad(paste0(urlStub, archive[1]),
                   infile[1]) %>%
  left_join(netLoad(paste0(urlStub, archive[2]),
                    infile[2]), by = 'unitid') %>%
  mutate(Unitid = unitid,
         YearId = year)

expenses <- raw.dat %>%
  gather(f2e012, f2e013, f2e014, f2e015, f2e016, f2e017,
         f2e022, f2e023, f2e024, f2e025, f2e026, f2e027,
         f2e032, f2e033, f2e034, f2e035, f2e036, f2e037,
         f2e042, f2e043, f2e044, f2e045, f2e046, f2e047,
         f2e052, f2e053, f2e054, f2e055, f2e056, f2e057,
         f2e062, f2e063, f2e064, f2e065, f2e066, f2e067,
         f2e072, f2e073, f2e074, f2e075, f2e076, f2e077,
         f2e087,
         f2e092, f2e093, f2e094, f2e095, f2e096, f2e097,
         f2e102, f2e103, f2e104, f2e105, f2e106, f2e107,
         f2e112, f2e113, f2e114, f2e115, f2e116, f2e117,
         f2e122, f2e123, f2e124, f2e125, f2e126, f2e127,
         key = 'field', value = 'Expense') %>%
  mutate(field = substr(field, 4, 6)) %>%
  separate(field, c('FunctionalClassification','NaturalClassification'), 2) %>%
  mutate(FunctionalClassification = recode(FunctionalClassification,
                                           '01'='Instruction',
                                           '02'='Research',
                                           '03'='Public service',
                                           '04'='Academic support',
                                           '05'='Student services',
                                           '06'='Institutional support',
                                           '07'='Auxiliary enterprises',
                                           '08'='Net grant aid to students',
                                           '09'='Hospital services',
                                           '10'='Independent operations',
                                           '11'='Operation and maintenance of plant',
                                           '12'='Other',
                                           '13'='Total'),
         NaturalClassification = recode(NaturalClassification,
                                        '1'='Total',
                                        '2'='Salaries and wages',
                                        '3'='Fringe benefits',
                                        '5'='Depreciation',
                                        '7'='Other',
                                        '4'='Operation and maintenance of plant',
                                        '6'='Interest'),
         Expense = ifelse(is.na(Expense),0,Expense)) %>%
  select(Unitid,
         YearId,
         FunctionalClassification,
         NaturalClassification,
         Expense)

con <- dbConnect(odbc::odbc(), odbcString)

response <- dbSendQuery(con, paste0('DELETE FROM dbo.FasbExpenses WHERE YearId=', year))
dbClearResult(response)

system.time({ dbWriteTable(con, 'FasbExpenses', expenses, append=TRUE) })

dbDisconnect(con)
