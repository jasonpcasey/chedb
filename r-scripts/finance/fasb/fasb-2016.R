# clear the workspace
rm(list=ls())

library(odbc)
library(tidyverse)

year <- 2016
archive <- c('f1516_f2.zip','flags2016.zip')
infile <- c('f1516_f2.csv','flags2016.csv')

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
  mutate(f2e017 = f2e011 - f2e012,
         f2e027 = f2e021 - f2e022,
         f2e037 = f2e031 - f2e032,
         f2e047 = f2e041 - f2e042,
         f2e057 = f2e051 - f2e052,
         f2e067 = f2e061 - f2e062,
         f2e077 = f2e071 - f2e072,
         f2e087 = f2e081,
         f2e097 = f2e091 - f2e092,
         f2e107 = f2e101 - f2e102,
         f2e127 = f2e121 - f2e122) %>%
  gather(f2e012,f2e017,
         f2e022,f2e027,
         f2e032,f2e037,
         f2e042,f2e047,
         f2e052,f2e057,
         f2e062,f2e067,
         f2e072,f2e077,
         f2e087,
         f2e092,f2e097,
         f2e102,f2e107,
         f2e122,f2e127,
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
