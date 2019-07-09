# include user functions
library(DBI)
library(odbc)
library(readxl)
library(keyring)
library(lubridate)
library(kableExtra)
library(tidyverse)

# access data via net
net_load <- function(file_url)
{
  read_csv(file_url,
           locale = locale(encoding = "latin1")) %>%
    rename_all(tolower)
}

# access zipped data via net
net_load_zip <- function(file_url, file_name)
{
  #Download data file
  temp <- tempfile()
  
  download.file(file_url,
                temp)
  
  files <- unzip(temp, list = TRUE)

  spec <- files$Name[str_detect(files$Name, "_rv")]

  file_name <- ifelse(length(spec) == 0, file_name, spec)
  
  data <- read_csv(unz(temp, file_name),
                   locale = locale(encoding = "latin1")) %>%
    rename_all(tolower)
  
  unlink(temp)
  rm(temp)
  
  return(data)
}

read_db <- function(db, query_string)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(), db)
  
  response <- dbSendQuery(connection, query_string)
  tbl <- dbFetch(response)
  dbClearResult(response)
  
  # disconnect from the database
  dbDisconnect(connection)
  
  return(as_tibble(tbl))
}

write_db <- function(frame, db, tab) {
  con <- dbConnect(odbc::odbc(), db)
  
  answer <- system.time({ dbWriteTable(con,
                                       tab,
                                       frame,
                                       overwrite = TRUE
  ) })
  
  dbDisconnect(con)
  
  return(answer)
}

append_db <- function(frame, db, tab) {
  con <- dbConnect(odbc::odbc(), db)
  
  answer <- system.time({ dbWriteTable(con,
                                       tab,
                                       frame,
                                       overwrite = FALSE,
                                       append = TRUE
  ) })
  
  dbDisconnect(con)
  
  return(answer)
}

insert_db <- function(frame, db, tab) {
  con <- dbConnect(odbc::odbc(), db)
  
  answer <- system.time({ dbWriteTable(con,
                                       tab,
                                       frame,
                                       append = TRUE,
                                       overwrite = FALSE
  ) })
  
  dbDisconnect(con)
  
  return(answer)
}

read_aaude_data <- function(dbString, query_string)
{
  # Open a connection
  connection <- dbConnect(odbc::odbc(),
                          dbString, 
                          UID = keyring::key_get("aaude_user"),
                          PWD = keyring::key_get("mit_secret"))
  
  response <- dbSendQuery(connection, query_string)
  tbl <- dbFetch(response) %>%
    rename_all(tolower)
  dbClearResult(response)
  
  # disconnect from the database
  dbDisconnect(connection)
  
  return(as_tibble(tbl))
}

