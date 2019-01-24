# parameters
start_year <- 2000
end_year <- 2017
db_string <- "ched-dev"

# include user functions
source(file="r-scripts/user-functions.R")

read_file <- function(year, file_name) {
  url <- str_c("https://nces.ed.gov/ipeds/datacenter/data/c", year, "_a.zip")
  
  recordset <- net_load_zip(url, file_name) %>%
    mutate(unitid = as.integer(unitid))
  
  if (year < 2001) {
    recordset <- recordset %>%
      mutate(unitid = as.integer(unitid),
             cipcode = as.character(cipcode),
             awlevel = as.integer(awlevel),
             majornum = 1,
             cnralm = as.integer(crace01),
             cnralw = as.integer(crace02),
             cunknm = as.integer(crace13),
             cunknw = as.integer(crace14),
             chispm = as.integer(crace09),
             chispw = as.integer(crace10),
             caianm = as.integer(crace05),
             caianw = as.integer(crace06),
             casiam = as.integer(crace07),
             casiaw = as.integer(crace08),
             cbkaam = as.integer(crace03),
             cbkaaw = as.integer(crace04),
             cnhpim = 0,
             cnhpiw = 0,
             cwhitm = as.integer(crace11),
             cwhitw = as.integer(crace12),
             c2morm = 0,
             c2morw = 0)
  }

  if (year %in% 2001:2007) {
    recordset <- recordset %>%
      mutate(unitid = as.integer(unitid),
             cipcode = as.character(cipcode),
             awlevel = as.integer(awlevel),
             majornum = as.integer(majornum),
             cnralm = as.integer(crace01),
             cnralw = as.integer(crace02),
             cunknm = as.integer(crace13),
             cunknw = as.integer(crace14),
             chispm = as.integer(crace09),
             chispw = as.integer(crace10),
             caianm = as.integer(crace05),
             caianw = as.integer(crace06),
             casiam = as.integer(crace07),
             casiaw = as.integer(crace08),
             cbkaam = as.integer(crace03),
             cbkaaw = as.integer(crace04),
             cnhpim = 0,
             cnhpiw = 0,
             cwhitm = as.integer(crace11),
             cwhitw = as.integer(crace12),
             c2morm = 0,
             c2morw = 0)
  }
  
  if (year %in% 2008:2010) {
    recordset <- recordset %>%
      mutate(unitid = as.integer(unitid),
             cipcode = as.character(cipcode),
             awlevel = as.integer(awlevel),
             majornum = as.integer(majornum),
             cnralm = as.integer(cnralm),
             cnralw = as.integer(cnralw),
             cunknm = as.integer(cunknm),
             cunknw = as.integer(cunknw),
             chispm = as.integer(dvchsm),
             chispw = as.integer(dvchsw),
             caianm = as.integer(dvcaim),
             caianw = as.integer(dvcaiw),
             casiam = as.integer(dvcapm),
             casiaw = as.integer(dvcapw),
             cbkaam = as.integer(dvcbkm),
             cbkaaw = as.integer(dvcbkw),
             cnhpim = 0,
             cnhpiw = 0,
             cwhitm = as.integer(dvcwhm),
             cwhitw = as.integer(dvcwhw),
             c2morm = as.integer(c2morm),
             c2morw = as.integer(c2morw))
  }
  
  if (year > 2010) {
    recordset <- recordset %>%
      mutate(unitid = as.integer(unitid),
             cipcode = as.character(cipcode),
             awlevel = as.integer(awlevel),
             majornum = as.integer(majornum),
             cnralm = as.integer(cnralm),
             cnralw = as.integer(cnralw),
             cunknm = as.integer(cunknm),
             cunknw = as.integer(cunknw),
             chispm = as.integer(chispm),
             chispw = as.integer(chispw),
             caianm = as.integer(caianm),
             caianw = as.integer(caianw),
             casiam = as.integer(casiam),
             casiaw = as.integer(casiaw),
             cbkaam = as.integer(cbkaam),
             cbkaaw = as.integer(cbkaaw),
             cnhpim = as.integer(cnhpim),
             cnhpiw = as.integer(cnhpiw),
             cwhitm = as.integer(cwhitm),
             cwhitw = as.integer(cwhitw),
             c2morm = as.integer(c2morm),
             c2morw = as.integer(c2morw))
  }
  
  recordset %>%
    # filter(awlevel %in% c(1,2,3,4,5,6,7,8,17,18,19)) %>%
    select(unitid, cipcode, majornum, awlevel,
           cnralm, cnralw, cunknm, cunknw, chispm,
           chispw, caianm, caianw, casiam, casiaw,
           cbkaam, cbkaaw, cnhpim, cnhpiw, cwhitm,
           cwhitw, c2morm, c2morw)
}

for (year in start_year:end_year) {
  read_file(year, str_c("c", year, "_a.csv")) %>%
    mutate(date_key = (year * 10000) + 630,
           degree_key = str_c(str_pad(awlevel, 2, pad="0"), majornum),
           award_level = recode(awlevel,
                                `1` = "Award of less than 1 academic year",
                                `2` = "Award of at least 1 but less than 2 academic years",
                                `3` = "Associate's degree",
                                `4` = "Award of at least 2 but less than 4 academic years",
                                `5` = "Bachelor's degree",
                                `6` = "Postbaccalaureate certificate",
                                `7` = "Master's degree",
                                `8` = "Post-master's certificate",
                                `9` = "Doctor's degree",
                                `10` = "First-professional degree",
                                `11` = "First-professional certificate",
                                `17` = "Doctor's degree - research/scholarship",
                                `18` = "Doctor's degree - professional practice",
                                `19` = "Doctor's degree - other",
                                .default = "Unknown"),
           major_number = recode(majornum,
                                 `1` = "First Major",
                                 `2` = "Second Major",
                                 .default = "Unknown")) %>%
    select(unitid, date_key, degree_key, cipcode, award_level, major_number, cnralm:c2morw) %>%
    gather(cnralm:c2morw, key = "variable", value = "awards") %>%
    filter(!is.na(awards)) %>%
    separate(variable, c("survey", "demographic_key"), sep = c(1)) %>%
    select(unitid, date_key, degree_key, cipcode, demographic_key, awards) %>%
    insert_db(db_string, "ipeds_degree_completions")
}


