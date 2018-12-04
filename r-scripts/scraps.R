# Institutions Table

```{r}
institutions <- dat %>%
  mutate(NsfSurveyId = inst_id,
         `Institution Name` = inst_name_long,
         City = inst_city,
         State = inst_state_code,
         ZIP = inst_zip,
         HBCU = recode(hbcu_flag,
                       `1`='HBCU',
                       `0`='Not HBCU',
                       .default='Unknown'),
         `High Hispanic Enrollment` = ifelse(hhe_flag==1, 'High Hispanic Enrollment','Not High Hispanic Enrollment'),
         `Medical School` = ifelse(med_sch_flag, 'Has Medical School','No Medical School'),
         `Academic Institution` = ifelse(toi_code==1, 'Academic', 'Non-academic'),
         `Highest Degree Granted` = recode(hdg_code,
                                           `1`='Doctorate',
                                           `2`="Master's",
                                           `3`="Bachelor's",
                                           `4`="Associates's",
                                           `5`='No degree',
                                           `6`='Professional degree'),
         `Type of Control` = recode(toc_code,
                                    `1` = 'Public',
                                    `2` = 'Private',
                                    .default = 'Unknown')) %>%
  inner_join(fice) %>%
  filter(year == maxyear) %>%
  group_by(RdFice, 
           `Institution Name`,
           City,
           State,
           ZIP,
           HBCU,
           `High Hispanic Enrollment`,
           `Medical School`,
           `Academic Institution`,
           `Highest Degree Granted`,
           `Type of Control`) %>%
  summarise(rows = n()) %>%
  ungroup()

institutions
```

# frame_fixes <- function(x) {
#   cols <- c('bsurvid',
#             'birthyear')
#   
#   x_cols <- colnames(x)
#   
#   for (i in seq_along(cols)) {
#     if (!cols[i] %in% x_cols)
#     {
#       x[cols[i]] <- NA
#     }
#   }
#   
#   x %>%
#     mutate(bsurvid = as.integer(bsurvid),
#            birthyear = as.integer(birthyear))
# }




library(readxl)

carnegie_2015 <- read_excel("http://carnegieclassifications.iu.edu/downloads/CCIHE2015-PublicDataFile.xlsx",
                            sheet = "Data")

net_read_files <- function(file1, file2) {
  spec1 <- str_c("https://nces.ed.gov/ipeds/datacenter/data/", file1)
  spec2 <- str_c("https://nces.ed.gov/ipeds/datacenter/data/", file2)
  
  details <- net_load(spec2, file2) %>%
    rename_all(tolower)
  
  recordset <- net_load(spec1, file1) %>%
    rename_all(tolower) %>%
    left_join(details, by = "unitid") %>%
    mutate(	unitid = as.integer(unitid),
            zip = as.character(zip),
            fips = as.integer(fips),
            state = recode(fips,
                           `1` = "Alabama",
                           `2` = "Alaska",
                           `4` = "Arizona",
                           `5` = "Arkansas",
                           `6` = "California",
                           `8` = "Colorado",
                           `9` = "Connecticut",
                           `10` = "Delaware",
                           `11` = "District of Columbia",
                           `12` = "Florida",
                           `13` = "Georgia",
                           `15` = "Hawaii",
                           `16` = "Idaho",
                           `17` = "Illinois",
                           `18` = "Indiana",
                           `19` = "Iowa",
                           `20` = "Kansas",
                           `21` = "Kentucky",
                           `22` = "Louisiana",
                           `23` = "Maine",
                           `24` = "Maryland",
                           `25` = "Massachusetts",
                           `26` = "Michigan",
                           `27` = "Minnesota",
                           `28` = "Mississippi",
                           `29` = "Missouri",
                           `30` = "Montana",
                           `31` = "Nebraska",
                           `32` = "Nevada",
                           `33` = "New Hampshire",
                           `34` = "New Jersey",
                           `35` = "New Mexico",
                           `36` = "New York",
                           `37` = "North Carolina",
                           `38` = "North Dakota",
                           `39` = "Ohio",
                           `40` = "Oklahoma",
                           `41` = "Oregon",
                           `42` = "Pennsylvania",
                           `44` = "Rhode Island",
                           `45` = "South Carolina",
                           `46` = "South Dakota",
                           `47` = "Tennessee",
                           `48` = "Texas",
                           `49` = "Utah",
                           `50` = "Vermont",
                           `51` = "Virginia",
                           `53` = "Washington",
                           `54` = "West Virginia",
                           `55` = "Wisconsin",
                           `56` = "Wyoming",
                           `60` = "American Samoa",
                           `64` = "Federated States of Micronesia",
                           `66` = "Guam",
                           `68` = "Marshall Islands",
                           `69` = "Northern Marianas",
                           `70` = "Palau",
                           `72` = "Puerto Rico",
                           `78` = "Virgin Islands",
                           .default = "Unknown"),
            service_academy = ifelse(as.integer(obereg) == 0, 1, 0),
            region = recode(fips,
                            `1` = "Southeast",
                            `2` = "Far West",
                            `4` = "Southwest",
                            `5` = "Southeast",
                            `6` = "Far West",
                            `8` = "Rocky Mountains",
                            `9` = "New England",
                            `10` = "Mid East",
                            `11` = "Mid East",
                            `12` = "Southeast",
                            `13` = "Southeast",
                            `15` = "Far West",
                            `16` = "Rocky Mountains",
                            `17` = "Great Lakes",
                            `18` = "Great Lakes",
                            `19` = "Plains",
                            `20` = "Plains",
                            `21` = "Southeast",
                            `22` = "Southeast",
                            `23` = "New England",
                            `24` = "Mid East",
                            `25` = "New England",
                            `26` = "Great Lakes",
                            `27` = "Plains",
                            `28` = "Southeast",
                            `29` = "Plains",
                            `30` = "Rocky Mountains",
                            `31` = "Plains",
                            `32` = "Far West",
                            `33` = "New England",
                            `34` = "Mid East",
                            `35` = "Southwest",
                            `36` = "Mid East",
                            `37` = "Southeast",
                            `38` = "Plains",
                            `39` = "Great Lakes",
                            `40` = "Southwest",
                            `41` = "Far West",
                            `42` = "Mid East",
                            `44` = "New England",
                            `45` = "Southeast",
                            `46` = "Plains",
                            `47` = "Southeast",
                            `48` = "Southwest",
                            `49` = "Rocky Mountains",
                            `50` = "New England",
                            `51` = "Southeast",
                            `53` = "Far West",
                            `54` = "Southeast",
                            `55` = "Great Lakes",
                            `56` = "Rocky Mountains",
                            `60` = "Outlying Areas",
                            `64` = "Outlying Areas",
                            `66` = "Outlying Areas",
                            `68` = "Outlying Areas",
                            `69` = "Outlying Areas",
                            `70` = "Outlying Areas",
                            `72` = "Outlying Areas",
                            `78` = "Outlying Areas",
                            .default = "Unknown"),
            closedat = ifelse(str_length(closedat) < 3, NA, closedat),
            sector = as.integer(sector),
            iclevel = as.integer(iclevel),
            control = as.integer(control),
            hloffer = as.integer(hloffer),
            hdegofr1 = as.integer(hdegofr1),
            locale = as.integer(locale),
            newid = as.integer(newid),
            deathyr = as.integer(deathyr),
            cbsa = as.integer(cbsa),
            cbsatype = as.integer(cbsatype),
            csa = as.integer(csa),
            necta = as.integer(necta),
            countycd = as.integer(countycd),
            longitud = as.numeric(longitud),
            latitude = as.numeric(latitude),
            f1syscod = ifelse("f1syscod" %in% colnames(.), as.character(f1syscod), NA),
            f1systyp = as.character(f1systyp),
            f1sysnam = as.character(f1sysnam),
            ugoffer = as.integer(ugoffer),
            groffer = as.integer(groffer),
            deggrant = as.integer(deggrant),
            openpubl = as.integer(openpubl),
            landgrnt = as.integer(landgrnt),
            hbcu = as.integer(hbcu),
            hospital = as.integer(hospital),
            medical = as.integer(medical),
            tribal = as.integer(tribal),
            slo5 = as.integer(slo5),
            confno1 = as.integer(confno1),
            confno2 = as.integer(confno2),
            confno3 = as.integer(confno3),
            confno4 = as.integer(confno4),
            cyactive = as.integer(cyactive)) %>%
    select(unitid, instnm, addr, city, stabbr, state, zip, webaddr, fips, region, service_academy,
           iclevel, control, hloffer, hdegofr1, locale, newid, deathyr, closedat,
           cbsa, cbsatype, csa, necta, countycd, longitud, latitude, f1syscod,
           f1systyp, f1sysnam, ugoffer, groffer, deggrant, openpubl, landgrnt,
           hbcu, hospital, medical, tribal, slo5,
           confno1, confno2, confno3, confno4, cyactive)
}


charge_type_id = recode(variable,
                        'chg1ay1'=1,
                        'chg1ay2'=1,
                        'chg1ay3'=1,
                        'chg2ay1'=2,
                        'chg2ay2'=2,
                        'chg2ay3'=2,
                        'chg3ay1'=3,
                        'chg3ay2'=3,
                        'chg3ay3'=3,
                        'chg4ay1'=4,
                        'chg4ay2'=4,
                        'chg4ay3'=4,
                        'chg5ay1'=5,
                        'chg5ay2'=5,
                        'chg5ay3'=5,
                        .default=6),


in_district_charge = ifelse(charge_type_id > 2,
                            as.numeric(charge),
                            ifelse(str_detect(class, "^1"), as.numeric(charge), NA)),
in_state_charge = ifelse(charge_type_id > 2,
                         as.numeric(charge),
                         ifelse(str_detect(class, "^2"), as.numeric(charge), NA)),
out_of_state_charge = ifelse(charge_type_id > 2,
                             as.numeric(charge),
                             ifelse(str_detect(class, "^3"), as.numeric(charge), NA))

%>%
  group_by(unitid, year_id, year, charge_type_id) %>%
  summarize(in_district_charge = max(in_district_charge, na.rm = TRUE),
            in_state_charge = max(in_state_charge, na.rm = TRUE),
            out_of_state_charge = max(out_of_state_charge, na.rm = TRUE))
