# clear workspace
rm(list=ls())

# include user functions
source(file="scripts/user-functions.R")

# Year is set to ND's academic year, same as fall year
year <- 2015L

# infile and outfile are set to the input and output file paths
spec1 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2015_16/Provisional/f1516_f2_dist.sav"
spec2 <- "U:/Data/EX/z_external_data/IPEDS_Finance/2015_16/Provisional/flags2016_dist.sav"

odbcString <- "OSPIR-PROD"

outdir <- 'C:/Temp/'

df1 <- readSPSSFile(spec1)
df2 <- readSPSSFile(spec2)

rm(spec1, spec2)

# make base data file
dat <- left_join(df1, df2, by = 'unitid') %>%
  separate(fybeg, c('bmonth','byear'), 2) %>%
  separate(fyend, c('emonth','eyear'), 2) %>%
  mutate(Unitid = unitid,
         YearId = year,
         pcf_f = as.integer(pcf_f),
         bmonth = ifelse(as.integer(bmonth) > 0 & as.integer(bmonth) < 13, bmonth, ''),
         byear = ifelse(as.integer(byear) > 1980 & as.integer(byear) < 3000, byear, ''),
         emonth = ifelse(as.integer(emonth) > 0 & as.integer(emonth) < 13, emonth, ''),
         eyear = ifelse(as.integer(eyear) > 1980 & as.integer(eyear) < 3000, eyear, ''),
         fybeg = paste(bmonth, byear, sep='/'),
         fyend = paste(emonth, eyear, sep='/'),
         FYBegin = as.Date(as.yearmon(fybeg, '%m/%Y')),
         FYEnd = eom(as.Date(as.yearmon(fyend, '%m/%Y')))) %>%
  select(-stat_ic,-lock_ic,-imp_ic,-stat_c,-lock_c,-prch_c,-idx_c,-imp_c,-stat_e12,-lock_e12,
         -prch_e12,-idx_e12,-imp_e12,-stat_sfa,-lock_sfa,-prch_sfa,-idx_sfa,-imp_sfa,-sfaform,
         -stat_gr,-lock_gr,-prch_gr,-idx_gr,-imp_gr,-cohrtstu,-stat_gr2,-lock_gr2,-prch_gr2,
         -idx_gr2,-imp_gr2,-stat_om,-lock_om,-prch_om,-idx_om,-imp_om,-stat_adm,-lock_adm,
         -imp_adm,-stat_hr,-lock_hr,-prch_hr,-idx_hr,-imp_hr,-ftemp15,-tenursys,-sa_excl,
         -stat_eap,-stat_sa,-stat_s,-stat_ef,-lock_ef,-prch_ef,-idx_ef,-imp_ef,-pta99_ef,
         -ptacipef,-ptb_ef,-ptc_ef,-ptd_ef,-stat_al,-lock_al,-prch_al,-idx_al,-imp_al,
         -hasal,-ntrldstr,-xf2h02,-xf2h01,
         -xf2e137,-xf2e136,-xf2e135,-xf2e134,-xf2e133,-xf2e132,-xf2e131,
         -xf2e122,-xf2e121,-xf2e102,-xf2e101,-xf2e092,-xf2e091,-xf2e081,
         -xf2e072,-xf2e071,-xf2e062,-xf2e061,-xf2e052,-xf2e051,-xf2e042,-xf2e041,
         -xf2e032,-xf2e031,-xf2e022,-xf2e021,-xf2e012,-xf2e011,-xf2d184,-xf2d183,
         -xf2d182,-xf2d18,-xf2d174,-xf2d173,-xf2d172,-xf2d17,-xf2d164,-xf2d163,
         -xf2d162,-xf2d16,-xf2d154,-xf2d153,-xf2d152,-xf2d15,-xf2d144,-xf2d143,
         -xf2d142,-xf2d14,-xf2d132,-xf2d13,-xf2d122,-xf2d12,-xf2d112,-xf2d11,
         -xf2d104,-xf2d103,-xf2d102,-xf2d10,-xf2d094,-xf2d093,-xf2d092,-xf2d09,
         -xf2d08b,-xf2d08a,-xf2d084b,-xf2d084a,-xf2d084,-xf2d083b,-xf2d083a,
         -xf2d083,-xf2d082b,-xf2d082a,-xf2d082,-xf2d08,-xf2d074,-xf2d073,
         -xf2d072,-xf2d07,-xf2d064,-xf2d063,-xf2d062,-xf2d06,-xf2d054,
         -xf2d053,-xf2d052,-xf2d05,-xf2d044,-xf2d043,-xf2d042,-xf2d04,
         -xf2d034,-xf2d033,-xf2d032,-xf2d03,-xf2d024,-xf2d023,-xf2d022,
         -xf2d02,-xf2d014,-xf2d013,-xf2d012,-xf2d01,-xf2c10,-xf2c09,
         -xf2c08,-xf2c07,-xf2c06,-xf2c05,-xf2c04,-xf2c03,-xf2c02,-xf2c01,
         -xf2b07,-xf2b06,-xf2b05,-xf2b04,-xf2b03,-xf2b02,-xf2b01,-xf2a20,
         -xf2a19,-xf2a18,-xf2a17,-xf2a16,-xf2a15,-xf2a13,-xf2a12,-xf2a11,
         -xf2a06,-xf2a05b,-xf2a05a,-xf2a05,-xf2a04,-xf2a03a,-xf2a03,-xf2a02,-xf2a01,
         -fybeg, -fyend, -bmonth, -byear, -emonth, -eyear, -unitid)


# housekeeping
rm(df1, df2)

finpos <- dat %>%
  mutate(LongTermInvestments = f2a01,
         PropertyNetDeprec = f2a19,
         IntangibleAssets = f2a20,
         TotalAssets = f2a02,
         TotalLiabilities = f2a03,
         PropertyDebt = f2a03a,
         UnrestrictedNetAssets = f2a04,
         PermRestrictedNetAssets = f2a05a,
         TempRestrictedNetAssets = f2a05b,
         RestrictedNetAssets = f2a05,
         TotalNetAssets = f2a06,
         Land = f2a11,
         Buildings = f2a12,
         Equipment = f2a13,
         Construction = f2a15,
         OtherPlant = f2a16,
         TotalPlant = f2a17,
         AccumulatedDepreciation = f2a18,
         TotalRevenuesInvReturn = f2b01,
         TotalExpenses = f2b02,
         OtherChangesNetAssets = f2b03,
         ChangeNetAssets = f2b04,
         NetAssetsBeginning = f2b05,
         AdjustmentsNetAssetsBeginning = f2b06,
         NetAssetsEnding = f2b07,
         AllowancesTuition = f2c08,
         AllowancesAuxiliaryEnterprise = f2c09,
         NetAssetsReleasedTempRestriction = f2d173,
         NetAssetsReleasedPermRestriction = f2d174,
         NetRevenuesAfterAssetsReleased = f2d18,
         EndowmentAssetsBeginning = f2h01,
         EndowmentAssetsEnding = f2h02,
         ParentId = ifelse(idx_f > 0, idx_f, Unitid),
         CleanOpinion = recode(as.integer(gpfs),
                               `1` = 1,
                               `2` = 0,
                               .default=0),
         ParentChildAllocationFactor = ifelse(is.na(pcf_f) & Unitid==ParentId, 1, pcf_f / 100),
         ParentChildIndicator = recode(as.integer(prchtp_f),
                                       `1`='Parent record - possibly includes data from branch campuses',
                                       `2`='Child record - data reported with parent campus',
                                       `3`='Partial child record - reports partial data and other data reported with parent campus',
                                       `5`='Partial child record - reports partial data but other data is included with entity that is not a postsecondary institution',
                                       .missing='Not applicable',
                                       .default='Not applicable'),
         GASBAlternativeModel = recode(as.integer(f1gasbal),
                                       `1` = 'Business Type Activities',
                                       `2` = 'Governmental Activities',
                                       `3` = 'Governmental Activities with Business-Type Activities',
                                       `-1` = 'Not reported',
                                       .missing = 'Not applicable',
                                       .default = 'Not applicable'),
         PellGrantAccounting = recode(as.integer(f2pell),
                                      `1` = 'Pass through (agency)',
                                      `2` = 'Federal grants',
                                      `3` = 'Does not award Pell grants',
                                      `-1` = 'Not reported',
                                      .missing = 'Not applicable',
                                      .default = 'Not applicable'),
         AthleticExpenses = recode(as.integer(f_athltc),
                                   `1` = 'Auxiliary enterprises',
                                   `2` = 'Student services',
                                   `3` = 'Does not participate in intercollegiate athletics',
                                   `4` = 'Other',
                                   `-1` = 'Not reported',
                                   .missing = 'Not applicable',
                                   .default = 'Not applicable')) %>%
  select(Unitid,
         YearId,
         FYBegin,
         FYEnd,
         ParentChildIndicator,
         ParentId,
         ParentChildAllocationFactor,
         CleanOpinion,
         GASBAlternativeModel,
         PellGrantAccounting,
         AthleticExpenses,
         LongTermInvestments,
         PropertyNetDeprec,
         IntangibleAssets,
         TotalAssets,
         TotalLiabilities,
         PropertyDebt,
         UnrestrictedNetAssets,
         PermRestrictedNetAssets,
         TempRestrictedNetAssets,
         RestrictedNetAssets,
         TotalNetAssets,
         Land,
         Buildings,
         Equipment,
         Construction,
         OtherPlant,
         TotalPlant,
         AccumulatedDepreciation,
         TotalRevenuesInvReturn,
         TotalExpenses,
         OtherChangesNetAssets,
         ChangeNetAssets,
         NetAssetsBeginning,
         AdjustmentsNetAssetsBeginning,
         NetAssetsEnding,
         AllowancesTuition,
         AllowancesAuxiliaryEnterprise,
         NetAssetsReleasedTempRestriction,
         NetAssetsReleasedPermRestriction,
         NetRevenuesAfterAssetsReleased,
         EndowmentAssetsBeginning,
         EndowmentAssetsEnding)




expenses <- dat %>%
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
  separate(field, c('FunctionalClassificationId','NaturalClassificationId'), 2) %>%
  mutate(FunctionalClassificationId = as.integer(FunctionalClassificationId),
         NaturalClassificationId = as.integer(NaturalClassificationId),
         Expense = ifelse(is.na(Expense),0,Expense)) %>%
  select(Unitid,
         YearId,
         FunctionalClassificationId,
         NaturalClassificationId,
         Expense)




aid <- dat %>%
  gather(f2c01,f2c02,f2c03,f2c04,f2c05,f2c06, key='field', value='Allowance') %>%
  separate(field, c('chaff', 'AidSourceId'), -2) %>%
  mutate(Allowance = ifelse(is.na(Allowance), 0, Allowance),
         AidSourceId = as.integer(AidSourceId)) %>%
  select(Unitid,
         YearId,
         AidSourceId,
         Allowance)




revenues <- dat %>%
  gather(f2d012, f2d013, f2d014,
         f2d022, f2d023, f2d024,
         f2d032, f2d033, f2d034,
         f2d042, f2d043, f2d044,
         f2d052, f2d053, f2d054,
         f2d062, f2d063, f2d064,
         f2d072, f2d073, f2d074,
         f2d082a, f2d083a, f2d084a, 
         f2d082b, f2d083b, f2d084b,
         f2d092, f2d093, f2d094,
         f2d102, f2d103, f2d104,
         f2d112, f2d122, f2d132,
         f2d142, f2d143, f2d144,
         f2d152, f2d153, f2d154,
         f2d173, f2d174,
         key='field',
         value='Revenue') %>%
  separate(field, c('chaff','field'), 3) %>%
  separate(field, c('RevenueClassificationId','field'), 2) %>%
  separate(field, c('RestrictionTypeId','flag'), 1) %>%
  mutate(RevenueClassificationId = as.integer(RevenueClassificationId) * 10,
         RevenueClassificationId = ifelse(flag=='a', RevenueClassificationId + 1, RevenueClassificationId),
         RevenueClassificationId = ifelse(flag=='b', RevenueClassificationId + 2, RevenueClassificationId),
         RestrictionTypeId = as.integer(RestrictionTypeId),
         Revenue = ifelse(is.na(Revenue), 0, Revenue)) %>%
  select(Unitid,
         YearId,
         RevenueClassificationId,
         RestrictionTypeId,
         Revenue)

rm(dat)

# Write the outfile.  Empty fields are left null
writeFile(expenses, paste0(paste0(outdir,'fasb_expensedat'), year, ".csv"))
writeFile(finpos, paste0(paste0(outdir,'fasb_finposdat'), year, ".csv"))
writeFile(aid, paste0(paste0(outdir,'fasb_aiddat'), year, ".csv"))
writeFile(revenues, paste0(paste0(outdir,'fasb_revenuedat'), year, ".csv"))

# Open a connection to ipeds database (needs ODBC source named ipeds to work)
con <-odbcConnect(odbcString)

resp <- sqlQuery(con, paste0("DELETE FROM extract.FASBFinancialPosition WHERE YearId=",as.character(year)))

# Append values to appropriate tables.  Run ONCE or there will be a key violation
system.time({sqlSave(con,
        finpos,
        tablename='extract.FASBFinancialPosition',
        append=TRUE,
        rownames=FALSE,
        verbose=FALSE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)
})

# Execute Extract-to-Staging SPROC
resp <- sqlQuery(con, "EXEC operations.TransferFASBFinancialPositionExtractToStagedTable")

rm(finpos)





resp <- sqlQuery(con, paste0("DELETE FROM extract.FASBExpenses WHERE YearId=",as.character(year)))

# Append values to appropriate tables.  Run ONCE or there will be a key violation
system.time({sqlSave(con,
        expenses,
        tablename='extract.FASBExpenses',
        append=TRUE,
        rownames=FALSE,
        verbose=FALSE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)
})

# Execute Extract-to-Staging SPROC
resp <- sqlQuery(con, "EXEC operations.TransferFASBExpensesExtractToStagedTable")

rm(expenses)



resp <- sqlQuery(con, paste0("DELETE FROM extract.FASBStudentAid WHERE YearId=",as.character(year)))

# Append values to appropriate tables.  Run ONCE or there will be a key violation
system.time({sqlSave(con,
        aid,
        tablename='extract.FASBStudentAid',
        append=TRUE,
        rownames=FALSE,
        verbose=FALSE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)
})

# Execute Extract-to-Staging SPROC
resp <- sqlQuery(con, "EXEC operations.TransferFASBStudentAidExtractToStagedTable")

rm(aid)




resp <- sqlQuery(con, paste0("DELETE FROM extract.FASBRevenues WHERE YearId=",as.character(year)))

# Append values to appropriate tables.  Run ONCE or there will be a key violation
system.time({sqlSave(con,
        revenues,
        tablename='extract.FASBRevenues',
        append=TRUE,
        rownames=FALSE,
        verbose=FALSE,
        safer=TRUE,
        fast=TRUE,
        test=FALSE)
})

# Execute Extract-to-Staging SPROC
resp <- sqlQuery(con, "EXEC operations.TransferFASBRevenuesExtractToStagedTable")

rm(revenues)



close(con)
con <- NULL
rm(con)
rm(resp)

# Housekeeping
# rm(list=ls())
