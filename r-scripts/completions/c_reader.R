library(reshape2)
library(plyr)

specifications <- c("c:/Box Sync/Data/ipeds/c2013_a.csv",
           "c:/Box Sync/Data/ipeds/c2012_a.csv",
           "c:/Box Sync/Data/ipeds/c2011_a.csv",
           "c:/Box Sync/Data/ipeds/c2010_a.csv",
           "c:/Box Sync/Data/ipeds/c2009_a.csv",
           "c:/Box Sync/Data/ipeds/c2008_a.csv",
           "c:/Box Sync/Data/ipeds/c2007_a.csv",
           "c:/Box Sync/Data/ipeds/c2006_a.csv")

years <- c(2013L,
           2012L,
           2011L,
           2010L,
           2009L,
           2008L,
           2007L,
           2006L)

fileInformation <- list(Year=years, Specification=specifications)

ef <- data.frame()

for (i in 1:length(years))
{
  dat <- read.table(file=fileInformation$Specification[[i]],
                    header=TRUE,
                    sep=",",
                    quote="\"",
                    skip=0,
                    row.names=NULL,
                    stringsAsFactors=FALSE,
                    fileEncoding="utf-8")

  names(dat) <- tolower(names(dat))
  
  names(dat)[names(dat) == 'unitid'] <- c('Unitid')
  names(dat)[names(dat) == 'cipcode'] <- c('Cip')
  names(dat)[names(dat) == 'majornum'] <- c('MajorNumber')
  names(dat)[names(dat) == 'awlevel'] <- c('AwardLevel')
  names(dat)[names(dat) == 'crace01'] <- c('cnralm')
  names(dat)[names(dat) == 'crace02'] <- c('cnralw')  
  names(dat)[names(dat) == 'crace13'] <- c('cunknm')
  names(dat)[names(dat) == 'crace14'] <- c('cunknw')
  names(dat)[names(dat) == 'crace15'] <- c('ctotalm')
  names(dat)[names(dat) == 'crace16'] <- c('ctotalw')
  names(dat)[names(dat) == 'crace17'] <- c('cnralt') 
  names(dat)[names(dat) == 'crace23'] <- c('cunknt')
  names(dat)[names(dat) == 'crace24'] <- c('ctotalt')
  # add crace categories
  
  dat$DataYear <- fileInformation$Year[[i]]

  if (dat$DataYear < 2011) {
    dat <- subset(dat, select=c(Unitid,
                              DataYear,
                              Level,
                              TimeStatus,
                              IsDegreeSeeking,
                              EnrollmentType,
                              cnralm,
                              cnralw,
                              cunknm,
                              cunknw,
                              c2morm,
                              c2morw,
                              dvcaim,
                              dvcaiw,
                              dvcapm,
                              dvcapw,
                              dvcbkm,
                              dvcbkw,
                              dvchsm,
                              dvchsw,
                              dvcwhm,
                              dvcwhw))
  } else {
    dat <- subset(dat, select=c(Unitid,
                                DataYear,
                                Level,
                                TimeStatus,
                                IsDegreeSeeking,
                                EnrollmentType,
                                efnralm,
                                efnralw,
                                efunknm,
                                efunknw,
                                efhispm,
                                efhispw,
                                efaianm,
                                efaianw,
                                efasiam,
                                efasiaw,
                                efbkaam,
                                efbkaaw,
                                efnhpim,
                                efnhpiw,
                                efwhitm,
                                efwhitw,
                                ef2morm,
                                ef2morw))    
    
  }
  
  ef <- rbind.fill(ef,dat)
  
  rm(dat)
}

rm(i)
rm(fileInformation)
rm(specifications)
rm(years)

ef <- melt(ef, id.vars=c('Unitid','DataYear','Level','TimeStatus','IsDegreeSeeking','EnrollmentType'), variable.name='Variable',value.name='Headcount')  
# ef <- dcast(ef, Unitid + DataYear + Variable ~ Field, value.var="Count")

ef$RaceEthnicity <- factor(mapvalues(ef$Variable, 
                             c("efnralm",
                               "efnralw",
                               "efrace03",
                               "efrace04",
                               "efrace05",
                               "efrace06",
                               "efrace07",
                               "efrace08",
                               "efrace09",
                               "efrace10",
                               "efrace11",
                               "efrace12",
                               "efunknm",
                               "efunknw",
                               "efhispm",
                               "efhispw",
                               "efaianm",
                               "efaianw",
                               "efasiam",
                               "efasiaw",
                               "efbkaam",
                               "efbkaaw",
                               "efnhpim",
                               "efnhpiw",
                               "efwhitm",
                               "efwhitw",
                               "ef2morm",
                               "ef2morw",
                               "dvefaim",
                               "dvefaiw",
                               "dvefapm",
                               "dvefapw",
                               "dvefbkm",
                               "dvefbkw",
                               "dvefhsm",
                               "dvefhsw",
                               "dvefwhm",
                               "dvefwhw"),
                             c("Nonresident alien",
                               "Nonresident alien",
                               "Black non-Hispanic",
                               "Black non-Hispanic",
                               "American Indian or Alaska Native",
                               "American Indian or Alaska Native",
                               "Asian or Pacific Islander",
                               "Asian or Pacific Islander",
                               "Hispanic",
                               "Hispanic",
                               "White non-Hispanic",
                               "White non-Hispanic",
                               "Race/ethnicity unknown",
                               "Race/ethnicity unknown",
                               "Hispanic",
                               "Hispanic",
                               "American Indian or Alaska Native",
                               "American Indian or Alaska Native",
                               "Asian",
                               "Asian",
                               "Black or African American",
                               "Black or African American",
                               "Native Hawaiian or Other Pacific Islander",
                               "Native Hawaiian or Other Pacific Islander",
                               "White",
                               "White",
                               "Two or more races",
                               "Two or more races",
                               "American Indian or Alaska Native",
                               "American Indian or Alaska Native",
                               "Asian/Native Hawaiian/Other Pacific Islander",
                               "Asian/Native Hawaiian/Other Pacific Islander",
                               "Black or African American/Black non-Hispanic",
                               "Black or African American/Black non-Hispanic",
                               "Hispanic or Latino/Hispanic",
                               "Hispanic or Latino/Hispanic",
                               "White/White non-Hispanic",
                               "White/White non-Hispanic")))

ef$Sex <- factor(mapvalues(ef$Variable, 
                                      c("efnralm",
                                        "efnralw",
                                        "efrace03",
                                        "efrace04",
                                        "efrace05",
                                        "efrace06",
                                        "efrace07",
                                        "efrace08",
                                        "efrace09",
                                        "efrace10",
                                        "efrace11",
                                        "efrace12",
                                        "efunknm",
                                        "efunknw",
                                        "efhispm",
                                        "efhispw",
                                        "efaianm",
                                        "efaianw",
                                        "efasiam",
                                        "efasiaw",
                                        "efbkaam",
                                        "efbkaaw",
                                        "efnhpim",
                                        "efnhpiw",
                                        "efwhitm",
                                        "efwhitw",
                                        "ef2morm",
                                        "ef2morw",
                                        "dvefaim",
                                        "dvefaiw",
                                        "dvefapm",
                                        "dvefapw",
                                        "dvefbkm",
                                        "dvefbkw",
                                        "dvefhsm",
                                        "dvefhsw",
                                        "dvefwhm",
                                        "dvefwhw"),
                                      c("Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women",
                                        "Men",
                                        "Women")))

ef <- subset(ef, Headcount > 0, select=c("Unitid","DataYear","RaceEthnicity","Sex", "Level", "TimeStatus", "IsDegreeSeeking", "EnrollmentType", "Headcount"))

write.table(ef,
            file="c:/Box Sync/Data/ipeds/ef_dat.csv",
            append=FALSE,
            quote=TRUE,
            sep=",",
            row.names=FALSE,       # If you want to create row numbers, set this to TRUE
            col.names=TRUE,
            na = "")               # Set the missing values to blanks
