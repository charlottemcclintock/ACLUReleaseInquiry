
# Charlotte McClintock
# ACLU Request: Bail & Release Times

# ......................................................................................................

# load libraries
library(tidyverse)
library(lubridate)
library(readxl)

# read in the data 
jail <- read_excel("dsd-release.xlsx")

# dimensions
dim(jail)

# clean names
names(jail) <- str_to_lower(names(jail))

jail.og <- jail

# bail posted date and time
jail <- separate(jail, `createdatetime from obp`, into = c("day", "month", "year"), sep="-")
jail <- separate(jail, year, into = c("year", "time", "ampm"), sep=" ", extra = "merge")
jail <- separate(jail, time, into = c("hour", "minute", "second"), sep="\\.", extra = "drop")
jail$cent <- "20"
jail <- unite(jail, "time", hour, minute, second, sep=":")
jail <- unite(jail, "year", cent, year, sep="")
jail <- unite(jail, "date", year, month, day, sep='-')
jail$date <- strptime(jail$date,format="%Y-%b-%d")
jail <- unite(jail, "bailposteddate", date, time, ampm, sep=" ")
jail$bailposteddate <-  ymd_hms(jail$bailposteddate)

# release date and time
jail <- separate(jail, `maxmove_datetime`, into = c("day", "month", "year"), sep="-")
jail <- separate(jail, year, into = c("year", "time", "ampm"), sep=" ", extra = "merge")
jail <- separate(jail, time, into = c("hour", "minute", "second"), sep="\\.", extra = "drop")
jail$cent <- "20"
jail <- unite(jail, "time", hour, minute, second, sep=":")
jail <- unite(jail, "year", cent, year, sep="")
jail <- unite(jail, "date", year, month, day, sep='-')
jail$date <- strptime(jail$date,format="%Y-%b-%d")
jail <- unite(jail, "releasedate", date, time, ampm, sep=" ")
jail$releasedate <-  ymd_hms(jail$releasedate)

# calculate time to release
jail$releasediff <- with(jail, difftime(releasedate, bailposteddate, units="hours"))

# select relevant variables
jail <- select(jail, cd, bailposteddate, releasedate, releasediff, race, gender, 
               `movement reason description`, `release agency`)

# remove duplicate observations
jail <- unique(jail)


# why are there negative values?
jail.neg <- subset(jail, releasediff<0) # 12 negative values


# drop anomalous reasons for release, keep bonds
jail <- rename(jail, "mvmtreason"="movement reason description")
jail.bond <- subset(jail, mvmtreason=="RELEASED - PR BOND GRANTED"|
                      mvmtreason=="RELEASED - PRE-TRIAL SERVICES"|
                      mvmtreason=="RELEASED - BONDED NO CRT ORDERED REST")


jail.high <- subset(jail.bond, releasediff>=48) # 1541 observations
jail.high %>% group_by(mvmtreason) %>% count()

# define modal function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# central tendency measures
mean(jail.bond$releasediff)
median(jail.bond$releasediff)
getmode(jail.bond$releasediff)

# get rid of unreasonable values
jail.bond <- subset(jail.bond, releasediff>0&releasediff<=24)
ggplot(jail.bond, aes(releasediff, fill=mvmtreason)) + geom_density(alpha=0.5)

jail %>% group_by(mvmtreason) %>% summarise(meanreleasetime=mean(releasediff), 
                                                                      count=n())

save.image("jailrelease.RData")
