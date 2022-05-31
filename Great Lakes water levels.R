# website for tutorial: http://hselab.org/using-r-to-download-and-plot-great-lakes-historical-water-level-data.html

install.packages("readr")
install.packages("lubridate")
install.packages("stringr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
library(readr)
library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)

data_loc <- "https://www.glerl.noaa.gov/data/dashboard/data/levels/1918_PRES/"

miHuron1918 <- read_csv(str_c(data_loc,"miHuron1918.csv"), skip = 2)
ontario1918 <- read_csv(str_c(data_loc, "ontario1918.csv"), skip = 2)
erie1918 <- read_csv(str_c(data_loc, "erie1918.csv"), skip = 2)
superior1918 <- read_csv(str_c(data_loc, "superior1918.csv"), skip = 2)
clair1918 <- read_csv(str_c(data_loc, "ontario1918.csv"), skip = 2)

head(miHuron1918)
str(miHuron1918)

# need to reshape data into long format to facilitate ggplot2

miHuron1918_long <- gather(miHuron1918, "month", "mihuron", 2:13)
ontario1918_long <- gather(ontario1918, "month", "ontario", 2:13)
erie1918_long <- gather(erie1918, "month", "erie", 2:13)
superior1918_long <- gather(superior1918, "month", "superior", 2:13)
clair1918_long <- gather(clair1918, "month", "clair", 2:13)

head(erie1918_long)

# create a date column for joining individual tables and proper sorting

miHuron1918_long$date <- as.POSIXct(str_c(miHuron1918_long$month, " 1, ",
                                                miHuron1918_long$year),
                                    format="%b %d, %Y")
head(miHuron1918_long)
ontario1918_long$date <- as.POSIXct(str_c(ontario1918_long$month, " 1, ",
                                          ontario1918_long$year),
                                    format="%b %d, %Y")
erie1918_long$date <- as.POSIXct(str_c(erie1918_long$month, " 1, ",
                                          erie1918_long$year),
                                    format="%b %d, %Y")
superior1918_long$date <- as.POSIXct(str_c(superior1918_long$month, " 1, ",
                                          superior1918_long$year),
                                    format="%b %d, %Y")
clair1918_long$date <- as.POSIXct(str_c(clair1918_long$month, " 1, ",
                                          clair1918_long$year),
                                    format="%b %d, %Y")

head(miHuron1918_long)
head(erie1918_long)

#drop year and month columns as they are no longer needed, move date to left most column

miHuron1918_long <- miHuron1918_long[, c(4,3)]
ontario1918_long <- ontario1918_long[, c(4,3)]
erie1918_long <- erie1918_long[, c(4,3)]
superior1918_long <- superior1918_long[, c(4,3)]
clair1918_long <- clair1918_long[, c(4,3)]

head(miHuron1918_long)

#join long format taables together using new date field

lakes_long <- merge(miHuron1918_long, ontario1918_long, by = "date")
lakes_long <- merge(lakes_long, erie1918_long, by = "date")
lakes_long <- merge(lakes_long, superior1918_long, by = "date")
lakes_long <- merge(lakes_long, clair1918_long, by = "date")

head(lakes_long)

#get dataframe in "tidy" format by creating a lake field and gathering the lake fields

lake_level <- gather(lakes_long, 2:6, key = "lake", value = "waterlevelm")
lake_level <- lake_level %>%
  arrange(lake, date)
head(lake_level)

#clean up workspace by getting rid of unneeded dataframes

rm(miHuron1918, ontario1918, erie1918, superior1918, clair1918)
rm(miHuron1918_long, ontario1918_long, erie1918_long, superior1918_long,
   clair1918_long)
rm(lakes_long)

#plotting shows the differences, but intra-lake variation is hidden by the scale

ggplot(lake_level) + geom_line(aes(x=date, y = waterlevelm, color = lake)) +
  ylab("Water level (m)")

# Lake St Clair likely hidden under Lake Ontario

lake_level %>% 
  filter(lake == 'clair' | lake == 'ontario') %>%
ggplot() + geom_line(aes(x=date, y = waterlevelm, color = lake)) +
  ylab("Water Level (m)")

#measurements are identical for Lake St. Clair and Lake Ontario

all(lake_level[lake_level$lake == 'ontario' & !is.na(lake_level$waterlevelm), 3] == lake_level[lake_level$lake == 'clair' & !is.na(lake_level$waterlevelm),3])

#looks identical, let's drop Lake St Clair

lake_level <- lake_level %>%
  filter(lake != 'clair')

#find the latest month of data and save a version tagged by month

last_month <- max(lake_level[!is.na(lake_level$waterlevelm),"date"])
last_month

#also can use dplyr

last_month_d <- lake_level %>%
  filter(!is.na(waterlevelm)) %>%
  select(date) %>%
  summarize(whichmonth = max(date)) %>%
  pull()
last_month_d

#save as rds file

rdsname <- paste0("data/lake_level_", year(last_month), strftime(last_month,"%m"), ".rds")
saveRDS(lake_level, rdsname)


###### PLOTTING MONTHLY WATER LEVEL DATA ################

ts_all <- ggplot(lake_level) +
  geom_line(aes(x=date, y = waterlevelm, color = lake)) +
  ylab("Water level (m)") +
  facet_grid(lake~., scales = "free") +
  theme(strip.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.position = "none")
ts_all  

lake_level_stats <- lake_level %>%
  group_by(lake) %>%
  summarize(
    mean_level = mean(waterlevelm, na.rm = TRUE),
    min_level = min(waterlevelm, na.rm = TRUE),
    p05_level = quantile(waterlevelm, 0.05, na.rm = TRUE),
    p95_level = quantile(waterlevelm, 0.95, na.rm = TRUE),
    sd_level = sd(waterlevelm, na.rm = TRUE),
    cv_level = sd_level / mean_level
  )

lake_level_stats

# Combine time series with lines from the stats just created, starting with one lake

lake_level_huron <- lake_level %>%
  filter(lake == 'mihuron')

lake_level_stats_huron <- lake_level_stats %>%
  filter(lake == 'mihuron')

ggplot(lake_level_huron, aes(x = date, y = waterlevelm, color = lake)) +
  geom_line() + geom_hline(yintercept = lake_level_stats_huron$mean_level)

# Create a merged dataframe containing montly data and historical statistics

lake_level_ts_stats <- merge(x = lake_level, y = lake_level_stats) %>%
  arrange(lake, date)

head(lake_level_ts_stats[lake_level_ts_stats$lake == 'mihuron', 1:8])

# Add overall mean and percentile baands to the plot

ggplot(lake_level_ts_stats, aes(x = date, y = waterlevelm, color = lake)) +
  facet_grid(lake ~ ., scales = "free") +
  geom_line() +
  geom_line(aes(y = mean_level)) +
  geom_line(linetype = "dashed", aes(y = p05_level)) +
  geom_line(linetype = "dashed", aes(y = p95_level)) +
  theme(strip.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size =11),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "none")
