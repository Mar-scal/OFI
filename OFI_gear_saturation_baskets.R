### Summary of survey data in preparation for OFI meeting

require(plyr)
require(dplyr)
require(ggplot2)
require(sf)
require(tidyverse)
require(tidyr)
require(ROracle)
require(reshape2)
require(lubridate)
require(ggridges)
require(ggrepel)

## RUN THIS 
options(scipen=999)

load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")

GB <-  read.csv("Y:/Offshore/Assessment/Data/Survey_data/2019/Spring/GB/Survey1984-2019.csv")
GBb <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2019/Summer/GBb/Survey1984-2019.csv")
GBa <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2019/Summer/GBa/Survey1984-2019.csv")
BBn <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2019/Spring/BBn/Survey1991-2019.csv")

surveydat <- join(GB, GBb, type="full")
surveydat <- join(surveydat, GBa, type="full")
surveydat <- join(surveydat, BBn, type="full")

# read in the shps
bbn_shp <- st_read("Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/offshore_survey_strata/BBn.shp")
gba_shp <- st_read("Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/offshore_survey_strata/GBa.shp")

### TO THIS



# plot the commercial abundance in each tow by year
ggplot() +
  geom_point(data=surveydat[surveydat$state=="live",], aes(year, com)) +
  theme_bw() +
  facet_wrap(~bank)

# plot the survey tow locations by year
ggplot() + 
  geom_sf(data = st_cast(bbn_shp, "MULTIPOLYGON"), aes(fill=as.factor(ID)), colour=NA, alpha=0.4) +
  geom_point(data=surveydat[surveydat$bank=="BBn",], aes(lon, lat)) +
  facet_wrap(~year) +
  theme_void()
ggplot() + 
  geom_sf(data = st_cast(gba_shp, "MULTIPOLYGON"), aes(fill=as.factor(ID)), colour=NA, alpha=0.4) +
  geom_point(data=surveydat[surveydat$bank=="GBa",], aes(lon, lat)) +
  facet_wrap(~year) +
  theme_void()
  

bms <- surveydat %>%
  filter(state=="live") %>%
  select(ID, year, tow, cruise, bank, date, lon, lat, state, pre.bm, rec.bm, com.bm, tot.bm) %>%
  pivot_longer(cols=c(pre.bm, rec.bm, com.bm, tot.bm))

bms$name <- factor(bms$name, levels=c("pre.bm", "rec.bm", "com.bm", "tot.bm"))

# density plot
ggplot() + geom_density(data=bms[!bms$name %in% "tot.bm",], aes(value, fill=name), alpha=0.5, colour=NA)+ #,
                          #bins=50, position="dodge") + 
  theme_bw() +
  scale_x_log10() +
  facet_wrap(~bank)

# histogram
ggplot() + geom_histogram(data=bms[!bms$name %in% "tot.bm",], aes(value, fill=name), alpha=0.5,
  bins=50, position="stack") + 
  theme_bw() +
  scale_x_log10() +
  facet_wrap(~bank)


unique(survey.obj$BBn$model.dat$RS)
unique(survey.obj$BBn$model.dat$CS)
# BBn recruit size bin is 85-95 mm

unique(survey.obj$GBa$model.dat$RS[survey.obj$GBa$model.dat$year==2019])
unique(survey.obj$GBa$model.dat$CS[survey.obj$GBa$model.dat$year==2019])
# GBa recruit size bin is 85-95 mm

ggplot() + geom_histogram(data=bms[bms$name %in% "tot.bm" & bms$year>1999 & bms$value>=1,], aes(value), position="stack", colour="black", breaks=c(1,5,10,20,50,75,100)) + 
  theme_bw() +
  scale_x_log10(breaks=c(1,5, 10, 20,50, 75,100)) +
  facet_grid(year~bank)

# ggplot() + geom_density_ridges(data=bms[bms$name %in% "tot.bm",], aes(x=value, y=year)) + 
#   theme_bw() +
#   #scale_x_log10(breaks=c(0,0.1, 1,2,5, 10, 20,50, 75, 100)) +
#   facet_wrap(~bank) +
#   scale_fill_viridis_d(option = "D") +
#   coord_flip()

ggplot() + geom_histogram(data=bms[bms$name %in% "com.bm",], aes(value, fill=as.factor(year)), position="stack",
                          bins=50) + 
  theme_bw() +
  scale_x_log10(breaks=c(0,0.1, 1,2,5, 10, 20,50, 75, 100))



ggplot() + geom_histogram(data=bms[bms$name %in% "tot.bm",], aes(value), 
                          bins=50) + 
  theme_bw() 
  



#### BASKETS

basketdat <- NULL

for(i in 1:length(unique(all.surv.dat[all.surv.dat$year>1999,]$cruise))){
  
  cruise <- as.character(unique(all.surv.dat[all.surv.dat$year>1999,]$cruise)[i])
  print(cruise)
  ### read in OSSURVEYS, OSTOWS and OSHFREQ_SAMPLES
  chan <-dbConnect(dbDriver("Oracle"),username="scaloff", password="fgb256k","ptran")
  
  #####################################################################################################################
  # Jessica has new views for these calls, ,all this prorating is not necessary anymore as she's taken care of it in SQL
  # Key is to import those tables and send it out of this file looking identical!  
  ######################################################################################################################
  db <- "SCALOFF" ### CHANGE HUMF TO SCALOFF!!!
  #message("reminder that this is pulling data from HUMF views, not production SCALOFF")
  
  #qu.strata <- "select * from SCALOFF.OSSTRATA"
  # DK Oct 29, 2015, don't need tow data either, we don't ever use it.... 
  qu.surveys <- paste0("select * from ", db, ".OSSURVEYS")
  qu.surveys<- dbGetQuery(chan, qu.surveys)
  
  survey_seq <- paste(as.character(unique(qu.surveys[qu.surveys$CRUISE == cruise,]$SURVEY_SEQ)), sep="' '", collapse=", ")
  qu.tows <- paste0("select * from ", db, ".OSTOWS WHERE SURVEY_SEQ in (", survey_seq, ")")
  qu.tows<- dbGetQuery(chan, qu.tows)
  
  tow_seq <- paste(as.character(unique(qu.tows$TOW_SEQ)), sep="' '", collapse=", ")
  qu.hfreq <- paste0("select * from ", db, ".OSHFREQSAMPLES WHERE TOW_SEQ in (", tow_seq, ")")
  qu.hfreq<- dbGetQuery(chan, qu.hfreq)
  
  hfreq_seq <- paste(as.character(unique(qu.hfreq$HFREQ_SAMPLE_SEQ)), sep="' '", collapse=", ")
  qu.heightfreq <- paste0("select * from ", db, ".OSHEIGHTFREQ WHERE HFREQ_SAMPLE_SEQ in (", hfreq_seq, ")")
  qu.heightfreq<- dbGetQuery(chan, qu.heightfreq)
  dbDisconnect(chan)
  
  surv_tows <- join(qu.tows, qu.surveys, type="left", by="SURVEY_SEQ")
  # surv_tows <- rbind(data.frame(surv_tows, LIVECODE="L"), data.frame(surv_tows, LIVECODE="D"))
  surv_tows_samp <- join(surv_tows, qu.hfreq, type="left", by="TOW_SEQ")
  surv_tows_samp_hf <- join(surv_tows_samp, qu.heightfreq, type="left", by="HFREQ_SAMPLE_SEQ")
  
  basketdat <- rbind(basketdat, surv_tows_samp_hf)
  
}

basketdat$YEAR <- year(basketdat$TOW_DATE)

basketdat <- basketdat[,which(!names(basketdat) %in% "COMMENTS")]

basketdat <- basketdat[basketdat$MGT_AREA_CD %in% c("GBa", "GBb","BBn"),] # Note, "GB" is not considered a management area in the db, whereas it is a "bank" in all.surv.dat

basketdat <- dplyr::select(basketdat, -TOW_SEQ, -SURVEY_SEQ, -BOTTOM_TYPE_ID, -BOTTOM_TEMP, -HFREQ_SAMPLE_SEQ, -HEIGHT_FREQ_SEQ, -BIN_ID, -NUMBER_IN_BIN)

basketdat <- unique(basketdat)

basketdat$month <- month(basketdat$TOW_DATE)

# these are ALL THE TOWS (regardless of catch). BBn (2001+), GBa and GBb summer (2000+), GB spring (2001+) 
baskettow <- unique(dplyr::select(basketdat, -NUM_OF_CONTAINERS, -SAMPLED, -TOTAL, -CONTAINER_TYPE_ID, -SPECIES_ID, -LIVECODE))

basketdat_s <- basketdat %>%
  filter(NUM_OF_CONTAINERS <99) %>%
  filter(!is.na(LIVECODE)) %>%
  filter(LIVECODE == "L") %>%
  filter(CONTAINER_TYPE_ID == 1) %>%
  group_by(YEAR, MGT_AREA_CD, TOW_NO, START_LON, START_LAT, month) %>%
  summarise(baskets=sum(NUM_OF_CONTAINERS))

basketdat_s$lon <- convert.dd.dddd(basketdat_s$START_LON)
basketdat_s$lat <- convert.dd.dddd(basketdat_s$START_LAT)

# basketdat_s[basketdat_s$YEAR==2018 & basketdat_s$TOW_NO %in% c(304,306,312) & basketdat_s$month==5,]$MGT_AREA_CD <- "GBb"
# basketdat_s[basketdat_s$YEAR==2018 & basketdat_s$TOW_NO %in% c(301,309,321) & basketdat_s$month==5,]$MGT_AREA_CD <- "GBa"

basketdat_s <- basketdat_s[basketdat_s$MGT_AREA_CD == "BBn" | (basketdat_s$MGT_AREA_CD %in% c("GBa", "GBb") & basketdat_s$month > 6),]

# baskets histogram
png("Y:/Projects/OFI/Results/Figures/GBa_baskets_histogram.png", height=6, width=10, units="in", res=400)
ggplot() + geom_histogram(data=basketdat_s[basketdat_s$YEAR > 2011 & !basketdat_s$MGT_AREA_CD%in% c("GBb", "BBn") & basketdat_s$baskets>5,], 
                          aes(baskets), breaks=seq(6,44,2), fill="grey", colour="black", closed="left") + 
  facet_wrap(~YEAR, nrow=2) +
  theme_bw() + theme(panel.grid=element_blank())+
  ylab("Number of tows") +
  xlab("Number of baskets per tow (>5)") +
  scale_x_continuous(breaks=seq(6,44,4)) +
  ggtitle("Georges Bank 'a', summer surveys")
dev.off()  

source("Y:/Offshore/Assessment/Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r")

colours <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/survey_information.csv")

# spatial plot of tows with >20 baskets
png("Y:/Projects/OFI/Results/Figures/GBa_baskets_pointsize.png", height=4, width=11, units="in", res=400)
ggplot() + 
  geom_sf(data = st_cast(gba_shp, "MULTIPOLYGON"), aes(fill=as.factor(ID)), colour=NA) +
  geom_point(data=basketdat_s[basketdat_s$YEAR > 2011 & basketdat_s$baskets>=20 & basketdat_s$MGT_AREA_CD=="GBa",], 
            aes(lon, lat, size=baskets), shape=21, fill="white", alpha=0.75) + 
  facet_wrap(~YEAR, nrow=2) +
  theme_bw() +theme(panel.grid.major = element_line(colour = "transparent")) +
  scale_fill_manual(values=c(as.character(colours[colours$label=="GBa",]$col)), guide=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_size_continuous(name="Number of baskets (\u2265 20)")+
  scale_x_continuous(breaks=seq(-67, -66, 0.5)) +
  scale_y_continuous(limits=c(41.8, 42.2), breaks=seq(41.9, 42.1, 0.2))
dev.off()

bigtows <- basketdat_s[basketdat_s$YEAR > 2015 & basketdat_s$MGT_AREA_CD=="GBa",]

bigtows_20 <- bigtows[bigtows$baskets>=20,]
bigtows_20$atleast <- "\u2265 20 baskets"
bigtows_30 <- bigtows[bigtows$baskets>=30,]
bigtows_30$atleast <- "\u2265 30 baskets"
bigtows_25 <- bigtows[bigtows$baskets>=25,]
bigtows_25$atleast <- "\u2265 25 baskets"


bigtows <- rbind(bigtows_20, bigtows_30, bigtows_25)

# spatial plot of tows with >20 baskets
png("Y:/Projects/OFI/Results/Figures/GBa_baskets_facetted.png", height=7, width=11, units="in", res=400)
ggplot() + 
  geom_sf(data = st_cast(gba_shp, "MULTIPOLYGON"), aes(fill=as.factor(ID)), colour=NA) +
  geom_point(data=bigtows, 
            aes(lon, lat), shape=21, fill="white", size=2) + 
  facet_grid(atleast~YEAR) +
  theme_bw() +theme(panel.grid.major = element_line(colour = "transparent")) +
  scale_fill_manual(values=c(as.character(colours[colours$label=="GBa",]$col)), guide=F) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_x_continuous(breaks=seq(-67, -66, 0.5)) +
  scale_y_continuous(limits=c(41.8, 42.2), breaks=seq(41.9, 42.1, 0.2))
dev.off()

gba_baskets <- basketdat_s[basketdat_s$MGT_AREA_CD=="GBa",]
gba_survey <- surveydat[surveydat$bank=="GBa",]

names(gba_baskets) <- c("year", "bank", "tow", "START_LON", "START_LAT", "month", "baskets", "s_lon", "s_lat")

# NOTE: gba_baskets does not contain empty tows. I expect there to be this many empty tows after the join:
dim(gba_survey[gba_survey$state=="live" & gba_survey$year>2011 & gba_survey$survey=="summer" & gba_survey$tot==0,])[1]

gba_join <- join(gba_survey[gba_survey$state=="live" & gba_survey$year>2011 & gba_survey$survey=="summer",], gba_baskets[gba_baskets$year>2011,], type="full")

dim(gba_join[is.na(gba_join$baskets),])

gba_join$isna <- is.na(gba_join$baskets)
gba_join$is0 <- gba_join$tot ==0

ggplot() + geom_point(data=gba_join[gba_join$year%in%2017:2018,], aes(tow, isna, colour=is0)) + 
  facet_wrap(~year)

gba_join[(gba_join$is0==FALSE & gba_join$isna==TRUE),]

# there are 2 tows (1 in 2017 and 1 in 2018) that caught buckets (pre recruits) but no baskets. Otherwise, we have 33 empty tows, which matches the baskets data


# scatterplot
png("Y:/Projects/OFI/Results/Figures/GBa_baskets_scatter.png", height=6, width=8, units="in", res=400)
ggplot() + geom_point(data=gba_join[gba_join$year>2011,], aes(tot.bm, baskets)) + theme_bw() + theme(panel.grid=element_blank()) + 
  ylab("Number of baskets per tow") +
  xlab("Total biomass per tow") + 
  geom_smooth(data=gba_join, aes(tot.bm, baskets), method="lm") +
  ggtitle("Georges Bank 'a' (2012-2019)")
dev.off()

summary(gba_join$tot.bm/gba_join$baskets)

summary(gba_join$baskets)

summary(gba_join$tot.bm)


