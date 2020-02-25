---
  title: "DFO Survey Results"
runtime: shiny
output: 
  flexdashboard::flex_dashboard:
  orientation: rows
vertical_layout: fill
---
  
```{r setup, include=FALSE}
library(flexdashboard)
require(crosstalk)
require(DT)
require(tidyverse)
require(shiny)
require(tidyr)
library(sf)

# Clean this up for the meeting with the group to highlight what biomass/volume seems to be
# where we might be experiencing, show how frequently we experience this, highlighter
# there could actually currently be an area that this could currently work, though there
# has been some fishing here, so could be too late.  A pilot in the area may be worthwhile
# 

gba <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2019/Summer/GBa/Survey1984-2019.csv")
gbb <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2019/Summer/GBb/Survey1984-2019.csv")
bbn <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2019/Spring/BBn/Survey1991-2019.csv")
source("Y:/Offshore/Assessment/Assessment_fns/Maps/pectinid_projector.R")
source("Y:/Offshore/Assessment/Assessment_fns/Fishery/logs_and_fishery_data.r")

dat <- rbind(gba,gbb)
dat$bank <- "GB"
bbn$bank <- "BBn"
dat <- rbind(dat,bbn)

dat <- dat %>% filter(state=='live')
dat$rcs <- dat$com + dat$rec
head(dat)

# Now convert SH's to an approximate volume
multiplier <- seq(.25,19.75,by=5)^3
volumes <- dat[,grep("^h",names(dat))] * multiplier

names(volumes) <- paste0("v",substr(names(volumes),2,4))

volumes$tot.vol <- rowSums(volumes)

dat <- cbind(dat,volumes)
dat$tot.vol <- dat$tot.vol/1e6

dat.sf <- st_as_sf(dat,coords = c("lon","lat"))
st_crs(dat.sf) <- 4326 # Assuming it's all WGS84...
# Now grab the fishery data
logs_and_fish(loc='offshore',year = 2015:2019,direct = "Y:/Offshore/Assessment/")
log.dat <- new.log.dat %>% dplyr::filter(bank %in% c("GBa","GBb","BBn")) %>% dplyr::filter(year ==2019) %>% dplyr::filter(kg.h<4000)
log.dat <- st_as_sf(log.dat,coords = c('lon','lat'))
st_crs(log.dat) <- 4326 # Assuming it's all WGS84...

log.dat$bank[log.dat$bank %in% c("GBa","GBb")] <- "GB"

# OK, ready to go, now let's turn these plots into a nice dashboard...
```

Column {.sidebar}
-----------------------------------------------------------------------
  
  ### Select:
  
  ```{r}
selectInput("bank", "Survey_bank",  dat.sf$bank)

```

# 
# windows(11,11)
# ggplot(dat) + geom_point(aes(x = year, y=com))
# ggplot(dat) + geom_point(aes(x = year, y=pre))
# ggplot(dat) + geom_point(aes(x = year, y=rec))
# 
# # Abundances
# ggplot(dat) + geom_histogram(aes(com),bins=50)
# ggplot(dat) + geom_histogram(aes(pre),bins=50)
# ggplot(dat) + geom_histogram(aes(rec),bins=50) + scale_x_log10()
# ggplot(dat) + geom_histogram(aes(tot),bins=50) + scale_x_log10()
# ggplot(dat) + geom_histogram(aes(rcs),bins=50) + scale_x_log10()
# 
# #Biomasses
# 
# ggplot(dat) + geom_histogram(aes(com.bm),bins=50)+ scale_x_log10(breaks = c(0.1,1,2,5,10,20,50,75,100))
# ggplot(dat) + geom_histogram(aes(tot.bm),bins=50)+ scale_x_log10(breaks = c(0.1,1,2,5,10,20,50,75,100))
# 
# ggplot(dat %>% filter(year > 2000)) + geom_histogram(aes(tot.bm),bins=20)+ 
#   scale_x_log10(breaks = c(2,5,10,20,50,75,90,100),limits = c(5,100)) + 
#               facet_wrap(~year) + geom_vline(xintercept=75,size=1,colour='blue',alpha=0.2)
# 
# 
# 
# # Don't see much past 5 m^3 let's use those volumes
# ggplot(dat) + geom_histogram(aes(tot.vol),bins=500) + scale_x_log10(breaks= c(0.1,1,2,5))
# ggplot(dat) + geom_point(aes(tot.bm,tot.vol))
# 
# high <- dat %>% filter(tot.vol > 5)
# high <- dat %>% filter(tot.bm >= 50)
# 
# 
# tst <- st_as_sf(high,coords = c("lon","lat"))
# 
# 
# tst <- tst %>% filter(year >= 2018)
# 
# ggplot(tst %>% filter(year > 2015)) +  geom_sf_text(aes(label=substr(year,3,4)),size=4)
# 
# pecjector(area= "GB",plot_package="base",add_EEZ=T,add_nafo='sub',add_strata = T)
# points(tst$slon,tst$slat,pch=19)
# 
# 

# ggplot(gb.log.dat) + geom_sf(aes(fill = kg.h,colour=kg.h)) +
#       scale_fill_viridis_c(direction=-1)+ scale_colour_viridis_c(direction=-1) +
#         geom_sf(data=tst)
# 
# ggplot(gb.log.dat) + geom_histogram(aes(kg.h)) + scale_x_log10()
# 
# 
# ```
