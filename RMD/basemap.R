library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(tidyverse)
library(fs)
library(stringr)
library(tigris)
library(raster)
library(grid)
library(ggplot2)
library(magrittr)
library(RColorBrewer)
library(viridis)


msoabounds <- st_read(here::here("../../bigData", 
                                      "statistical-gis-boundaries-london", 
                                      "ESRI", "MSOA_2011_London_gen_MHW.shp"))%>%
  st_transform(., 27700)

census <- read_csv("msoadata.csv")

msoacensus <- geo_join(msoabounds,census,'MSOA11CD','MSOA11CD')


tweet1222 <- st_read(here::here("../1222data.csv")) %>%
  st_as_sf(., coords = c("long", "lat"), 
           crs = 4326)%>%
  st_transform(., 27700)

tweet1222 <- tweet1222[msoabounds,]
tweet1222_latepeak <- tweet1222 %>% slice(1:152,1792:1991)
tweet1222_home <- tweet1222 %>% slice(153:849)
tweet1222_mornpeak <- tweet1222 %>% slice(850:1078)
tweet1222_work <- tweet1222 %>% slice(1079:1792)

dist_1222tw <- data.frame(c('morning peak', 'workplace', 'evening peak', 'home'),
                       c(nrow(tweet1222_mornpeak), nrow(tweet1222_work), 
                         nrow(tweet1222_latepeak), nrow(tweet1222_home)))
colnames(dist_1222tw) <-c('period', 'number_of_tweets')
dist_1222tw$timespan <- c(3,6.5,3,11.5)
dist_1222tw %<>% mutate(density=number_of_tweets/timespan)
dist_1222tw

dist_1222tw$x <- c(1,2,3,4)
x_tick <- c(0, unique(dist_1222tw$x)) + 0.5
len <- length(x_tick)
ggplot(dist_1222tw, aes(x = x, y = density)) + 
  ggtitle("Distribution of number of geo-tagged tweets \nsent per hour") +
  xlab("Time") + ylab("Density of tweets") +
  geom_col(position = "dodge") +
  ylim(0,150) +
  geom_text(data=dist_1222tw, 
            aes(label=round(density)), 
            position = position_dodge(width=0.9),
            vjust=-0.3, size=5) +
  scale_x_continuous(breaks = c(sort(unique(dist_1222tw$x)), x_tick),
                     labels = c('1'='3h','2'='6.5h','3'='3h','4'='11.5h','0.5'='6:30',
                                '1.5'='9:30', '2.5'='16:00', '3.5'='19:00', '4.5'='6:30\nnext day')) +
  theme(axis.ticks.x = element_line(color = c(rep(NA, len - 1), rep("black", len))))

#####################
# base msoa map with points
#tm_shape(msoabounds) +
#  tm_polygons(col = NA, alpha = 0.5) +
#  tm_shape(whichtweet) +
#  tm_dots(col = "blue")

tmplot_tweets <- function(whichtweet){
  
  tweetineachward <- msoabounds%>%
    st_join(whichtweet)%>%
    add_count(MSOA11NM)%>%
    #calculate area
    mutate(area=st_area(.))%>%
    #then density of the points per ward
    mutate(density=n/area)%>%
    #select density and some other variables 
    dplyr::select(density, MSOA11NM, MSOA11CD, n)
  
  tweetineachward<- tweetineachward %>%                    
    group_by(MSOA11CD) %>%         
    summarise(density = first(density),
              wardname= first(MSOA11NM),
              tweetcount= first(n))
  

  t <- tm_shape(tweetineachward) +
    tm_polygons("density",
                style="jenks",
                palette=c('#ffffcc', '#a1dab4', '#41b6c4', '#2c7fb8', '#253494'),
                midpoint=NA,
                popup.vars=c("wardname", "density"),
                title="Tweet Density")
}
  
tmap_home_1222 <- tmplot_tweets(tweet1222_home)
tmap_work_1222 <- tmplot_tweets(tweet1222_work)
tmap_mornpeak_1222 <- tmplot_tweets(tweet1222_mornpeak)
tmap_latepeak_1222 <- tmplot_tweets(tweet1222_latepeak)

tm_shape(msoabounds) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(tweet1222) +
  tm_dots(col = "blue")

tweetineachward <- msoabounds%>%
  st_join(tweet1222)%>%
  add_count(MSOA11NM)%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, MSOA11NM, MSOA11CD, n)

tweetineachward<- tweetineachward %>%                    
  group_by(MSOA11CD) %>%         
  summarise(density = first(density),
            wardname= first(MSOA11NM),
            tweetcount= first(n))

tmap_mode("view")
tm_shape(tweetineachward) +
  tm_polygons("density",
              style="jenks",
              palette=c('#ffffcc', '#a1dab4', '#41b6c4', '#2c7fb8', '#253494'),
              #palette="YlGnBu",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Tweet Density")

######################################

tmap_mode("plot")
tm_work <- tm_shape(msoacensus) + 
  tm_polygons("working_pop",style = "jenks",legend.hist = TRUE) +
  tm_layout(frame=FALSE,legend.outside = TRUE)+
  tm_credits("(a1)", position=c(0,0.85), size=1.5)

tm_res <- tm_shape(msoacensus) + 
  tm_polygons("resident_num",style = "jenks",legend.hist = TRUE) +
  tm_layout(frame=FALSE,legend.outside = TRUE)+
  tm_credits("(a2)", position=c(0,0.85), size=1.5)

tm_tweet <- tm_shape(tweetineachward) + 
  tm_polygons("density",style = "jenks",legend.hist = TRUE) +
  tm_layout(frame=FALSE,legend.outside = TRUE)+
  tm_credits("(tweet density across all users)", position=c(0,0.85), size=1)

tmap_home_1222 <- tmap_home_1222 +
  tm_layout(frame=FALSE,legend.outside = TRUE)+
  tm_credits("(tweet density from home)", position=c(0,0.85), size=1)

tmap_work_1222 <- tmap_work_1222 +
  tm_layout(frame=FALSE,legend.outside = TRUE)+
  tm_credits("(tweet density from work)", position=c(0,0.85), size=1)

tmap_mornpeak_1222 <- tmap_mornpeak_1222 +
  tm_layout(frame=FALSE,legend.outside = TRUE)+
  tm_credits("(tweet density during 6:30-9:30)", position=c(0,0.85), size=1)

tmap_latepeak_1222 <- tmap_latepeak_1222 +
  tm_layout(frame=FALSE,legend.outside = TRUE)+
  tm_credits("(tweet density during 16:00-19:00)", position=c(0,0.85), size=1)

t=tmap_arrange(tm_work, tm_res,tmap_work_1222,tmap_home_1222,ncol=2)
t
alltweetsmap=tmap_arrange(tm_tweet, tmap_mornpeak_1222,tmap_latepeak_1222,
                          tmap_work_1222,tmap_home_1222,ncol=1)

