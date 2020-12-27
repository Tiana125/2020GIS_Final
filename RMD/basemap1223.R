library(sp)
library(spatstat)
library(here)
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
library(fpc)
library(dbscan)
library(OpenStreetMap)
library(factoextra)
library(tidymodels)
library(caret)

msoabounds <- st_read(here::here("../../bigData", 
                                      "statistical-gis-boundaries-london", 
                                      "ESRI", "MSOA_2011_London_gen_MHW.shp"))%>%
  st_transform(., 27700)

pop <- read_csv('land-area-population-density-lsoa11-msoa11.csv')
#https://data.london.gov.uk/dataset/super-output-area-population-lsoa-msoa-london
msoapop <- geo_join(msoabounds,pop,'MSOA11CD','MSOA11CD')

census <- read_csv("msoadata.csv")
msoacensus <- geo_join(msoabounds,census,'MSOA11CD','MSOA11CD')

tweet1222 <- st_read(here::here("../data/1223data.csv")) %>%
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
  
  tweetineachmsoa <- msoapop%>%
    st_join(whichtweet)%>%
    add_count(MSOA11NM)%>%
    #then density of the points per ward
    mutate(density=n/population)%>%
    #select density and some other variables 
    dplyr::select(density, MSOA11NM, MSOA11CD, n)
  
  tweetineachmsoa<- tweetineachmsoa %>%                    
    group_by(MSOA11CD) %>%         
    summarise(density = first(density),
              wardname= first(MSOA11NM),
              tweetcount= first(n))
  
  t <- tm_shape(tweetineachmsoa) +
    tm_polygons("density",
                style="jenks",legend.hist = TRUE,
                palette=c('#ffffcc', '#a1dab4', '#41b6c4', '#2c7fb8', '#253494'),
                midpoint=NA,
                popup.vars=c("wardname", "density"),
                title="Tweet Density")
}

tmap_all_1222 <- tmplot_tweets(tweet1222)
tmap_home_1222 <- tmplot_tweets(tweet1222_home)
tmap_work_1222 <- tmplot_tweets(tweet1222_work)
tmap_mornpeak_1222 <- tmplot_tweets(tweet1222_mornpeak)
tmap_latepeak_1222 <- tmplot_tweets(tweet1222_latepeak)

tm_shape(msoabounds) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(tweet1222) +
  tm_dots(col = "blue")

window <- as.owin(msoabounds)

msoatweetsSub<- tweet1222 %>%
  as(., 'Spatial')

msoatweetsSub.ppp <- ppp(x=msoatweetsSub@coords[,1],
                          y=msoatweetsSub@coords[,2],
                          window=window)
msoatweetsSub.ppp %>%
  plot(.,pch=16,cex=0.5, lwd=.1,
       main="tweets in london")
msoatweetsSub.ppp %>%
  density(., sigma=500) %>%
  plot()

K <- msoatweetsSub.ppp %>%
  Kest(., correction="border") %>%
  plot()

msoatweetsSubpoints <- msoatweetsSub %>%
  coordinates(.)%>%
  as.data.frame()

msoatweetsSubpointsmap <- ggplot(msoatweetsSubpoints, 
                      aes(x=coords.x1,y=coords.x2))+
  geom_point()+
  coord_equal()
msoatweetsSubpointsmap
msoatweetsSubpointsmap+stat_density2d(aes(fill = ..level..), geom="polygon")

msoatweetsSubpointsmap<-ggplot(msoatweetsSubpoints, 
                    aes(x=coords.x1,y=coords.x2))+
  stat_bin2d(bins=10)

msoatweetsSubpointsmap


kmeanstweets <- kmeans(msoatweetsSubpoints, centers = 5)
fviz_cluster(kmeanstweets, data = msoatweetsSubpoints)

centroid <- tidy(kmeanstweets)%>%
  #print the results of the cluster groupings
  print()%>%
  dplyr::select(coords.x1, coords.x2)

p <- ggplot(msoatweetsSubpoints,aes(coords.x1, coords.x2))+
  geom_point(aes(colour=factor(kmeanstweets$cluster)))+
  geom_point(data=centroid,aes(coords.x1, coords.x2), size=7, shape=18)+ theme(legend.position="none")

msoabounds <- kmeanstweets %>% 
  # 
  augment(., msoabounds)%>%
  dplyr::select(MSOA11CD, .cluster)%>%
  #make sure the .cluster column is numeric
  mutate(across(.cluster, as.numeric))%>%
  # join the .cluster to our sf layer
  left_join(msoabounds, 
            .,
            by = c("MSOA11CD" = "MSOA11CD"))






# Split the data:
indxTrain <- createDataPartition(y = msoatweetsSubpoints$coords.x2,
                                 p = 0.75,list = FALSE)
training <- msoatweetsSubpoints[indxTrain,]
testing <- msoatweetsSubpoints[-indxTrain,]

# Run k-NN:
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(coords.x1 ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
knnFit

#Use plots to see optimal number of clusters:
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(knnFit,xlim=c(1,50))
kNNdistplot(msoatweetsSubpoints,k=5)


#now run the dbscan analysis
db <- msoatweetsSubpoints %>%
  fpc::dbscan(.,eps = 1900, MinPts = 5)

#now plot the results
plot(db, msoatweetsSubpoints, main = "DBSCAN Output", frame = F)
plot(msoapop$geometry, add=T)


msoatweetsSubpoints%>%
  dbscan::kNNdistplot(.,k=5)
kNNdistplot(msoatweetsSubpoints,k=5)
msoatweetsSubpoints<- msoatweetsSubpoints %>%
  mutate(dbcluster=db$cluster)

chulls <- msoatweetsSubpoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)

chulls <- chulls %>%
  filter(dbcluster >=1)

dbplot <- ggplot(data=msoatweetsSubpoints, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()

londonmsoaGSbb <- msoapop %>%
  st_transform(., 4326)%>%
  st_bbox()






tweetineachmsoa <- msoapop%>%
  st_join(tweet1222)%>%
  add_count(MSOA11NM)%>%
  #calculate area
  #then density of the points per ward
  mutate(density=n/population)%>%
  #select density and some other variables 
  dplyr::select(density, MSOA11NM, MSOA11CD, n)

tweetineachmsoa<- tweetineachmsoa %>%                    
  group_by(MSOA11CD) %>%         
  summarise(density = first(density),
            wardname= first(MSOA11NM),
            tweetcount= first(n))

tmap_mode("view")

tm_shape(tweetineachmsoa) +
  tm_polygons("density",
              style="jenks",
              palette=c('#ffffcc', '#a1dab4', '#41b6c4', '#2c7fb8', '#253494'),
              #palette="YlGnBu",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Tweet Density")

hist(msoacensus$working_pop,ylim=c(0,10))
hist(tweetineachmsoa$tweetcount)
tweetineachmsoa$tweetcount
msoacensus$working_pop

plot(log(tweetineachmsoa$tweetcount),log(msoacensus$working_pop))




coordstweet <- tweetineachmsoa%>%
  st_centroid()%>%
  st_geometry()

plot(coordstweet,axes=TRUE)

#create a neighbours list
Lmsoa_nb <- tweetineachmsoa %>%
  poly2nb(., queen=T)

#plot them
plot(Lmsoa_nb, st_geometry(coordstweet), col="red")
#add a map underneath
plot(tweetineachmsoa$geometry, add=T)

Lmsoa.lw <- Lmsoa_nb %>%
  poly2nb(., queen=T)

head(Lmsoa.lw$neighbours)

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

tm_tweet <- tm_shape(tweetineachmsoa) + 
  tm_polygons("density",style = "jenks",legend.hist = TRUE) +
  tm_layout(frame=FALSE,legend.outside = TRUE)+
  tm_credits("(tweet density across all users)", position=c(0,0.85), size=1)

tmap_home_1222 <- tmap_home_1222 +
  tm_layout(main.title= 'Density of tweets sent from home', 
            main.title.position = c('center', 'top'), frame=TRUE,
            legend.hist.width = 0.6,legend.hist.height = .3,
            legend.outside.position = "right", legend.outside.size = 0.3,
            legend.outside = TRUE)
#  tm_credits("()", position=c(0,0), size=1)
tmap_mode('view')
tmap_home_1222

tmap_work_1222 <- tmap_work_1222 +
  tm_layout(frame=FALSE,legend.outside = TRUE)+
  tm_credits("(tweet density from work)", position=c(0,0.85), size=1)

tmap_mornpeak_1222 <- tmap_mornpeak_1222 +
  tm_layout(frame=FALSE,legend.outside = TRUE)+
  tm_credits("(tweet density during 6:30-9:30)", position=c(0,0.85), size=1)

tmap_latepeak_1222 <- tmap_latepeak_1222 +
  tm_layout(frame=FALSE,legend.outside = TRUE)+
  tm_credits("(tweet density during 16:00-19:00)", position=c(0,0.85), size=1)

t=tmap_arrange(tmap_mornpeak_1222,tm_res,ncol=1)
t
alltweetsmap=tmap_arrange(tm_tweet, tmap_mornpeak_1222,tmap_latepeak_1222,
                          tmap_work_1222,tmap_home_1222, tm_work, ncol=2)

