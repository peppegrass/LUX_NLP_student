#' Title: Homework III
#' Purpose: Perform sentiment analysis and cluster analysis
#' Author: Giuseppe Grasso
#' email: giuseppegrasso.econ@gmail.com
#' License: GPL>=3
#' Date: Oct 20, 2021
#'

# Preliminaries

## Setting the working directory
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct18/HW")

## Options
options(scipen = 999, stringsAsFactors = F)

## Libraries
library(skmeans)
library(tidytext)
library(tm)
library(clue)
library(cluster)
library(wordcloud)
library(lexicon)
library(dplyr)
library(plyr)
library(radarchart)
library(ggplot2)
library(ggthemes)
library(webshot)
library(htmlwidgets)

## Custom Functions
source('~/Documents/GitHub/LUX_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

# Examine Raw Text
## incidents_dates.csv
incidents_dates <- read.csv('incident_dates.csv')
incidents_dates$incidents_dates_fmt <- as.POSIXct(incidents_dates$Incident.Date, format = "%m/%d/%y")
incidents_dates$Year <- format(as.Date(incidents_dates$incidents_dates_fmt, format="%d/%m/%y"),"%Y")
incidents_dates <- incidents_dates[,c("incident_ID", "Incident.Date", "Year", "Citation", "Report.Count")]
t(incidents_dates[1,])
## incidents.csv
incidents <- read.csv('incidents.csv')
t(incidents[1:14,])
## joining the two
inc <- full_join(incidents,incidents_dates, by="incident_ID")
#View(inc)
table(inc$Year) # Incidents from the period 2014-2018 have the highest number of entries
inc$Year <- as.numeric(inc$Year)
inc <- subset(inc,Year>=2014 & Year<=2018) # ...therefore, I am subsetting the data to the period 2014-2018
table(inc$Year)
inc <- arrange(inc, Year, incident_ID)
t(inc[1,])

# TM
## Organize into a DF for TM
allInfo <- data.frame(doc_id = 1:nrow(inc),
                      text   = paste0(inc$title, 
                                      inc$Summary_Text),
                      year   = inc$Year)

## Now the TM
stops  <- c(stopwords('SMART'),'chars') # API truncation "[+3394 chars]"

## Process
allInfo    <- VCorpus(DataframeSource(allInfo))
allInfo    <- cleanCorpus(allInfo, stops) 
saveRDS(allInfo, 'allInfo.rds')
allInfo    <- readRDS('allInfo.rds')
allInfoDTM <-  DocumentTermMatrix(allInfo)
allInfoDTM <- as.matrix(allInfoDTM)
allInfoDTM <- subset(allInfoDTM, rowSums(allInfoDTM) > 0)
dim(allInfoDTM) #1,047 entries (summary texts); 9,911 different terms referring to incidents occurred in 2014-2018

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~v

# Perform a Spherical K Means Clustering
set.seed(1234)
txtSKMeans <- skmeans(allInfoDTM, 
                      3, 
                      m = 1, 
                      control = list(nruns = 5, verbose = T))

## Examine Separation
barplot(table(txtSKMeans$cluster), main = 'spherical k-means')
plot(silhouette(txtSKMeans), col=1:2, border=NULL)

## What are the terms of our 4 clusters?
## ID protypical terms
protoTypical           <- t(cl_prototypes(txtSKMeans))
colnames(protoTypical) <- paste0('cluster_',1:ncol(protoTypical))
head(protoTypical)

pdf(file = "incidents_cluster_topics.pdf", 
    width  = 6, 
    height = 6) 
comparison.cloud(protoTypical, title.size=1.1, scale=c(1,.5))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Performing sentiment analysis

## Building a tidy DTM
tidyCorp <- tidy(DocumentTermMatrix(allInfo))
tidyCorp

## Let's understand the meta data of new year
(yearID <- unique(meta(allInfo))) # I'll use this information for the "breaks" field
yearID[,1]

tidyCorp <- as.data.frame(tidyCorp)
tidyCorp$year <- cut(as.numeric(tidyCorp$document), 
                     breaks = c(0,74,226,609,908,1047), 
                     labels = yearID[,1])
tidyCorp[2944:2948,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################################################## NRC LEXICON

## Loading NRC lexicon
nrc     <- get_sentiments(lexicon = c("nrc"))

## Perform the inner joins
nrcSent <- inner_join(tidyCorp,
                      nrc, by=c('term' = 'word'))
head(nrcSent)

# Adjust for quick analysis
sentimentTally <- aggregate(count~sentiment + year, nrcSent, sum)
sentimentTally
year_counts <- aggregate(sentimentTally$count, by=list(year=sentimentTally$year), FUN=sum)
colnames(year_counts) <- c("year","year_count")
year_counts
sentimentShares <- inner_join(sentimentTally, year_counts, by=c('year' = 'year'))
sentimentShares <- mutate(sentimentShares, share=count/year_count)
sentimentShares <- select(sentimentShares, -count, -year_count)
sentimentShares

emos <- reshape2::dcast(sentimentShares, sentiment ~year)
emos

# Make a radarChart
plt <- chartJSRadar(scores = emos, 
             labs = emos$sentiment,
             labelSize = 10, showLegend = F,
             main = "Sentiment shares by year (all 4 clusters; nrc lexicon)")
saveWidget(plt, "radar_allclusters_nrc.html")

# Intersect the Clusters and Sentiment; ID outlier years
clusterProp <- table(data.frame(txtSKMeans$cluster,
                                clusterYear = cut(1:length(txtSKMeans$cluster), 
                                                  breaks = c(0,74,226,609,908,1047), 
                                                  labels = yearID[,1])))
clusterProp <- prop.table(clusterProp, margin = 1)
clusterProp

# Intersect the Clusters and Sentiment; join the clusters
docCluster <- data.frame(document = names(txtSKMeans$cluster), 
                         clusterAssignment = txtSKMeans$cluster)
combinedData <- left_join(nrcSent, docCluster)

######################## CLUSTER 1

# Intersect the Clusters and Sentiment; subset to the cluster of interest
oneTopic <- subset(combinedData, combinedData$clusterAssignment == 1) #GG: cluster 1

# Adjust for quick analysis
table(oneTopic $sentiment, oneTopic $year)
oneEmo <- as.data.frame.matrix(table(oneTopic$sentiment, oneTopic $year))
oneEmo$sum2014 <- sum(oneEmo$`2014`)
oneEmo$sum2015 <- sum(oneEmo$`2015`)
oneEmo$sum2016 <- sum(oneEmo$`2016`)
oneEmo$sum2017 <- sum(oneEmo$`2017`)
oneEmo$sum2018 <- sum(oneEmo$`2018`)
oneEmo
oneEmoShares <- mutate(oneEmo,
                       share2014=`2014`/sum2014,
                       share2015=`2015`/sum2015,
                       share2016=`2016`/sum2016,
                       share2017=`2017`/sum2017,
                       share2018=`2018`/sum2018,
                       )
oneEmoShares <- select(oneEmoShares, share2014, share2015, share2016, share2017, share2018)
oneEmoShares
# Make a radarChart
plt <- chartJSRadar(scores = oneEmoShares, 
             labs = rownames(oneEmoShares),
             labelSize = 10, showLegend = F,
             main = "Sentiment shares by year (cluster 1; nrc lexicon)")
saveWidget(plt, "radar_cluster1_nrc.html")

######################## CLUSTER 2

# Intersect the Clusters and Sentiment; subset to the cluster of interest
twoTopic <- subset(combinedData, combinedData$clusterAssignment == 2) #GG: cluster 2

# Adjust for quick analysis
table(twoTopic $sentiment, twoTopic $year)
twoEmo <- as.data.frame.matrix(table(twoTopic$sentiment, twoTopic $year))
twoEmo$sum2014 <- sum(twoEmo$`2014`)
twoEmo$sum2015 <- sum(twoEmo$`2015`)
twoEmo$sum2016 <- sum(twoEmo$`2016`)
twoEmo$sum2017 <- sum(twoEmo$`2017`)
twoEmo$sum2018 <- sum(twoEmo$`2018`)
twoEmo
twoEmoShares <- mutate(twoEmo,
                       share2014=`2014`/sum2014,
                       share2015=`2015`/sum2015,
                       share2016=`2016`/sum2016,
                       share2017=`2017`/sum2017,
                       share2018=`2018`/sum2018,
)
twoEmoShares <- select(twoEmoShares, share2014, share2015, share2016, share2017, share2018)
twoEmoShares
# Make a radarChart
plt <- chartJSRadar(scores = twoEmoShares, 
                    labs = rownames(twoEmoShares),
                    labelSize = 10, showLegend = F,
                    main = "Sentiment shares by year (cluster 2; nrc lexicon)")
saveWidget(plt, "radar_cluster2_nrc.html")

######################## CLUSTER 3

# Intersect the Clusters and Sentiment; subset to the cluster of interest
threeTopic <- subset(combinedData, combinedData$clusterAssignment == 3) #GG: cluster 3

# Adjust for quick analysis
table(threeTopic $sentiment, threeTopic $year)
threeEmo <- as.data.frame.matrix(table(threeTopic$sentiment, threeTopic $year))
threeEmo$sum2014 <- sum(threeEmo$`2014`)
threeEmo$sum2015 <- sum(threeEmo$`2015`)
threeEmo$sum2016 <- sum(threeEmo$`2016`)
threeEmo$sum2017 <- sum(threeEmo$`2017`)
threeEmo$sum2018 <- sum(threeEmo$`2018`)
threeEmo
threeEmoShares <- mutate(threeEmo,
                         share2014=`2014`/sum2014,
                         share2015=`2015`/sum2015,
                         share2016=`2016`/sum2016,
                         share2017=`2017`/sum2017,
                         share2018=`2018`/sum2018,
)
threeEmoShares <- select(threeEmoShares, share2014, share2015, share2016, share2017, share2018)
threeEmoShares
# Make a radarChart
plt <- chartJSRadar(scores = threeEmoShares, 
                    labs = rownames(threeEmoShares),
                    labelSize = 10, showLegend = F,
                    main = "Sentiment shares by year (cluster 3; nrc lexicon)")
saveWidget(plt, "radar_cluster3_nrc.html")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################################################## AFINN LEXICON

## Loading AFINN lexicon
afinn     <- get_sentiments(lexicon = c("afinn"))

## Perform the inner joins
afinnSent <- inner_join(tidyCorp,
                        afinn, by=c('term' = 'word'))
head(afinnSent)

# Adjust for quick analysis
sentimentTally <- aggregate(count~sentiment + year, afinnSent, sum)
sentimentTally
year_counts <- aggregate(sentimentTally$count, by=list(year=sentimentTally$year), FUN=sum)
colnames(year_counts) <- c("year","year_count")
year_counts
sentimentShares <- inner_join(sentimentTally, year_counts, by=c('year' = 'year'))
sentimentShares <- mutate(sentimentShares, share=count/year_count)
sentimentShares <- select(sentimentShares, -count, -year_count)
sentimentShares

emos <- reshape2::dcast(sentimentShares, sentiment ~year)
emos

# Make a radarChart
plt <- chartJSRadar(scores = emos, 
                    labs = emos$sentiment,
                    labelSize = 10, showLegend = F,
                    main = "Sentiment shares by year (all 4 clusters; afinn lexicon)")
saveWidget(plt, "radar_allclusters_afinn.html")

# Intersect the Clusters and Sentiment; ID outlier years
clusterProp <- table(data.frame(txtSKMeans$cluster,
                                clusterYear = cut(1:length(txtSKMeans$cluster), 
                                                  breaks = c(0,74,226,609,908,1047), 
                                                  labels = yearID[,1])))
clusterProp <- prop.table(clusterProp, margin = 1)
clusterProp

# Intersect the Clusters and Sentiment; join the clusters
docCluster <- data.frame(document = names(txtSKMeans$cluster), 
                         clusterAssignment = txtSKMeans$cluster)
combinedData <- left_join(afinnSent, docCluster)

######################## CLUSTER 1

# Intersect the Clusters and Sentiment; subset to the cluster of interest
oneTopic <- subset(combinedData, combinedData$clusterAssignment == 1) #GG: cluster 1

# Adjust for quick analysis
table(oneTopic $sentiment, oneTopic $year)
oneEmo <- as.data.frame.matrix(table(oneTopic$sentiment, oneTopic $year))
oneEmo$sum2014 <- sum(oneEmo$`2014`)
oneEmo$sum2015 <- sum(oneEmo$`2015`)
oneEmo$sum2016 <- sum(oneEmo$`2016`)
oneEmo$sum2017 <- sum(oneEmo$`2017`)
oneEmo$sum2018 <- sum(oneEmo$`2018`)
oneEmo
oneEmoShares <- mutate(oneEmo,
                       share2014=`2014`/sum2014,
                       share2015=`2015`/sum2015,
                       share2016=`2016`/sum2016,
                       share2017=`2017`/sum2017,
                       share2018=`2018`/sum2018,
)
oneEmoShares <- select(oneEmoShares, share2014, share2015, share2016, share2017, share2018)
oneEmoShares
# Make a radarChart
plt <- chartJSRadar(scores = oneEmoShares, 
                    labs = rownames(oneEmoShares),
                    labelSize = 10, showLegend = F,
                    main = "Sentiment shares by year (cluster 1; afinn lexicon)")
saveWidget(plt, "radar_cluster1_afinn.html")

######################## CLUSTER 2

# Intersect the Clusters and Sentiment; subset to the cluster of interest
twoTopic <- subset(combinedData, combinedData$clusterAssignment == 2) #GG: cluster 2

# Adjust for quick analysis
table(twoTopic $sentiment, twoTopic $year)
twoEmo <- as.data.frame.matrix(table(twoTopic$sentiment, twoTopic $year))
twoEmo$sum2014 <- sum(twoEmo$`2014`)
twoEmo$sum2015 <- sum(twoEmo$`2015`)
twoEmo$sum2016 <- sum(twoEmo$`2016`)
twoEmo$sum2017 <- sum(twoEmo$`2017`)
twoEmo$sum2018 <- sum(twoEmo$`2018`)
twoEmo
twoEmoShares <- mutate(twoEmo,
                       share2014=`2014`/sum2014,
                       share2015=`2015`/sum2015,
                       share2016=`2016`/sum2016,
                       share2017=`2017`/sum2017,
                       share2018=`2018`/sum2018,
)
twoEmoShares <- select(twoEmoShares, share2014, share2015, share2016, share2017, share2018)
twoEmoShares
# Make a radarChart
plt <- chartJSRadar(scores = twoEmoShares, 
                    labs = rownames(twoEmoShares),
                    labelSize = 10, showLegend = F,
                    main = "Sentiment shares by year (cluster 2; afinn lexicon)")
saveWidget(plt, "radar_cluster2_afinn.html")

######################## CLUSTER 3

# Intersect the Clusters and Sentiment; subset to the cluster of interest
threeTopic <- subset(combinedData, combinedData$clusterAssignment == 3) #GG: cluster 3

# Adjust for quick analysis
table(threeTopic $sentiment, threeTopic $year)
threeEmo <- as.data.frame.matrix(table(threeTopic$sentiment, threeTopic $year))
threeEmo$sum2014 <- sum(threeEmo$`2014`)
threeEmo$sum2015 <- sum(threeEmo$`2015`)
threeEmo$sum2016 <- sum(threeEmo$`2016`)
threeEmo$sum2017 <- sum(threeEmo$`2017`)
threeEmo$sum2018 <- sum(threeEmo$`2018`)
threeEmo
threeEmoShares <- mutate(threeEmo,
                         share2014=`2014`/sum2014,
                         share2015=`2015`/sum2015,
                         share2016=`2016`/sum2016,
                         share2017=`2017`/sum2017,
                         share2018=`2018`/sum2018,
)
threeEmoShares <- select(threeEmoShares, share2014, share2015, share2016, share2017, share2018)
threeEmoShares
# Make a radarChart
plt <- chartJSRadar(scores = threeEmoShares, 
                    labs = rownames(threeEmoShares),
                    labelSize = 10, showLegend = F,
                    main = "Sentiment shares by year (cluster 3; afinn lexicon)")
saveWidget(plt, "radar_cluster3_afinn.html")
