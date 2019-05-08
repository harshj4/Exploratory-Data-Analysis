library(rtweet)
library(zipcode)
query <- c("flu","Influenza","H1N1","H3N2","Yamagata","Victoria Lineage","Positive flu test","Influenza A","Influenza B","Influenza Illness","Flu Death")
all_tweets <- search_tweets2(
  q = query, 
  geocode = lookup_coords("usa"), n = 10000,retryonratelimit = TRUE,include_rts = FALSE,type = 'recent',parse = TRUE
)
all_tweets
save_as_csv(all_tweets, "tweets.csv", prepend_ids = TRUE, na = "NA",
            fileEncoding = "UTF-8")
tweet_df <- read.csv("tweets.csv",header = TRUE,sep=",")
locColumn <- tweet_df$location
locColumn <- locColumn[2100:5100]
locColumn <- head(locColumn,n = 2000)
length(locColumn)
install.packages('ggmap')
library(ggmap)
library(plotly)

register_google(key = Sys.getenv("apiKey"))

finalDf <- data.frame(stateCodes = NULL)
for (i in locColumn){
  i
  lonlat_sample <- as.numeric(geocode(i))
  res <- revgeocode(lonlat_sample,output="address")
  res <- as.data.frame(res)
  finalDf <- rbind(finalDf,res)
}
finalDf
write.csv(finalDf,file = "addresses.csv",append=TRUE)
write.table(finalDf,file='states.csv',append=TRUE)

loc_data <- read.csv(file="states.csv",header = TRUE)
length(loc_data)
all_state_codes <- state.abb
flu_df <-data.frame(states = NULL,intensity = NULL)
for (code in all_state_codes){
  res <- grepl(code,loc_data$res,ignore.case = FALSE)
  code_count <- length(res[res=="TRUE"])
  flu_df <- rbind(flu_df,data.frame(code,code_count))
}
flu_df
colnames(flu_df) <- c("states","intensity")
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(flu_df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~code_count, text = ~code_count,locations = ~code,colors = 'Blues')%>%
  layout(geo = list(scope = 'usa',showLegend = TRUE,projection = list(type = 'albers usa')), title = '2018-19 Influenza Season week 4 ILI Activity level indicator \n (Hower to breakdown)')
p

#------------------------------- Heat map using FluView data for 2019 ----------------------------------------------------------------------------

library(plotly)
df <- read.csv("StateWise2019.csv")
state_abbs <- state.abb[match(df$STATENAME,state.name)]
newdf <- data.frame(state = df$STATENAME,levels = substr(df$ACTIVITY.LEVEL,7,8),levelLabel = df$ACTIVITY.LEVEL.LABEL,state_codes = state_abbs)

newdf <- newdf[-c(9,52,53,54),]
newdf
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)


heat_map <- plot_geo(newdf, locationmode = 'USA-states') %>%
  add_trace(
    z = ~levels, text = ~levelLabel,locations = ~state_codes,colors = 'Purples')%>%
  layout(geo = list(scope = 'usa',showLegend = FALSE,projection = list(type = 'albers usa')), title = '2018-19 Influenza Season week 4 ILI Activity level indicator \n (Hower to breakdown)')
heat_map
#------------------------------------ Heat maps using four keywords -------------------------------------------------------

keywords <- c("Influenza","H1N1","H2N2","Yamagata")
all_tweets <- read.csv("tweets.csv",header = TRUE,sep=',')
length(all_tweets)
key_tweets <- all_tweets[all_tweets$query %in% keywords,]
length(key_tweets)
addrs <- key_tweets$location
addrs
register_google(key = Sys.getenv("apiKey"))

key_addr <- data.frame(stateCodes = NULL)
for (j in addrs){
  j
  sample <- as.numeric(geocode(j))
  res <- revgeocode(sample,output="address")
  res <- as.data.frame(res)
  key_addr <- rbind(key_addr,res)
}
key_addr
write.csv(key_addr,file = "key_addresses.csv",append=TRUE)

loc_data <- read.csv(file="key_addresses.csv",header = TRUE)
length(loc_data)
all_state_codes <- state.abb
key_flu_df <-data.frame(states = NULL,intensity = NULL)
for (code in all_state_codes){
  res <- grepl(code,loc_data$x,ignore.case = FALSE)
  code_count <- length(res[res=="TRUE"])
  key_flu_df <- rbind(key_flu_df,data.frame(code,code_count))
}
key_flu_df

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

key_heat <- plot_geo(key_flu_df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~code_count, text = ~code_count,locations = ~code,colors = 'Purples')%>%
  layout(geo = list(scope = 'usa',showLegend = FALSE,projection = list(type = 'albers usa')), title = 'Heatmap for specific keywords')
key_heat
