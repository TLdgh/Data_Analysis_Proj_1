library(ggplot2)
library(janitor)
library(dplyr)
require(maps)
require(viridis)
poll_data<-ElectionPoll
summary(poll_data)
summ<-tabyl(poll_data,state)
n_state<-nrow(summ)
state_us<-summ$state
state_poll<-matrix(0,58,5)
state_poll[1,2:5]<-c("raw_clinton","raw_trump","adj_clinton","adj_trump")
state_poll[2:58,1]<-state_us
head(poll_data)

for(i in 1:n_state){
  mat<-poll_data[poll_data$state==state_us[i],]
  state_poll[i+1,2]<-mean(mat[,8])
  state_poll[i+1,3]<-mean(mat[,9])
  state_poll[i+1,4]<-mean(mat[,12])
  state_poll[i+1,5]<-mean(mat[,13])
  
}
head(state_poll)
as.numeric(state_poll[2:58,2:3])

#按百分比
#state_poll_result<-matrix(0,58,3)
#state_poll_result[1,]<-c("region","Predic_Result","precentage of C")
#state_poll_result[2:58,1]<-state_us

#for(i in 1:n_state){
  #state_poll[1+i,2]=as.numeric(state_poll[1+i,2])
  #state_poll[1+i,3]=as.numeric(state_poll[1+i,3])
  #if(state_poll[1+i,2]<state_poll[1+i,3]){
    #state_poll_result[i+1,3]<-state_poll[i+1,2]/(state_poll[i+1,2]+state_poll[i+1,3])*100
    #state_poll_result[i+1,2]<-"C"
  #}
  #else{
    #state_poll_result[i+1,3]<-state_poll[i+1,2]/(state_poll[i+1,2]+state_poll[i+1,3])*100
    #state_poll_result[i+1,2]<-"T"
  #}
#}
#state_poll_result

#按照结果
state_poll_result<-matrix(0,58,2)
state_poll_result[1,]<-c("region","Predic_Result")
state_poll_result[2:58,1]<-state_us
for(i in 1:n_state){
  if(state_poll[1+i,2]<state_poll[1+i,3]){
    state_poll_result[i+1,2]<-"C"
  }
  else{
    state_poll_result[i+1,2]<-"T"
  }
}
head(state_poll_result)

# 将观测数据合并进地图数据
states_map <- map_data(map = "state",region =state_poll_result[2:58,1])

# 将观测数据合并进地图数据

region.lab.data <- states_map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
ggplot(states_map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")







##################################Teng's suggestion
#get example data from a link:
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/us-cities-top-1k.csv")
df<-distinct(df,State, .keep_all = TRUE)
df$State<-toupper(df$State)

#get map data:
states_map <- map_data(map = "state",region =df$State)

#get mean longitude and latitude
region.lab.data <- states_map %>%
  group_by(region) %>%
  summarise(lon = mean(long), lat = mean(lat))
region.lab.data$region<-toupper(region.lab.data$region)


#replace the longitude and latitude from region.lab.data to df
df2<-left_join(df, region.lab.data, by=join_by(State==region))%>%select(State, Population, lat.y, lon.y)
colnames(df2)<-c("State", "Population", "lat", "lon")
df2[which(df2$State=="HAWAII"),]<-df[which(df$State=="HAWAII"),-1]
df2[which(df2$State=="ALASKA"),]<-df[which(df$State=="ALASKA"),-1]
tail(df2)

# geo styling
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"), #the color of the land
  subunitcolor = toRGB("green"), #the color of the state boundary
  countrycolor = toRGB("black"), #the 
  countrywidth = 0.5,
  subunitwidth = 0.5
)

#方案1: 最好
plot_geo(df2, lat = ~lat, lon = ~lon)%>%
  add_markers(color = ~State, size=~Population,
              text = ~paste(State, Population, sep = "<br />"),
              symbol = I("square"), hoverinfo = "text")%>% 
  layout(title = 'plot', geo = g, legend=list(title = list(text="State Name")))


#方案2: 不好
plot_ly(df, lat = ~lat, lon = ~lon, 
        marker = list(color = "red"), type = 'scattermapbox',hovertext = df$State)%>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =2.5,
      center = list(lon = -88, lat = 34))
  ) 


