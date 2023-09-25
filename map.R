library(ggplot2)
library(janitor)
library(dplyr)
require(maps)
require(viridis)
poll_data<-read.csv("D:\\2\\project1\\polls_us_election_2016.csv")
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


