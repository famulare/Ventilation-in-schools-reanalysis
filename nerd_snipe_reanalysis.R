# co2 vs room correlates reanalysis
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0291840#sec022

library(tidyverse)

d=read.csv('journal.pone.0291840.s002.csv',header = TRUE,skip=1,stringsAsFactors = TRUE) |>
  mutate(Mean.Occupants=as.numeric(Mean.Occupants)) |>
  mutate(PCR.Pos.Count=as.numeric(PCR.Pos.Count)) |>
  mutate(Room.Volume=Square.Footage*Ceiling.Height) |>
  mutate(CO2.Above1k.ppm_hrs = CO2.1.2K.ppm_hrs + CO2.2.3K.ppm_hrs + CO2.3.6K.ppm_hrs)

# fig 8 b
ggplot(d) +
  geom_point(aes(x=CO2.Above1k.ppm_hrs,y=PCR.Pos.Count/Mean.Occupants*24,color=MERV.Filter)) 


sapply(d,class)
summary(d)

head(d)



# it's basically all classroom data
ggplot(d) +
  geom_point(aes(y=PCR.Pos.Count,x=Mean.Occupants,color=ventRmId.1)) +
  facet_wrap('ventRmId.1')

d = d |> filter(ventRmId.1=='CLASSROOM')

d$OccupantGroup[d$Mean.Occupants<75]='50ish'
d$OccupantGroup[d$Mean.Occupants>110 & d$Mean.Occupants<=150]='110-150'
d$OccupantGroup[d$Mean.Occupants>200]='250ish'
d$OccupantGroup=factor(d$OccupantGroup, levels=c('50ish','110-150','250ish'))

## 
ggplot(d) +
  geom_point(aes(x=CO2.Above1k.ppm_hrs,y=PCR.Pos.Count/Mean.Occupants,color=OccupantGroup)) + 
  geom_smooth(aes(x=CO2.Above1k.ppm_hrs,y=PCR.Pos.Count/Mean.Occupants)) +
  ggtitle('Classrooms Only')

ggplot(d) +
  geom_point(aes(x=CO2.Above1k.ppm_hrs,y=PCR.Pos.Count/Mean.Occupants,color=OccupantGroup)) + 
  geom_smooth(aes(x=CO2.Above1k.ppm_hrs,y=PCR.Pos.Count/Mean.Occupants,group=OccupantGroup,color=OccupantGroup),method='lm') +
  ggtitle('Classrooms Only') +
  facet_grid('OccupantGroup',scale='free_y')

