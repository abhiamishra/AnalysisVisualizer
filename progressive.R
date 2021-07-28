#Progressive Passes

ProgPass <- MainData %>% filter(x>60) %>%
  filter(endX - x >0) %>%
  filter(endX >= 0.25*x) %>%
  filter(player == "Declan Rice")



#Preparing the dataset so that we can apply KMC to the passes
kdata <- data.frame("x1"=0,"y1"=0,"xe1"=0,"ye1"=0)

#Concatenating the passes into a single observation
cluster <- c()
for(j in 1:nrow(ProgPass)){
  if(is.na(ProgPass$endX[j])){
    ProgPass$endX[j] = ProgPass$x[j]
    ProgPass$endY[j] = ProgPass$y[j]
  }
  addingOne <- c(ProgPass$x[j],ProgPass$y[j],
                 ProgPass$endX[j],ProgPass$endY[j])
    kdata <- rbind(kdata, addingOne)
}

#preparing the two datasets to apply KMC
kdata <- kdata %>% filter(x1 != 0)
kdata <- na.omit(kdata)
kpop <- kdata
kpop <- scale(kpop)
kpop <- na.omit(kpop)

#required graph so that we can manually determine what to use as centers
fviz_nbclust(kpop, kmeans, method = "wss")

#applying KMC
kpasses <- kmeans(kpop, centers = 4, nstart = 25)

#Now adding in the Cluster factor to the passes
kdata <- kdata %>% mutate(Cluster=kpasses$cluster)
kdata$Cluster <- as.factor(kdata$Cluster)

ProgPass <- ProgPass %>% mutate(Cluster = 0)

for(j in 1:nrow(kdata)){
    ProgPass$Cluster[j] = kdata$Cluster[j]
}

ProgPass <- ProgPass %>% filter(Cluster!=0)
ProgPass$Cluster <- as.factor(ProgPass$Cluster)

ProgPass %>% 
  ggplot()+
  annotate_pitch(dimensions = pitch_opta, colour = "white",
                 fill = "#141622")+
  theme_pitch()+
  theme(panel.background = element_rect(fill = "#141622"))+
  geom_segment(aes(x=(x),y=(y),xend=(endX), yend=(endY),color=Cluster),
               lineend = "round", size = 1, arrow = arrow(length = unit(0.08, "inches")))+
  facet_wrap(~Cluster)
