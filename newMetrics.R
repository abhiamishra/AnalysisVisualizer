library(tidyverse)
library(ggplot2)
library("readxl")
library(dplyr)
library(ggpubr)
library(ggrepel)
library(ggsoccer)
library(gghighlight)
library(standardize)
library(RColorBrewer)
library(viridis)
library(ggrepel)
library(ggforce)
library(factoextra)

#Importing the dataset
maindata <- read_excel("WestHam.xlsx", sheet = 1)
legend = maindata %>%
  select(playerId, player, teamName, team) %>%
  filter(teamName == "West Ham")
legend = unique(legend)
legend = legend[2:nrow(legend),]
maindata <- maindata %>% filter(!is.na(player))
PassData <- maindata %>% filter(`type` == 'Pass')

#next stop cutting it off by team - in this case our team of Borussia Dortmund
team1 <- "West Ham"

#only having successful pasess, selecting the relevant columns, and making the receiverID column complete
Team1 <- PassData %>% filter(teamName == team1) %>% 
  filter(`outcomeType` == 'Successful') %>% 
  mutate(receiver = player)

#inputting the receivers set of the data so that each passer has a recipient
for(i in 1:nrow(Team1)){
  if(i != 1){
    Team1$receiver[(i-1)] = Team1$player[i]
  }
}

#EPV/xT
#Calculating xT/////////////////////////////////////

#loading in the data
xT <- as.matrix(xTLegend)
MainData <- Team1
size <- nrow(MainData)

vecX <- seq(0, 93.75, by=6.25)
vecY <- seq((100/12), 100, by=(100/12))

#assiging values!

xGx <- as.numeric(1:size)
vecI <- as.numeric(1:16)
index = 1
for(i in MainData$x) {
  for(j in vecI){
    if( (vecX[j]<=i) & i<=(vecX[j]+6.25) ){
      xGx[index] <- j
      
    }
  }
  index=index+1
}


xGy <- as.numeric(1:size)
y <- 12:1
indexY=1
for(a in MainData$y) {
  for(b in y){
    if( (vecY[b] >= a) & (a >= (vecY[b] - (100/12))) ){
      xGy[indexY] <- b
    }
  }
  indexY = indexY+1
}

#assign xG values to each quadrant
finalxG <- 1:size

indexing <- 1:size

for(i in indexing){
  i_X = xGx[i]
  i_Y = xGy[i]
  finalxG[i] = xT[i_Y, i_X]
}

#adding values to the column
MainData <- MainData %>% mutate(xTStart = finalxG)







#endlocations
size <- nrow(MainData)

vecX <- seq(0, 93.75, by=6.25)
vecY <- seq((100/12), 100, by=(100/12))

#assiging values!

xGx <- as.numeric(1:size)
vecI <- as.numeric(1:16)
index = 1
for(i in MainData$endX) {
  if( i == -1){
    xGx[index] <- -10
  }
  for(j in vecI){
    if( (vecX[j]<=i) & i<=(vecX[j]+6.25) ){
      xGx[index] <- j
      
    }
  }
  index=index+1
}


xGy <- as.numeric(1:size)
y <- 12:1
indexY=1
for(a in MainData$endY) {
  if(a == -1){
    xGy[indexY] = -10
  }
  for(b in y){
    if( (vecY[b] >= a) & (a >= (vecY[b] - 8.33333333333333333333333333333333333)) ){
      xGy[indexY] <- b
    }
  }
  indexY = indexY+1
}

#assign xG values to each quadrant
finalxG <- 1:size

indexing <- 1:size

for(i in indexing){
  i_X = xGx[i]
  i_Y = xGy[i]
  if(i_Y == -10 || i_X == -10){
    finalxG[i] = -10
  }else{
    finalxG[i] = xT[i_Y, i_X]
  }
}

#adding values to the column
MainData <- MainData %>% mutate(xTEnd = finalxG)
Team1 <- MainData %>% mutate(xT = xTEnd - xTStart)

#Inputting this back into the main data frame
MainData <- maindata

MainData <- merge(x=MainData, y=Team1[,c("receiver","xTStart", "xTEnd", "xT","id")], by="id", all.x=TRUE)

#TouchMap
DeclanTouch <- MainData %>% filter(player == "Declan Rice")

DeclanTouch %>%
  ggplot()+
  annotate_pitch(dimensions = pitch_opta, colour = "white",
                 fill = "#141622")+
  theme_pitch()+
  theme(panel.background = element_rect(fill = "#141622"))+
  geom_bin2d(aes(x=x,y=y),binwidth = c(10,10), alpha=0.6)+
  #scale_fill_viridis()+
  scale_fill_gradient(low = "white", high = "red")+
  labs(
    fill = "Count of How Many Touches"
  )+
  theme(
    legend.background = element_rect(fill = "#141622"),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white")
  )
  

  #BuildUpPhases
MainData <- MainData %>%
  mutate(length = endX-x) %>%
  mutate(group = "Z") %>%
  mutate(order = 0) %>%
  mutate(seq = "") %>%
  mutate(period_value = 
           ifelse(period == "FirstHalf", 1, 2))

  grouping = 0
PassChain <- MainData[1,]



for(j in 1:nrow(MainData)){
  print(j/nrow(MainData))
  if(#!is.na(MainData$endX[j]) &&
     #(MainData$endX[j]>=(83)) &&
     #!is.na(MainData$endY[j]) &&
     #(MainData$endY[j]>=21.1 && MainData$endY[j]<=78.9) &&
    #MainData$player[j] == "Declan Rice" && 
    MainData$outcomeType[j]=="Successful" &&
     MainData$teamName[j]=="West Ham"){
    
    if(j+3>0){
      
      boolean_chain = 0
      period_prop = as.numeric(c())
      for(b in 3:0){
        if(MainData$teamName[j+b] =="West Ham"){
          
          if(MainData$outcomeType[j+b] == "Successful" &&
             MainData$type[j+b] != "Foul" &&
             MainData$isTouch[j+b] == TRUE){
            
            period_prop = as.numeric(c(period_prop,MainData$period_value[j+b]))
            num = as.numeric(sum(period_prop==1))
            num2 = as.numeric(sum(period_prop==2))
            if((num/length(period_prop))==1 || (num2/length(period_prop))==1){
              boolean_chain = boolean_chain+1
            }
          }
        }
      }
      
      
      
      if(boolean_chain == 4){
        for(i in 0:3){
          if(i==0){
            groupdivider = "A"
          }
          else if(i==1){
            groupdivider = "B"
          }
          else if(i==2){
            groupdivider = "C"
          }
          else if(i==3){
            groupdivider = "D"
          }
          
          MainData$group[j+i] = groupdivider
          MainData$order[j+i] = grouping
         
        }
        fullstring <- c()
        for(i in 0:3){
          if(MainData$player[j+i] == "Declan Rice"){
            fullstring <- paste(fullstring,"A")
          }
          else{
            fullstring <- paste(fullstring,"B")
          }
        }
        for(i in 0:3){
          MainData$seq[j+i] = fullstring
          addChain <- MainData[(j+i),]
          PassChain <- rbind(PassChain, addChain)
        }
       
        grouping = grouping + 1
      }
    }
  }
}

PassChain <- PassChain[2:nrow(PassChain),]
PassChain <- PassChain %>% filter(player != "")
PassChain$x <- as.numeric(PassChain$x)
PassChain$endX <- as.numeric(PassChain$endX)
PassChain$y <- as.numeric(PassChain$y)
PassChain$endY <- as.numeric(PassChain$endY)
PassChain$order <- as.factor(PassChain$order)


for(i in 1:nrow(MainData)){
  if(MainData$group[i] == "A"){
    directness = abs(MainData$length[i+1]) + abs(MainData$length[i+2])
    directness = directness/MainData$length[i]  
    MainData$directness[i] = directness
  }
}
  
  #Preparing the dataset so that we can apply KMC to the passes
  kdata <- data.frame("x1"=0,"y1"=0,"xe1"=0,"ye1"=0,
                      "x2"=0,"y2"=0,"xe2"=0,"ye2"=0,
                      "x3"=0,"y3"=0,"xe3"=0,"ye3"=0,
                      "x4"=0,"y4"=0,"xe4"=0,"ye4"=0)
  
  #Concatenating the passes into a single observation
  cluster <- c()
  for(j in 1:nrow(PassChain)){
    if(is.na(PassChain$endX[j])){
      PassChain$endX[j] = PassChain$x[j]
      PassChain$endY[j] = PassChain$y[j]
    }
    addingOne <- c(PassChain$x[j],PassChain$y[j],
                   PassChain$endX[j],PassChain$endY[j])
    cluster = c(cluster,addingOne)
    if(j %% 4 == 0){
      kdata <- rbind(kdata, cluster)
      cluster <- c()
      
    }
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
  kpasses <- kmeans(kpop, centers = 5, nstart = 25)
  
  #Now adding in the Cluster factor to the passes
  kdata <- kdata %>% mutate(Cluster=kpasses$cluster)
  kdata$Cluster <- as.factor(kdata$Cluster)
  
  PassChain <- PassChain %>% mutate(Cluster = 0)
  
  for(j in 1:nrow(kdata)){
    for(i in 1:4){
      r = ((j-1)*4) + i
      PassChain$Cluster[r] = kdata$Cluster[j]
    }
  }
  
  PassChain <- PassChain %>% filter(Cluster!=0)
  PassChain$Cluster <- as.factor(PassChain$Cluster)
  PassChain <- PassChain %>% mutate(DR = 
                                      ifelse(player == "Declan Rice", 1, 2))
  PassChain$DR <- as.factor(PassChain$DR)
  #finding the single pass-chain representative for the cluster
  TableauData <- data.frame("x"=0,"y"=0,"endX"=0,"endY"=0, "Cluster"=0, 
                            "order"="", "count" = 0, "xT"=0)
  chainrows = nlevels(PassChain$Cluster)
  for(c in 1:chainrows){
    TempCP <- PassChain %>% filter(Cluster==c)
    for(i in 1:4){
      if(i==1){
        groupdivider = "A"
      }
      else if(i==2){
        groupdivider = "B"
      }
      else if(i==3){
        groupdivider = "C"
      }
      else if(i==4){
       groupdivider = "D"
      }
      TempCP2.0 <- TempCP %>% filter(group==groupdivider)
      x <- c(median(TempCP2.0$x),median(TempCP2.0$y),
             median(TempCP2.0$endX),median(TempCP2.0$endY),c,groupdivider,
             nrow(TempCP),sum(TempCP2.0$xT))
      TableauData <- rbind(TableauData,x)
    }
  }
  
  #formatting our final dataset so that it can be graphed
  TableauData <- TableauData %>% filter(x!=0)
  TableauData$x <- as.numeric(TableauData$x)
  TableauData$y <- as.numeric(TableauData$y)
  TableauData$endX <- as.numeric(TableauData$endX)
  TableauData$endY <- as.numeric(TableauData$endY)
  TableauData$count <- as.numeric(TableauData$count)
  TableauData$Cluster <- as.factor(TableauData$Cluster)
  TableauData <- TableauData %>% filter(!is.nan(x))
  
  
  TableauData %>% 
    ggplot()+
    annotate_pitch(dimensions = pitch_opta, colour = "white",
                   fill = "#141622")+
    theme_pitch()+
    theme(panel.background = element_rect(fill = "#141622"))+
    geom_segment(aes(x=(x),y=(y),xend=(endX), yend=(endY), color=Cluster,alpha=count),
                 lineend = "round", size = 2, arrow = arrow(length = unit(0.08, "inches")))+
    scale_alpha_continuous(range=c(0.2,1))+
    geom_point(aes(x=endX,y=endY, color=Cluster,alpha=count),size=8, shape=1)+
    geom_label(aes(x=10, y=10, label=count))+
    geom_label(aes(x=10, y=80, label=xT))+
    facet_wrap(~Cluster)+
    theme(
      panel.background = element_rect(fill = "#141622"),
      plot.background = element_rect(fill = "#141622")
    )
  

  
  PassChain %>% 
    ggplot()+
    annotate_pitch(dimensions = pitch_opta, colour = "white",
                   fill = "#141622")+
    theme_pitch()+
    theme(panel.background = element_rect(fill = "#141622"))+
    geom_segment(aes(x=(x),y=(y),xend=(endX), yend=(endY), color=DR, alpha=DR),
                 lineend = "round", size = 1, arrow = arrow(length = unit(0.08, "inches")))+
    scale_alpha_discrete(range=c(1,0.05))+
    geom_label(data=TableauData, aes(x=10, y=10, label=count))+
    facet_wrap(~Cluster)+
    theme(
      panel.background = element_rect(fill = "#141622"),
      plot.background = element_rect(fill = "#141622")
    )
  
  #Looking at tangible production from event-chains
  PassChain %>% filter(group == "D") %>%
    filter(type != "Pass") %>%
    ggplot()+
    annotate_pitch(dimensions = pitch_opta, colour = "white",
                   fill = "#141622")+
    theme_pitch()+
    theme(panel.background = element_rect(fill = "#141622"))+
    geom_point(aes(x=endX,y=endY, color=type,shape=type),size=3.5)
  
  PassChain$seq <- as.factor(PassChain$seq)
  countingSeq <- PassChain %>%
    group_by(seq) %>%
    summarise(n_row = n()) %>%
    filter(seq != " B B B B") %>%
    arrange(desc(n_row))
  
  countingSeq %>% 
    ggplot()+
    geom_bar(aes(x=reorder(seq, n_row), y=n_row),
             stat="identity",
             fill = "#CF2990")+
    labs(
      x="Types of Involvement by Player",
      y="Count"
    )+
    theme_minimal()
  

  

#xT
  WHUPlayers <- unique(Team1$player)
  player_xt = data.frame("player" = "", xT = 0.0)
  for(i in 1:length(WHUPlayers)){
    player_to_find = WHUPlayers[i]
    player_filtered = Team1 %>% filter(player == player_to_find)
    calc = sum(player_filtered$xT)
    player_xt <- rbind(player_xt, c(player_to_find, calc))
  }

  player_xt <- player_xt %>% filter(player != "") %>%
    arrange(desc(xT)) %>%
    slice(1:10)
  
  player_xt$xT <- as.numeric(player_xt$xT)

  player_xt %>%
    ggplot(aes(reorder(player,-player_xt$xT), xT))+
    geom_col(fill = "#CF2990")+
    xlab("Player")+
    theme_minimal()

  
  DeclanThreat <- Team1 %>% filter(player == "Declan Rice") %>%
    filter(xT > 0) %>%
    arrange(desc(xT))


  DeclanThreat %>% 
    ggplot()+
    annotate_pitch(dimensions = pitch_opta, colour = "white",
                   fill = "#141622")+
    theme_pitch()+
    theme(panel.background = element_rect(fill = "#141622"))+
    geom_segment(aes(x=(x),y=(y),xend=(endX), yend=(endY)), color="white",
                 lineend = "round", size = 1, arrow = arrow(length = unit(0.08, "inches")))          
  