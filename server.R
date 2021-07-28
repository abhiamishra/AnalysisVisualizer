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
library(shiny)

#Importing the dataset
maindata <- read_excel("WestHam.xlsx", sheet = 1)
maindata <- maindata %>% filter(!is.na(player))
PassData <- maindata %>% filter(`type` == 'Pass')

SERVER <- function(input, output, session) {
  
  territory <- reactive({
    filter(maindata, teamName == input$team_name)
  })
  observeEvent(territory(), {
    choices <- unique(territory()$player)
    updateSelectInput(session, inputId = "player_name", choices = choices) 
  })
  
  
  playerGraph <- eventReactive(input$show,{
    
    #next stop cutting it off by team - in this case our team of Borussia Dortmund
    team1 <- input$team_name
    
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
    
    #PassFlow
    bin = 20
    x_bin = 100/bin
    y_bin = 100/bin
    passfx <- seq(0,100,by=bin)
    passfy <- seq(0,100,by=bin)
    PassFlow <- data.frame("x"=0.0,"y"=0.0,"endX"=0.0,"endY"=0.0, countP=0.0)
    
    if(input$type == "Receive Flow Map" || input$type == "Pass Flow Map"){
      if(input$type == "Pass Flow Map"){
        PlayerPF <- Team1 %>% filter(player %in% c(input$player_name))
      }
      else if(input$type == "Receive Flow Map"){
        PlayerPF <- Team1 %>% filter(receiver %in% c(input$player_name))
      }
      
      for(i in 1:x_bin){
        filterx <- PlayerPF %>% filter(x>=passfx[i]) %>%
          filter(x<passfx[i+1])
        for(j in 1:y_bin){
          minY = passfy[j]
          maxY = passfy[j+1]
          filtery <- filterx %>% filter(y>=minY) %>%
            filter(y<maxY)
          if(nrow(filtery)>=1){
            me_x = mean(filtery$x)
            me_y = mean(filtery$y)
            me_ex = mean(filtery$endX)
            me_ey = mean(filtery$endY)
            count = sum(filtery$x)
            x <- c(me_x,me_y,me_ex,me_ey,count)
            PassFlow <- rbind(PassFlow, x)
          }
        }
      }
      
      PassFlow <- PassFlow[2:nrow(PassFlow), ]
      
      graph =  PassFlow %>%
        ggplot()+
        annotate_pitch(dimensions = pitch_opta, colour = "white",
                       fill = "#141622") +
        theme_pitch()+
        theme(panel.background = element_rect(fill = "#141622"))+
        geom_bin2d(data=PlayerPF,aes(x=x,y=y),alpha=0.6, 
                   binwidth = c(20, 20), position = "identity")+
        scale_fill_viridis()+
        geom_segment(aes(x=x,y=y,xend=endX,yend=endY,alpha=countP),
                     color="white",lineend = "round", size=3.5, arrow = arrow(length = unit(0.1, "inches")))
    }
    else if(input$type == "Attack Territory" || input$type == "Defensive Territory"){
      if(input$type == "Attack Territory"){
        hull <- c('SavedShot', 'MissedShots', 'Goal', 'TakeOn')
      }
      else if(input$type == "Defensive Territory"){
        hull <- c('Interception','Clearance','Tackle','Challenge','Ball Recoveries')
      }
      hull <- maindata %>% filter(`outcomeType` == "Successful" ) %>%
        filter(isTouch == TRUE) %>%
        filter(`type` %in% hull) %>%
        filter(player == input$player_name)
      
      hull = hull %>%
        mutate(scaledX = abs(scale(x))) %>%
        filter(scaledX < 1) %>%
        mutate(scaledY = abs(scale(y))) %>%
        filter(scaledY < 1)

        hull =  hull %>% slice(chull(x, y))
      graph = ggplot() +
        annotate_pitch(dimensions = pitch_opta, colour = "white",
                       fill = "#141622") +
        theme_pitch() +
        geom_point(data=hull, aes(x,y), color="#CF2990")+
        geom_polygon(data=hull, aes(x,y), alpha = 0.2, colour="#CF2990", fill="#CF2990")
    }
    else if(input$type == "Touch Heatmap"){
      touch = Team1 %>% filter(player == input$player_name) %>%
        filter(isTouch == TRUE)
      graph =  touch %>%
        ggplot()+
        annotate_pitch(dimensions = pitch_opta, colour = "white",
                       fill = "#141622") +
        theme_pitch()+
        geom_bin2d(aes(x=x,y=y),alpha=0.6, 
                   binwidth = c(20, 20), position = "identity")+
        scale_fill_viridis(option = "D")
    }
  
    graph
  }
  )
  
  output$playerData <- renderPlot({
    playerGraph()
  })
}