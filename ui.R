library(shiny)
library(tidyverse)
library("readxl")
library(reactable)

maindata <- read_excel("WestHam.xlsx", sheet = 1)
maindata <- maindata %>% filter(!is.na(player))

UI = fluidPage(
  titlePanel("West Ham Team Interface Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("team_name", "Select a Team", choices = unique(maindata$teamName),
                     options = list(maxItems = 1, placeholder = 'Type Team Name')),
      selectizeInput("player_name", "Select a Player", choices = unique(maindata$player),
                     options = list(maxItems = 1, placeholder = 'Type Player Name')),
      selectizeInput("type", "Select Type of Analysis", choices = c("Pass Flow Map", 
                                                                    "Receive Flow Map",
                                                                    "Attack Territory",
                                                                    "Defensive Territory",
                                                                    "Touch Heatmap"),
                     options = list(maxItems = 1, placeholder = 'Type Type Name')),
      actionButton("show", label = "Show"),
      
    ),
    
    mainPanel(
      plotOutput("playerData")
    )
  )
)

