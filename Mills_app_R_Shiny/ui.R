library(rgdal)
library(leaflet)
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggmap)
library(forcats)
library(rsconnect)


df <- read_csv("df_molens.csv")
df_table <- read_csv("df_table.csv")


#create the app 
#Create the user interface
ui <- fluidPage(
  
  theme = shinytheme("paper"),
  
  ############ Map ############### 
  navbarPage("Mills in the Netherlands",
             tabPanel("Map",
                      
                      
                      sidebarPanel(
                        p("The Netherlands is famous for it's windmills, but there are way more 
                              types of mills than just windmills. Select a time range and category below 
                              and see where the mills are located. When you click on the marker displayed 
                              on the map, additional information wll be given."),
                        br(),
                        sliderInput("bouwjaar_range", "Year built", 
                                    min = min(df$bouwjaar),
                                    max = max(df$bouwjaar), 
                                    value = c(min(df$bouwjaar), max(df$bouwjaar)),
                                    sep = "",
                                    step = 1),
                        selectInput("categorie", "Select category", 
                                    choices = c("all",(unique(df$categorie))),
                                    selected = "all",
                                    multiple = FALSE),
                        br(),
                        plotOutput("plot1")
                        
                      ),
                      
                      mainPanel(
                        leafletOutput("map", height = "800px"))
             ),
             
             ############ Data explorer ############### 
             tabPanel("Data explorer",
                      fluidRow(
                        column(3,
                               selectInput("categorie_1", "categories",
                                           choices = c(unique(df$categorie),
                                                       multiple = TRUE)
                               )
                        ),
                        column(3,
                               selectInput("functie", "function",
                                           choices = c(unique(df$functie),
                                                       multiple = TRUE)
                               )
                        )
                      ),
                      
                      fluidRow(
                        column(1,
                               numericInput("min_bouwjaar", 
                                            "Min year built", 
                                            min = min(df$bouwjaar), 
                                            max = max(df$bouwjaar), 
                                            value = 1212)
                        ),
                        column(1,
                               numericInput("max_bouwjaar", 
                                            "Max year built",
                                            min = min(df$bouwjaar), 
                                            max = max(df$bouwjaar), 
                                            value = 2020)
                               
                               
                        )
                      ),
                      hr(),
                      DT::dataTableOutput("table")
             )
  )
)

