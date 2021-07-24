library(rgdal)
library(leaflet)
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggmap)
library(forcats)

server <- function(input, output, session) {
  
  ############ Map ###############
  
  #Assign windmill icon
  windmileIcon <- makeIcon(
    iconUrl = "https://pics.freeicons.io/uploads/icons/png/6439110561558095288-512.png",
    iconWidth = 15, iconHeight = 15,
  )
  
  #Create reactive data based on user inputs  
  
  filtered_year <- reactive({
    filter(df, (bouwjaar >= input$bouwjaar_range[1] & bouwjaar <= input$bouwjaar_range[2])) 
  })
  
  filtered_final <- reactive({
    if (input$categorie == "all"){
      filtered_year()
    } else {
      filtered_year() %>%
        filter(categorie == input$categorie)
    }
  })
  
  #Create map
  output$map <- renderLeaflet({
    leaflet(data = filtered_final()) %>%
      addTiles() %>%
      addMarkers(lat = ~lat,
                 lng = ~lon,
                 icon = windmileIcon,
                 label = ~naam,
                 popup = ~popup_info)
    
  })
  
  #Create bar plot
  theme_set(theme_bw())
  
  output$plot1 <- renderPlot({
    ggplot(mutate(df, categorie = fct_infreq(categorie))) + 
      geom_bar(aes(x = categorie), width=.5, fill="tomato3") + 
      labs(title="Total amount of mills in the netherlands: 1213", 
           subtitle="Diveded by category")  + 
      theme(axis.text.x = element_text(size = 10, angle=65, vjust=0.6)) 
  })
  
  ############ Data Explorer ###############
  observe({
    functie <- if (is.null(input$categorie_1)) character(0) else {
      filter(df, categorie %in% input$categorie_1) %>%
        `$`("functie") %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$functie[input$functie %in% functie])
    updateSelectizeInput(session, "functie", choices = functie,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    type <- if (is.null(input$categorie_1)) character(0) else {
      df %>%
        filter(categorie %in% input$categorie_1,
               is.null(input$funcite) | functie %in% input$functie) %>%
        `$`("categorie") %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$type[input$type %in% type])
    updateSelectizeInput(session, "type", choices = type,
                         selected = stillSelected, server = TRUE)
  })
  
  output$table <- DT::renderDataTable({
    df_1 <- df_table %>%
      filter(
        bouwjaar >= input$min_bouwjaar,
        bouwjaar <= input$max_bouwjaar,
        is.null(input$categorie_1) | categorie %in% input$categorie_1,
        is.null(input$functie) | functie %in% input$functie
      ) 
    DT::datatable(df_1, options = list(lengthMenu = c(10, 50, 100), pageLength = 5))
  })
}


#start the app

shinyApp(ui, server)
  

