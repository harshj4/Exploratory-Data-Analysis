library(shiny)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)
library(ggplotify)
library(ggimage)

ui <- fluidPage(
  theme = "ext.css" ,
  
  headerPanel('Twitter Application Development'),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                  
                  "Select map type to show:",
                  checkboxInput("donum1", "CDC Map", value = F),
                  checkboxInput("donum2", "Twitter Map", value = F),
                  checkboxInput("donum3", "Twitter Map for 4 keywords", value = F),
                  checkboxInput("donum4", "Show comarison", value = F)
                  
                ),
                mainPanel("",
                          #  column(3,plotOutput(outputId="plotgraph", width="500px",height="400px"))
                          fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("plotgraph1"), plotlyOutput("plotgraph2"))
                          ),
                          fluidRow(
                            column(7, plotlyOutput(("plotgraph3"))),
                            column(5, textOutput(("plotgraph4")))
                          )  
                )
  )
)

server <- function(input, output) {
  
  
  
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    
    
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
    
    
    
    #qplot(rnorm(500),fill=I("red"),binwidth=0.2,main="plotgraph1")
  })
  
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    
    library(rtweet)
    library(zipcode)
    
    #tweet_df <- read.csv("tweets.csv",header = TRUE,sep=",")
    
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
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    heat_map <- plot_geo(flu_df, locationmode = 'USA-states') %>%
      add_trace(
        z = ~code_count, text = ~code_count,locations = ~code,colors = 'Blues')%>%
      layout(geo = list(scope = 'usa',showLegend = TRUE,projection = list(type = 'albers usa')), title = '2018-19 Influenza Season week 4 ILI Activity level indicator \n (Hower to breakdown)')
    heat_map
    
    
  })
  
  pt3 <- reactive({
    if (!input$donum3) return(NULL)
    
    keywords <- c("Influenza","H1N1","H2N2","Yamagata")
    #all_tweets <- read.csv("tweets.csv",header = TRUE,sep=',')
    
    loc_data <- read.csv(file="key_addresses.csv",header = TRUE)
    length(loc_data)
    all_state_codes <- state.abb
    key_flu_df <-data.frame(states = NULL,intensity = NULL)
    for (code in all_state_codes){
      res <- grepl(code,loc_data$res,ignore.case = FALSE)
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
    
    
  })
  pt4 <- reactive({
    if (!input$donum4) return(NULL)
    ("
      Comparison between Heat Map 1 and Heat Map 2
Heat Map 1:

Generated using twitter data for the year 2019 using Flu related keywords


Heat Map 2:
Generated using CDC data for the same time period


If we consider that the data obtained from CDC website is most accurate, then it can be observed that heap map 1 is only accurate about only few states (Texas, Kansas). This is because of our assumption that mere mention of flu related keyword can be attributed to actual presence of positive specimen in that state. In reality, there can be a case where, twitter is flooded with flu related keywords in a state without any major outbreak of flu in that state. 

The data collected via CDC website was collected from more credible sources like Public Health Labs. In order to improve our results obtained from twitte data, we can perform senetimental analysis of tweets and determine if that tweets we are analyzing are actually worthy of counting for our analysis. We can disregard such tweets which don't provide any significant indication about presence of flu outbreak.

Comparison between Heat Map 1 and Heat Map 3
Heat Map 3:

Generated by using tweets related to specific keywords


Heat map 3 looks sparse as compared to Heat Map 1. This is because there was less amount of tweet data available for specific keywords. As we look for mor specific keywords, we narrow down our search. Hence, we get lesser amount of data.
      ")
    
  })
  
  
  output$plotgraph1 = renderPlotly({pt1()})
  output$plotgraph2 = renderPlotly({pt2()})
  output$plotgraph3 = renderPlotly({pt3()})
  output$plotgraph4 = renderText({pt4()})
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
