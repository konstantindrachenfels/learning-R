library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(fresh)

load("gplay.RData")

#choose theme and skin colors as they match well Google Play colors
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#FF2222"
  ),
  adminlte_sidebar(
    width = "320",
    dark_bg = "#FABD02",
  )
)


ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = span(tagList(icon("google-play"),"Google Play Store Apps")),
                                    titleWidth = 350),                   
                    dashboardSidebar( 
                      width = 300,   
                      column(12, h2("Filters")),
                      checkboxGroupInput("type", h4("App type"),                #check for differences between free and paid          
                                          choices = list("Free" = T, 
                                          "Paid" = F),
                                          selected = c(T, F)),
                      sliderInput("rating", h4("Rating"),                        
                                  min = 1, max = 5, 
                                  value = c(1, 5), step = 0.1, sep=""),
                      numericInput("min_reviews", h4("Min number of reviews"),
                                   min = 1, max = 1000000, step = 1000, value = 1000), #filter set to 1000 as results more representative
                      selectInput("cat", h4("Categories"),
                                  choices = c("All categories", as.character(unique(gplay$Cat))),
                                  selected = "All categories", multiple = TRUE) #check for differences throughout categories
                    ),
                    ###### NEW ELEMENTS HERE
                    dashboardBody(use_theme(mytheme),
                      tabBox(
                          title="", height="870px", width=12,
                          tabPanel("Most succesful Apps",
                                    plotlyOutput("plotApps", height = 300),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    fluidRow(column(6,
                                                    DT::dataTableOutput("best1")
                                            ),
                                             column(6,
                                                    DT::dataTableOutput("best2")
                                            )
                                   )
                          ),
                          tabPanel("Most succesful categories",
                                   fluidRow(column(6,
                                                   plotlyOutput("plotCat1", height = 300)
                                            ),
                                            column(6,
                                                   plotlyOutput("plotCat2", height = 300)
                                            )
                                   ),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   fluidRow(column(12, DT::dataTableOutput("pt1"))),
                          ),
                          tabPanel("Relation of success indicators",
                                   plotOutput("plotFin1", height = 300),
                                   br(),
                                   br(),
                                   plotOutput("plotFin2", height = 300),
                                   br(),
                                   br(),
                                   DT::dataTableOutput("pt2")
                          )
                        
                     )
                  )
)

server <- function(input, output){
  
  data <- reactive({
    if(input$cat == "All categories"){
    gplay %>%                  
      filter(Is_Free %in% input$type,
             Rating >= input$rating[1],
             Rating <= input$rating[2],
             nb_Reviews >= input$min_reviews)
    }
    else {
    gplay %>%                  
      filter(Is_Free %in% input$type,
             Rating >= input$rating[1],
             Rating <= input$rating[2],
             nb_Reviews >= input$min_reviews,
             Cat %in% input$cat)  
    }
  })
  
  #table focused on categories to match information in graphs
  output$pt1 <- DT::renderDataTable({data() %>%
      group_by(Cat) %>%
      summarise(Avg_Rating = mean(Rating),
                Avg_Nb_Reviews = mean(nb_Reviews),
                Avg_Ver_Count = mean(Current_Ver),
                Avg_last_Update = mean(last_Update)) %>%
      arrange(Avg_Rating %>% desc(), Avg_Nb_Reviews %>% desc())}, options = list(
                  lengthMenu = list(c(5, 10), c('5', '10')),
                  pageLength = 5
                )
  )
  
  output$pt2 <- DT::renderDataTable({data() %>%
      select(-c(Is_Free, Size_Mbyte, Genres))}, options = list(
        lengthMenu = list(c(5, 10), c('5', '10')),
        pageLength = 5
      )
  )
  
  #Ratings first and arranged after ratings = showing best rated Apps in table
  output$best1 <- DT::renderDataTable({
    data() %>%
      select(Name, Cat, Rating, nb_Dloads, nb_Reviews) %>%
      arrange(Rating %>% desc(), nb_Dloads %>% desc(), nb_Reviews %>% desc())
    }, 
    options = list(
              lengthMenu = list(c(5,10), c('5', '10')),
              pageLength = 5
    )
  )
  
  #nb_Dloads first and arranged after no of downloads = showing most downloaded Apps in table
  output$best2 <- DT::renderDataTable({
    data() %>%
      select(Name, Cat, nb_Dloads, Rating, nb_Reviews) %>%
      arrange(nb_Dloads %>% desc(), Rating %>% desc(), nb_Reviews %>% desc())
    }, 
    options = list(
              lengthMenu = list(c(5,10), c('5', '10')),
              pageLength = 5
    )
  )
  
  #Plot to show most successful Apps = high downloads(filter 50M+), ratings and reviews as axes => 3 indicators to show success
  output$plotApps <- renderPlotly({
    g <- data() %>%
      filter(nb_Dloads >= "50M+",
             nb_Reviews < 40000000) %>%
      ggplot(aes(x = nb_Reviews, y = Rating, color = nb_Dloads, label = Name)) + geom_point() + 
      ggtitle("Apps with more than 50 million downloads") + theme(plot.title = element_text(face = "bold", color = "#008D4C", hjust = 0.5 )) +
      xlab("Number of reviews") 
    ggplotly(g)
  })
  
  #Show Categories that have most Apps that reached very high download numbers => filter to only show Apps with more than !00 million dloads
  output$plotCat1 <- renderPlotly({
    h <- data() %>%
            group_by(Cat) %>%
            filter(nb_Dloads>= "100M+") %>%
            summarise(nb_Apps = n(),
                      avg_rating = (mean(Rating)),
                      avg_reviews = mean(nb_Reviews)) %>%
            arrange(desc(nb_Apps)) %>%
            head(5) %>%
            ggplot(aes(x = Cat, y = nb_Apps, fill = avg_rating, label = avg_reviews)) + geom_bar(stat="identity") +
            ggtitle("Categories with highest downloads") + theme(plot.title = element_text(face = "bold", color = "#008D4C", hjust = 0.5 )) +
            xlab("Categories") + 
            ylab("Nb of apps with 100M+ downloads")
    ggplotly(h)
  })
  
  #Show categories in which we have most Apps in total => no download number filter, all Apps considered
  output$plotCat2 <- renderPlotly({
    i <- data() %>%
            group_by(Cat) %>%
            summarise(nb_Apps = n(),
                      avg_rating = (mean(Rating))) %>%
            arrange(desc(nb_Apps)) %>%
            head(5) %>%
            ggplot(aes(x = Cat, y = nb_Apps, fill = avg_rating)) + geom_bar(stat="identity") + 
            ggtitle("Categories with most Apps") + theme(plot.title = element_text(face = "bold", color = "#008D4C", hjust = 0.5 )) +
            xlab("Categories") + 
            ylab("Number of apps")
    ggplotly(i)
  })
  
  #In the following 2 graphs I want to see if there is a clear relationship in-between the chosen success factors:
  # number of downloads, ratings and filters
  
  #First plot analyzes relationship in-between rating and download numbers
  output$plotFin1 <- renderPlot({
    data() %>% 
      group_by(nb_Dloads) %>%
      filter(nb_Dloads > "500+") %>%
      ggplot(aes(x = nb_Dloads, y = Rating), fill = nb_Reviews) + geom_jitter(size = 0.3) +
      geom_boxplot(alpha = 0.7) +
      ggtitle("Impact of number of downloads on rating") + theme(plot.title = element_text(size = 20, face = "bold", color = "#008D4C", hjust = 0.5 )) +
      xlab("Number of downloads")
  })
  
  #Second plot analyzes relationship in_between rating and review numbers
  output$plotFin2 <- renderPlot({
    data() %>%
      filter(nb_Reviews >= 1000,
             nb_Reviews <= 1000000) %>%
      ggplot(aes(x = nb_Reviews , y = Rating)) + geom_jitter() + geom_smooth(model=lm) +
      ggtitle("Impact of number of reviews on rating") + theme(plot.title = element_text(size = 20, face = "bold", color = "#008D4C", hjust = 0.5 )) +
      xlab("Number of reviews")
  })
  
  }

# We can't detect a clear relationship in-between any of these factors. However, the dispersion decreases as the number of downloads/reviews increases
# I also looked if there are interesting conclusions to draw from the relationship with the update date, version number and support number but did not find anything striking.
# Thus I decided to not include these factors. Difference between paid and free and categories are included in filters

# Run the app ----
shinyApp(ui = ui, server = server)

