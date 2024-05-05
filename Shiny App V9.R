
 # Adam Billen, Ogechi Onyewu, Sally Henley 
 

```{r}
library(tidyverse)
library(shiny)
library (shinyWidgets)
#library (shinyui)
library(DT)
library(ggplot2)

baldur <- read.csv("baldur.csv")
```

```{r}
ui <- fluidPage(
  titlePanel("Companion stats filtered by alignment"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Ethics", 
                  
                  "You find a wounded knight on the side of the road. Do you...",
                  
                  choices = c("A. Sacrifice my water and bandages to help him" = "Lawful", 
                              "B. Help him walk town but keep your water" = "Neutral", 
                              "C. Rob him and leave him to die" = "Chaotic")),
      
      selectInput("Morals", 
                  
                  "You come across a village plagued by a deadly disease. The only cure is a rare 
                  herb guarded by a reclusive hermit. What do you do?",
                  
                  choices = c("A. Immediately seek out the hermit, risking your own safety to save the village" = "Good",
                              "B. Negotiate with the hermit, offering something in return for the herb" = "Neutral",
                              "C. Leave the village to its fate, prioritizing your own interests" = "Evil")
                  
      ),
      
      sliderInput("str_lower",
                  label = "Select the lower bound of strength",
                  value = 4, min = 4, max = 20),
      
      sliderInput("str_upper",
                  label = "Select the upper bound of strength",
                  value = 20, min = 4, max = 20),
      
      sliderInput("dex_lower",
                  label = "Select the lower bound of dexterity",
                  value = 4, min = 4, max = 20),
      
      sliderInput("dex_upper",
                  label = "Select the upper bound of dexterity",
                  value = 20, min = 4, max = 20),
      
      sliderInput("con_lower",
                  label = "Select the lower bound of constitution",
                  value = 4, min = 4, max = 20),
      
      sliderInput("con_upper",
                  label = "Select the upper bound of constitution",
                  value = 20, min = 4, max = 20),
      
      sliderInput("int_lower",
                  label = "Select the lower bound of intuition",
                  value = 4, min = 4, max = 20),
      
      sliderInput("int_upper",
                  label = "Select the upper bound of intuition",
                  value = 20, min = 4, max = 20),
      
      sliderInput("wis_lower",
                  label = "Select the lower bound of wisdom",
                  value = 4, min = 4, max = 20),
      
      sliderInput("wis_upper",
                  label = "Select the upper bound of wisdom",
                  value = 20, min = 4, max = 20),
      
      sliderInput("cha_lower",
                  label = "Select the lower bound of charisma",
                  value = 4, min = 4, max = 20),
      
      sliderInput("cha_upper",
                  label = "Select the upper bound of charisma",
                  value = 20, min = 4, max = 20),
      
      selectInput("Stat", "Which stat are you interested in?",
                  choices = c("str", "dex", "con", "int", "wis", "cha"))
      
    ),
    
    mainPanel(
      img(src='BaldurScreenshot 2024-05-05.png', align ="right"),
      tabsetPanel(type = "tab",
                  tabPanel("Table",  dataTableOutput("table")),
                  tabPanel("Bar plot", plotOutput("plot")),
                  tabPanel("Histogram plot", plotOutput("Histogram")),
                  tabPanel("Box plot", plotOutput("boxplot")),
                  tabPanel("Pie plot", plotOutput("pie"))
      )
    )
  )
)

server <- function(input, output) {
  
  
  baldur_filtered <- reactive({
    filter(baldur, 
           ethics == input$Ethics 
           & morals == input$Morals 
           & between(str, input$str_lower, input$str_upper)
           & between(dex, input$dex_lower, input$dex_upper)
           & between(con, input$con_lower, input$con_upper)
           & between(int, input$int_lower, input$int_upper)
           & between(wis, input$wis_lower, input$wis_upper)
           & between(cha, input$cha_lower, input$cha_upper))
  })
  
  output$table <- renderDataTable(
    baldur_filtered()
  )
  
  output$plot <- renderPlot({
    ggplot(baldur_filtered(), aes_string(x = input$Stat)) +
      geom_bar() +
      ggtitle("Barplot filtered by selections")
  })
  output$boxplot <- renderPlot({
    ggplot(baldur_filtered(), mapping = aes(x = race, y = .data[[input$Stat]])) +
      geom_boxplot() +
      ggtitle("Boxplot filtered by selections") 
    
  })
  
  output$Histogram <- renderPlot({
    ggplot(baldur_filtered(), mapping = aes(x = .data[[input$Stat]])) +
      geom_histogram() +
      ggtitle("Histogram filtered by selections")})
  
  
  output$pie <- renderPlot({
    
    baldurdata1 <- baldur_filtered() %>% group_by(class) %>% summarize(count = n())
    total <- sum(baldurdata1$count)
    baldurdata1 <- baldurdata1 %>% mutate(perc = round((count/total) * 100, 2))
    
    ggplot(baldurdata1, aes(x="", y=count, fill=class)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      geom_text(aes(label = paste0(perc, "%"), x = 1.2), position = position_stack(vjust=0.5, )) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
  })
}

shinyApp(ui = ui, server = server)
```


