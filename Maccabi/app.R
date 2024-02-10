## Simplified app from last week!
library(shiny)
library(ggplot2)

## base R!
d <- mtcars
d$car_types <- rownames(d)
d <- d[,c("car_types","mpg", "cyl", "am", "disp", "wt")]
d$cyl <- as.factor(d$cyl)
d$am <- as.factor(d$am)

### UI
ui <- fluidPage(
  
  title = "Cars!",
  
  sidebarPanel(
    
    selectInput(inputId = "cyl",
                label = "Cylinders",
                choices = sort(unique(d$cyl)),
                selected = NULL,
                multiple = FALSE),
    
    selectInput(inputId = "am",
                label = "Transmission",
                choices = sort(unique(d$am)),
                selected = NULL,
                multiple = FALSE)
    
    
  ),
  
  mainPanel(
    plotOutput(outputId = 'xy_plt'),
    plotOutput(outputId = "hist_plt"),
    tableOutput(outputId = "tbl")
  )
)


### Server
server <- function(input, output, session){
  
  ## NO URL QUERY PARSING FOR YOU
  
  ## get data
  dat <- reactive({
    d[d$cyl == input$cyl & d$am == input$am, ]
  })
  
  ## xy plot
  output$xy_plt <- renderPlot({
    
    dat() |>
      ggplot(aes(x = disp, y = mpg)) +
      geom_jitter(size = 5,
                  color = "green") +
      geom_smooth(method = "lm",
                  se = FALSE,
                  color = "black",
                  size = 1.2) +
      theme_light()
    
  })
  
  ## hist plot
  output$hist_plt <- renderPlot({
    
    dat() |>
      ggplot(aes(x = "1", y = wt)) +
      geom_boxplot(color = "black",
                   fill = "light grey") +
      theme_light()
    
  })
  
  ## table of car types
  output$tbl <- renderTable({
    dat()[,"car_types"]
  })
  
}


## Shiny app must return a shinyapp
shiny::shinyApp(ui = ui, server = server)