#source("./global.R")
library(shinydashboard)
library(shiny)
library(shinythemes)
library(DT) 
#library(data.table)
library(shinyBS)
library(shinyjs)
library(stringr)
library(stringi)
library(tidyr)
library(dplyr)
library(highcharter)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinymanager)
#library(RColorBrewer)
library(reactable)
library(shinybusy)
#library(fst)
#library(shinyalert)
#library(plotly)
#library(ggplot2)
#library(ggfortify)
#library(ggrepel)
#library(corrplot) # new package
#library(heatmaply)
library(collapsibleTree)
library(lubridate)
library(fresh)

setwd("C:/Users/tomhaber/OneDrive - Intel Corporation/Desktop/Maccabi")

load("All_Games_Tables_Maccabi.RData")

Main_Team <- sort(unique(All_Games_Tables$Main_Team))
Season <- sort(unique(All_Games_Tables$Season))
Tournament <- sort(unique(All_Games_Tables$Tournament))
ref <- unique(str_trim(sort(unique(All_Games_Tables$ref))))
event <- unique(str_trim(sort(unique(All_Games_Tables$Event))))
outcome <- unique(str_trim(sort(unique(All_Games_Tables$Outcome))))
outcome_list <- list(
  'ניצחון' = 'Won',
  'תיקו' = 'Draw',
  'הפסד' = 'Lost'
)

# creating hosting type : 
All_Games_Tables$hosting_type <- ifelse(All_Games_Tables$Host=="מכבי חיפה",
                                        "משחק בית",
                                        "משחק חוץ"
)

# creat the options for selecting hosting type
hosting <- c("משחק בית","משחק חוץ")

All_Games_Tables$Year <- year(All_Games_Tables$Date)

YEAR <- sort(unique(All_Games_Tables$Year))
MONTH <- month.name

month_list <- list(
  'ינואר' = 'January',
  'פברואר' = 'February',
  'מרץ' = 'March',
  'אפריל' = 'April',
  'מאי' = 'May',
  'יוני' = 'June',
  'יולי' = 'July',
  'אוגוסט' = 'August',
  'ספטמבר' = 'September',
  'אוקטובר' = 'October',
  'נובמבר' = 'November',
  'דצמבר' = 'December'
)

### creating data of lineup maccabi players with ID 

host_lineup <- All_Games_Tables %>% filter(Host=="מכבי חיפה") %>% pull(Games_Lineup_Table_list_a)
#host_bench <- All_Games_Tables %>% filter(Host=="מכבי חיפה") %>% pull(Games_bench_Table_list_a)
games_id_host <- All_Games_Tables %>% filter(Host=="מכבי חיפה") %>% pull(Game_ID)

visitor_lineup <- All_Games_Tables %>% filter(Visitor=="מכבי חיפה") %>% pull(Games_Lineup_Table_list_b)
games_id_visitor <- All_Games_Tables %>% filter(Visitor=="מכבי חיפה") %>% pull(Game_ID)



maccabi_host_id=lapply(seq_along(host_lineup), function(i) {
  
  if(nrow(host_lineup[[i]])!=0){
    host_lineup[[i]]$games_id <- games_id_host[i]
  }
  return( host_lineup[[i]] )
}) %>% bind_rows()

maccabi_visitor_id=lapply(seq_along(visitor_lineup), function(i) {
  
  if(nrow(visitor_lineup[[i]])!=0){
    visitor_lineup[[i]]$games_id <- games_id_visitor[i]
  }
  return( visitor_lineup[[i]] )
}) %>% bind_rows()

all_lineup_id <- bind_rows(maccabi_host_id,maccabi_visitor_id)

players_lineup_names <- sort(unique(str_trim(str_replace_all(all_lineup_id$Player_Name,"[x[:digit:]]",""))))

#### END CREATING MACCABI PLAYERS ID



ui <-  dashboardPagePlus(
  skin = "green-light",
  dashboardHeader(title = "ניתוח משחקים היסטורים",titleWidth = "350px"),
  
  dashboardSidebar(disable=FALSE,
                   tags$img(src='logo.png'),
                   
                   
                   # setting the impage  to no appear which collapsing the side bar
                   tags$head(
                     tags$style(HTML("
                                            .sidebar-collapse .sidebar img {
                                              width: 100%;  /* Adjust the width as needed when collapsed */
                                            }
                                          "))
                   ),
                   
                   # tags$head(
                   #   tags$style(HTML(custom_css))
                   # ),
                   
                   width = "300px",
                   ## setting style so the popup for selecting identifier column
                   ## will show the content
                   tags$style(HTML('.popover-title {color:black;}
                                                     .popover-content {color:black;}
                                                      .main-sidebar {z-index:auto;}')),
                   
                   
                   
                   tags$head(
                     tags$style(HTML("
                                          /* Custom CSS for right-to-left direction */
                                          body {
                                            direction: rtl;
                                            text-align: right;
                                          }
                                        "))
                   ),
                   
                   
                   tags$style(HTML("
                          /* Custom CSS to increase the text size in infoBox */
                          .info-box-text {
                            font-size: 30px; /* Adjust the font size as needed */
                          }
                        ")),
                   
                   
                   
                   
                   ## setting dropdown for uploaded data for selecting identifier
                   
                   
                   sidebarMenu(
                     
                     
                     br(),
                     
                     # setting the explanation button to no appear which collapsing the side bar
                     # and set its location at the bottom of the side bar
                     tags$head(
                       tags$style(HTML("
                                                .sidebar-collapse .sidebar #hideshow_test_explanation {
                                                  display: none;  /* Adjust the width as needed when collapsed */
                                                  position: absolute;
                                                }
                                                .sidebar #hideshow_test_explanation {
                                                  position: absolute;
                                                  bottom: 0;
                                                  margin-left: 10px;
                                                }
                                              "))
                     ),
                     
                     
                     actionBttn(
                       inputId = "hideshow_test_explanation",
                       label = "מה יש באפליקציה", 
                       style = "material-flat",size = "md",
                       color = "success",
                     ),
                     
                     shiny::tags$style(type='text/css', "#hideshow_test_explanation {  margin-left: 10px;}"),
                     
                     
                     menuItem("מכבי חיפה",badgeLabel = " 1 ", tabName = "Analyzing_Team",icon = icon("user"))
                     
                     
                     
                   )
  ), # closing dashboardSidebar
  
  dashboardBody(
    #fresh::use_theme(mytheme),
    
    
    
    tags$style(HTML(".sidebar-menu li a { font-size: 17px; }")),
    
    tags$style(
      type="text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    
    # setting width size of modal dialog pop up
    tags$head(tags$style(".modal-lg { width:2200px;  }")),
    #tags$head(tags$style(".modal-lg { height:2200px;  }")),
    
    #tags$head(tags$style(".modal-body{ min-height:2000px}")),
    
    
    shinyjs::useShinyjs(),
    #js function to reset a button, variableName is the button name whose value we want to reset
    shiny::tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                                                     Shiny.onInputChange(variableName, null);
                                                     });
                                                     "),
    
    
    
    tags$head(tags$style(HTML('.form-control{font-weight: bold; font-size: 25px;}'))),
    
    ### setting bs model to view data for selected population 
    
    ### end setting bsmodal for viewing selected population
    
    bsModal("bsmodal_hideshow_test_explanation", "About this application",
            "hideshow_test_explanation", size = "large",
            htmlOutput("app_explanation_output"),
            tags$img(src = 'About this aplication2.png',style="text-align: right;")
    ),
    
    bsModal("bsmodal_hideshow_factor_explanation", "Factor Info",
            "hideshow_factor_explanation", size = "large",
            collapsibleTreeOutput("factor_explanation_output",height = 700,width = 2400),
            reactableOutput("factor_explanation_output_table")
            
    ),
    
    
    tabItems(
      
      ########################################
      #                                      #
      #   Medium Risk Tab                    #
      #                                      #
      ########################################
      
      tabItem(tabName = "Analyzing_Team", 
              
              fluidRow(    
                
                
                ## upper box to select intel employee population
                box(title =  tags$b("בחירת פרמטרים", style = "font-size: 120%;"),
                    status = "primary", solidHeader = TRUE,height = "470px",
                    collapsible = TRUE,width=9,
                    
                    fluidRow(
                      
                      ## Season ## 
                      
                      fluidRow(
                        column(12,
                               column(3,
                                      pickerInput(
                                        inputId = "select_Season",selected = NULL,
                                        options = list(`live-search`=TRUE,style = "btn-default btn-lg",`actions-box` = TRUE),
                                        choicesOpt = list(
                                          style = rep_len("font-size: 120%; line-height: 1.6;", length(Season))
                                        ), 
                                        label = tags$div("עונה", style = "font-size: 150%; text-align: center;"), 
                                        choices = Season,
                                        multiple = TRUE
                                      )),
                               
                               ## opponent ## 
                               column(3,
                                      pickerInput(
                                        inputId = "select_main_opponent",selected = NULL,
                                        options = list(`live-search`=TRUE,style = "btn-default btn-lg",`actions-box` = TRUE),
                                        choicesOpt = list(
                                          style = rep_len("font-size: 120%; line-height: 1.6;", length(Main_Team))
                                        ), 
                                        label =tags$div("יריבה", style = "font-size: 150%; text-align: center;"), 
                                        choices = Main_Team,
                                        multiple = TRUE
                                      )),
                               
                               
                               ## Tournament ##
                               column(3,
                                      pickerInput(
                                        inputId = "select_Tournament",selected = NULL,
                                        options = list(`live-search`=TRUE,style = "btn-default btn-lg",`actions-box` = TRUE),
                                        choicesOpt = list(
                                          style = rep_len("font-size: 120%; line-height: 1.6;", length(Tournament))
                                        ), 
                                        label = tags$div("תחרות", style = "font-size: 150%; text-align: center;"), 
                                        choices = Tournament,
                                        multiple = TRUE
                                      )),
                               
                               
                               
                               ## YEAR ##
                               column(3,
                                      pickerInput(
                                        inputId = "select_YEAR",selected = NULL,
                                        options = list(`live-search`=TRUE,style = "btn-default btn-lg",`actions-box` = TRUE),
                                        choicesOpt = list(
                                          style = rep_len("font-size: 120%; line-height: 1.6;", length(YEAR))
                                        ), 
                                        label = tags$div("שנה", style = "font-size: 150%; text-align: center;"), 
                                        choices = YEAR,
                                        multiple = TRUE
                                      )),
                               
                        )),
                      tags$hr(),  # Add a horizontal line
                      
                      fluidRow(
                        column(12,
                               column(3,
                                      pickerInput(
                                        inputId = "select_EVENT",selected = NULL,
                                        options = list(`live-search`=TRUE,style = "btn-default btn-lg",`actions-box` = TRUE),
                                        choicesOpt = list(
                                          style = rep_len("font-size: 120%; line-height: 1.6;", length(event))
                                        ), 
                                        label = tags$div("מחזור/שלב", style = "font-size: 150%; text-align: center;"), 
                                        choices = event,
                                        multiple = TRUE
                                      )),
                               
                               ## opponent ## 
                               column(3,
                                      pickerInput(
                                        inputId = "select_outcome",selected = NULL,
                                        options = list(`live-search`=TRUE,style = "btn-default btn-lg",`actions-box` = TRUE),
                                        choicesOpt = list(
                                          style = rep_len("font-size: 120%; line-height: 1.6;", length(outcome))
                                        ), 
                                        label =tags$div("תוצאה", style = "font-size: 150%; text-align: center;"), 
                                        choices = outcome_list,
                                        multiple = TRUE
                                      )),
                               
                               
                               ## Tournament ##
                               column(3,
                                      pickerInput(
                                        inputId = "select_players_lineup_names",selected = NULL,
                                        options = list(`live-search`=TRUE,style = "btn-default btn-lg",`actions-box` = TRUE),
                                        choicesOpt = list(
                                          style = rep_len("font-size: 120%; line-height: 1.6;", length(players_lineup_names))
                                        ), 
                                        label = tags$div("שחקני הרכב", style = "font-size: 150%; text-align: center;"), 
                                        choices = players_lineup_names,
                                        multiple = TRUE
                                      )),
                               
                               
                               
                               ## YEAR ##
                               column(3,
                                      pickerInput(
                                        inputId = "select_hosting",selected = NULL,
                                        options = list(`live-search`=TRUE,style = "btn-default btn-lg",`actions-box` = TRUE),
                                        choicesOpt = list(
                                          style = rep_len("font-size: 120%; line-height: 1.6;", length(hosting))
                                        ), 
                                        label = tags$div("משחק בית או חוץ", style = "font-size: 150%; text-align: center;"), 
                                        choices = hosting,
                                        multiple = TRUE
                                      )),
                               
                        )),
                      
                      tags$hr(),  # Add a horizontal line
                      
                      
                      ## MONTH ##
                      column(3,
                             pickerInput(
                               inputId = "select_MONTH",selected = NULL,
                               options = list(`live-search`=TRUE,style = "btn-default btn-lg",`actions-box` = TRUE),
                               choicesOpt = list(
                                 style = rep_len("font-size: 120%; line-height: 1.6;", length(MONTH)),
                                 title=1:12
                               ), 
                               label = tags$div("חודש", style = "font-size: 150%; text-align: center;"), 
                               choices = month_list ,
                               multiple = TRUE
                             )),
                      
                      ## ref ##
                      column(3,
                             pickerInput(
                               inputId = "select_ref",selected = NULL,
                               options = list(`live-search`=TRUE,style = "btn-default btn-lg",`actions-box` = TRUE),
                               choicesOpt = list(
                                 style = rep_len("font-size: 120%; line-height: 1.6;", length(ref))
                               ), 
                               label = tags$div("שופט", style = "font-size: 150%; text-align: center;"), 
                               choices = ref,
                               multiple = TRUE
                             )),
                      
                      column(3,
                             numericInput("scorrer_numeric_input_id", label =tags$div("מבקיעים", style = "font-size: 22px;"), value = 10),
                      ),
                      shiny::tags$style(type='text/css',"#scorrer_numeric_input_id { margin-top: 6px;}"),
                      
                      
                      ## date range ##
                      column(3,
                             dateRangeInput('select_dateRange',
                                            label =tags$div("תאריך", style = "font-size: 150%; text-align: center;"),
                                            start = min(All_Games_Tables$Date), end = Sys.Date()
                                            
                             )),
                      
                      shiny::tags$style(type='text/css',"#select_dateRange { margin-top: 9px; }"),
                      
                      shiny::tags$style(
                        type = 'text/css',
                        "#select_dateRange .input-group-addon { font-size: 13px; }",
                        "#select_dateRange input { font-size: 20px; }"
                      )
                      
                      
                    ) # closing fluid row of all filtering options
                    
                    
                ),
                
                br(),
                # A static infoBox
                column(3,infoBoxOutput("total_games",width = "200px")),
                tags$hr(),  # Add a horizontal line
                column(3,infoBoxOutput("Win",width = "200px")),
                tags$hr(),  # Add a horizontal line
                column(3,infoBoxOutput("Draw",width = "200px")),
                tags$hr(),  # Add a horizontal line
                column(3,infoBoxOutput("Lost",width = "200px"))
          
              ), # closing fluid row for both uploaded data
              br(),
            
                              box(title =  tags$b("מבקיעי שערים מכבי חיפה", style = "font-size: 120%;"),
                                  status = "success",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,width=3,height = "1100px",
                                  #shinycssloaders::withSpinner(plotlyOutput('factors_features_graph',height = "100%"))
                                  
                                  fluidRow(
                                    
                                    column(6,uiOutput("select_maccabi_player")),
                                    
                                    column(6,actionBttn(
                                      inputId = "hideshow_player_details",
                                      label = "נתוני שחקן", 
                                      style = "material-flat",size = "md",
                                      color = "success",
                                    )),
                                    shiny::tags$style(type='text/css', "#hideshow_player_details {  margin-top: 38px; margin-right: 80px;}"),
                                    
                                    bsModal("bsmodal_hideshow_player_details", "נתוני שחקן",
                                            "hideshow_player_details", size = "large",
                                            fluidRow(
                                              column(12,shinycssloaders::withSpinner(uiOutput("player_card_name"))),
                                              column(4,shinycssloaders::withSpinner(highchartOutput("app_maccabi_players_minutes_scored_cut"))),
                                              column(4,shinycssloaders::withSpinner(highchartOutput("app_scorres_filter_games_data_graph"))),
                                              column(4,shinycssloaders::withSpinner(highchartOutput("app_maccabi_players_minutes_scored_cut_pie"))),
                                              column(12,shinycssloaders::withSpinner(reactableOutput("app_maccabi_player_data_output")))
                                            )
                                    )
                                  ),
                                  tags$hr(),  # Add a horizontal line
                                  
                                  shinycssloaders::withSpinner(highchartOutput('scorer_high_graph_maccabi',height = "900px"))
                                  
                              ),
              
              box(title =  tags$b("משחקים", style = "font-size: 120%;"),
                  status = "primary", solidHeader = TRUE,
                  height = "1100px",
                  collapsible = TRUE,
                  width=9,
                  
                  fluidRow(
                    column(3,
                           numericInput("maccabi_games_filter_size", label =tags$div("מספר משחקים להציג", style = "font-size: 22px; margin-right:-900px; "), value = 20),
                    ),
                    shiny::tags$style(type='text/css',"#maccabi_games_filter_size { margin-right:-900px; }"),
                    column(12,shinycssloaders::withSpinner(reactableOutput("app_maccabi_filter_games_data"))),
                    column(6,shinycssloaders::withSpinner(highchartOutput("app_maccabi_filter_games_data_graph"))),
                    column(6,shinycssloaders::withSpinner(highchartOutput("app_maccabi_all_games_minutes_scored_cut")))
                    
                    
                  )
                  
              )
                
              
              
      ) ### closing tabitem for MEDIUM RISK ANALYSIS
      
    ) # closing tabitems (the main one for all the tabs)
  ) # closing dashboard body  
) # closing main dashboard page

options(shiny.maxRequestSize=100*1024^2) 

server <- function(input, output,session) {
  
  
  
  ## main matches filtering data  -------------------------------------------------------------
  
  
  ## setting main data filtered to use afterward in visualization and calculation
  filtered_games_data <- reactive({
    
    
    
    ### country select option
    select_main_opponent <- do.call(c,ifelse(is.null(input$select_main_opponent)==TRUE,
                                             list(Main_Team),
                                             list(input$select_main_opponent)))
    
    
    ### gender select option 
    select_Season <- do.call(c,ifelse(is.null(input$select_Season)==TRUE,
                                      list(Season),
                                      list(input$select_Season)))
    
    ### gradegruop select option 
    select_Tournament <- do.call(c,ifelse(is.null(input$select_Tournament)==TRUE,
                                          list(Tournament),
                                          list(input$select_Tournament)))
    
    
    select_YEAR <- do.call(c,ifelse(is.null(input$select_YEAR)==TRUE,
                                    list(YEAR),
                                    list(input$select_YEAR)))
    
    select_MONTH <- do.call(c,ifelse(is.null(input$select_MONTH)==TRUE,
                                     list(MONTH),
                                     list(input$select_MONTH)))
    
    select_ref <- do.call(c,ifelse(is.null(input$select_ref)==TRUE,
                                   list(ref),
                                   list(input$select_ref)))
    
    select_outcome <- do.call(c,ifelse(is.null(input$select_outcome)==TRUE,
                                       list(outcome),
                                       list(input$select_outcome)))
    
    select_event <- do.call(c,ifelse(is.null(input$select_EVENT)==TRUE,
                                     list(event),
                                     list(input$select_EVENT)))
    
    select_player_lineup <- do.call(c,ifelse(is.null(input$select_players_lineup_names)==TRUE,
                                             list(players_lineup_names),
                                             list(input$select_players_lineup_names)))
    
    # getting the plater lineup id game
    player_lineup_id <- all_lineup_id$games_id[all_lineup_id$Player_Name %in% select_player_lineup]
    
    select_hosting <- do.call(c,ifelse(is.null(input$select_hosting)==TRUE,
                                       list(hosting),
                                       list(input$select_hosting)))
    
    ### visualizing the scorers : 
    
    soccer_data <- All_Games_Tables %>% 
      filter(Main_Team %in% select_main_opponent,
             Season  %in% select_Season,
             Tournament  %in% select_Tournament,
             Year %in% select_YEAR,
             Outcome %in% select_outcome,
             Event %in% select_event,
             Month_Name %in% select_MONTH,
             Game_ID %in% player_lineup_id,
             ref %in% select_ref,
             hosting_type %in% select_hosting,
             Date >= input$select_dateRange[1] , 
             Date <= input$select_dateRange[2]
      )
    
    soccer_data
    
  })
  
  ## setting the reactive to for the entire games talbes view : 
  maccabi_full_games_vis_for_popup <- reactive({
    
    
    
    # here we tabe the player who score goals
    games_results_data <- filtered_games_data() %>% # create the iframe link using the link column
      select(Season,Host,Visitor,Score,Tournament,Event,ref,Date,content,Link,Outcome) %>% 
      mutate(IframeColumn = paste('<iframe width="2000" height="750" src="', paste0(Link,"#item4"), '," frameborder="0" allowfullscreen></iframe>', sep=''))
    
    #  players_results_data <- players_results_data %>% select(Date,ref,Tournament,Score,Visitor,Host,Season,content,Link,IframeColumn) 
    
    games_results_data <- games_results_data %>% select(Season,Tournament,Event,Host,Visitor,Score,ref,Date,content,Link,IframeColumn) 
    
    games_results_data <- games_results_data %>% 
      arrange(desc(Date))  
    
    games_results_data <- games_results_data %>%
      mutate(Score=sapply(strsplit(Score, ":"),
                          function(parts) paste(rev(parts), collapse = ":"))) %>%
      rename(
        "תאריך" = Date,
        "שופט" = ref,
        "תחרות"=Tournament,
        "מחזור/שלב"=Event,
        "תוצאה"=Score,
        "אורח"=Visitor,
        "מארח"=Host,
        "עונה"=Season
      )
    
    ##-------------------------------------------------------
    ## part 2 setting tournament visualization count
    
    
    data_tournament_maccabi <-  filtered_games_data()$Tournament %>% table
    
    # converting maccabi goals to table
    data_tournament_df <- data.frame(tournament = names(data_tournament_maccabi), Count = as.numeric(data_tournament_maccabi))
    
    # sorting from hghest to lowest
    data_tournament_df <- data_tournament_df %>% arrange(desc(Count))
    
    #--------------------------------------------
    # part 3 setting the overall score goals time
    # getting players obs location when the score in a 
    
    maccabin_as_host=filtered_games_data() %>% filter(Host=="מכבי חיפה") %>% select(updated_goals_scored_a)
    maccabi_as_visitor=filtered_games_data() %>% filter(Visitor=="מכבי חיפה") %>% select(updated_goals_scored_b)
    all_maccabi_goals=bind_rows(maccabin_as_host$updated_goals_scored_a,maccabi_as_visitor$updated_goals_scored_b)
    
    
    all_maccabi_goals$MinuteRange <- cut(all_maccabi_goals$Minutes, breaks = seq(0, 120, by = 15), 
                                         labels = c("0-15", "15-30", "30-45", "45-60", "60-75", "75-90","90-105" ,"105-120"))
    
    
    data_summary_cut_goals=all_maccabi_goals %>%
      group_by(MinuteRange) %>%
      summarise(total_goals = n())
    
    
    list(games_results_data=games_results_data,
         data_tournament_df=data_tournament_df,
         data_summary_cut_goals=data_summary_cut_goals)
    
    
  })
  
  ## showing filtered list of games in tables ---------------------------------------------------------
  
  
  ### INSERTING PLAYER WHO SCORED GAMES INFO
  output$app_maccabi_filter_games_data <- renderReactable({
    
    
    ### creatin the reactable view
    reactable(maccabi_full_games_vis_for_popup()$games_results_data[1:input$maccabi_games_filter_size,] %>% select(-content,-Link,-IframeColumn),
              details = function(index) {
                
                sub_rows_display <- maccabi_full_games_vis_for_popup()$games_results_data[1:input$maccabi_games_filter_size,] %>%
                  filter(Link==maccabi_full_games_vis_for_popup()$games_results_data[1:input$maccabi_games_filter_size,]$Link[index]) %>% 
                  select(IframeColumn)
                
                htmltools::div(style = "padding: 16px",
                               # displaying using datatable the iframe of the game link 
                               datatable(sub_rows_display, escape = FALSE,colnames = "",
                                         options = list(
                                           searching = FALSE,  # Remove search box
                                           paging = FALSE       # Remove page numbers
                                         ))
                               
                )
              }, ## end details for nexted table
              #searchable = TRUE,
              rownames = FALSE,
              striped = TRUE,
              highlight = TRUE,
              bordered = TRUE,
              filterable = TRUE,
              theme = reactableTheme(
                borderColor = "#dfe2e5",
                stripedColor = "#f6f8fa",
                highlightColor = "#f0f5f9",
                cellPadding = "8px 12px",
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",fontSize = "16px"),
                searchInputStyle = list(width = "100%"),
                rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
              ),
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                #   cell = function(value) format(value, nsmall = 1),
                align = "center",
                #  minWidth = 70,
                headerStyle = list(background = "#f7f7f8"),
                style =list(fontWeight = "bold")
              ),
              columns = list(
                Candidate = colDef(minWidth = 40)  # overrides the default
              ),
              #selection = "multiple", onClick = "select",
              wrap = FALSE,
              resizable = TRUE
              
    ) ## closing main reactable
    
    
    
  })
  
  ### INSERTING games  WHO SCORED GAMES INFO
  output$app_maccabi_all_games_minutes_scored_cut <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = maccabi_full_games_vis_for_popup()$data_summary_cut_goals$MinuteRange) %>%
      hc_add_series(name = "סך גולים", data = maccabi_full_games_vis_for_popup()$data_summary_cut_goals$total_goals,color = "#17B169") %>%
      hc_tooltip(valueDecimals = 0,style = list(
        fontSize = "20px",  # Adjust the font size as needed
        fontWeight = "bold"  # Make it bold
      )) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE, format = "{point.y}"))) %>%
      hc_yAxis(title = list(text = "כמות גולים")) %>%
      hc_title(text = "גולים כל 15 דקות") %>%
      hc_legend(enabled = FALSE) %>%
      hc_add_theme(hc_theme_ggplot2()) 
    
  })
  
  ## adding to games table pop up also the barchart of tournament count
  
  ## setting goals of the opponent
  output$app_maccabi_filter_games_data_graph <- renderHighchart({
    
    # visualizing results
    # visualizing results
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "התפלגות תחרויות") %>%
      hc_xAxis(categories =   maccabi_full_games_vis_for_popup()$data_tournament_df$tournament,
               labels = list(
                 style = list(
                   fontSize = "15px",  # Adjust the font size as needed
                   fontWeight = "bold"  # Make it bold
                 )
               )) %>%
      hc_yAxis(
        yAxis = list(
          #pointPadding = 0.9,  # Adjust as needed
          groupPadding = 1
          # pointWidth = 50  # Adjust the width as needed
        ),
        title = list(text = ""),
        labels = list(
          style = list(
            fontSize = "15px",  # Adjust the font size as needed
            fontWeight = "bold"  # Make it bold
          )
        )
      ) %>%
      hc_tooltip(pointFormat = ' {point.y} סה"כ תחרות',style = list(
        fontSize = "20px",  # Adjust the font size as needed
        fontWeight = "bold"  # Make it bold
      )) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE, format = "{y}"),
          pointPadding = 0.3,  # Adjust as needed
          #groupPadding = 0.1,   # Adjust as needed
          pointWidth = 15  # Adjust the width as needed
        )
      ) %>%
      hc_add_series(
        name = "Player Scores",
        data =   maccabi_full_games_vis_for_popup()$data_tournament_df$Count
        # color = "#17B169"  # Nice green color
      )
    
    
    
  })
  
  
  
  #-----------------------------------------------------------------------------------------------------
  ## players scorrer select input creator -------------------------------------------------------------
  #-----------------------------------------------------------------------------------------------------
  
  
  ## setting render ui for employee org levles for deeper factor analysis
  output$select_maccabi_player <- renderUI({
    
    goals_maccabi_players <-  bind_rows(
      do.call(bind_rows,filtered_games_data() %>%  # getting goals when maccabi is hosting
                filter(Host=="מכבי חיפה") %>% 
                select(updated_goals_scored_a))
      ,
      do.call(bind_rows,filtered_games_data() %>%  # getting goals when maccabi is visiting
                filter(Visitor=="מכבי חיפה") %>% 
                select(updated_goals_scored_b))
    ) %>% select(Name) %>% pull %>% unique %>% sort
    
    
    pickerInput(
      inputId = "select_player",selected = NULL,
      options = list(`live-search`=TRUE,style = "btn-default btn-lg",`actions-box` = TRUE,
                     size=10,`selected-text-format` = "count > 0"),
      choicesOpt = list(
        style = rep_len("font-size: 120%; line-height: 1.6;", length(goals_maccabi_players))
      ), 
      label = tags$div("שחקן מכבי", style = "font-size: 150%; text-align: center;"), 
      choices = goals_maccabi_players,
      multiple = TRUE
    )
    
    
  })
  
  
  ## popup button players goals info  -------------------------------------------------------------
  
  maccabi_scorres_for_popup <- reactive({
    
    
    ## -------------------------------
    
    # getting players obs location when the score in a 
    scored_a_location <- do.call(c,
                                 
                                 lapply(
                                   filtered_games_data()$updated_goals_scored_a,function(x){
                                     
                                     any(x$Name %in% input$select_player)
                                     
                                   })) %>% which
    
    # getting players obs location when the score in b 
    scored_b_location <- do.call(c,
                                 
                                 lapply(
                                   filtered_games_data()$updated_goals_scored_b,function(x){
                                     
                                     any(x$Name %in% input$select_player)
                                     
                                   })) %>% which
    
    ##------------------------------------------------------
    ## adding the bargraph for displaying the goals by 15 minutes cut 
    
    # here we tabe the player who score goals
    players_results_data <- filtered_games_data()[c(scored_a_location,
                                                    scored_b_location),] 
    
    ### - setting the tournament count #### 
    
    ## setting the tournament count visualization : 
    
    ## part 1 setting tournament visualization count
    
    ## -------------------------------
    
    data_tournament_maccabi <-  players_results_data$Tournament %>% table
    
    # converting maccabi goals to table
    data_tournament_df <- data.frame(tournament = names(data_tournament_maccabi), Count = as.numeric(data_tournament_maccabi))
    
    # sorting from hghest to lowest
    data_tournament_df <- data_tournament_df %>% arrange(desc(Count))
    
    ### end setting tournamnet count 
    
    players_results_data_for_pie=players_results_data
    
    data_players_goals_min <-  bind_rows(players_results_data$updated_goals_scored_a,
                                         players_results_data$updated_goals_scored_b) %>% filter(Name  %in% input$select_player)
    
    data_players_goals_min$MinuteRange <- cut(data_players_goals_min$Minutes, breaks = seq(0, 120, by = 15), 
                                              labels = c("0-15", "15-30", "30-45", "45-60", "60-75", "75-90","90-105" ,"105-120"))
    
    data_summary_cut_goals=data_players_goals_min %>%
      group_by(MinuteRange) %>%
      summarise(total_goals = n())
    
    ##-----------------------------------------------------
    ## setting table for dispaly with iframe
    
    # here we tabe the player who score goals
    players_results_data <- players_results_data %>% # create the iframe link using the link column
      select(Season,Host,Visitor,Score,Tournament,ref,Date,content,Link,Outcome) %>% 
      mutate(IframeColumn = paste('<iframe width="2000" height="750" src="', paste0(Link,"#item4"), '," frameborder="0" allowfullscreen></iframe>', sep=''))
    
    #  players_results_data <- players_results_data %>% select(Date,ref,Tournament,Score,Visitor,Host,Season,content,Link,IframeColumn) 
    
    players_results_data <- players_results_data %>% select(Season,Tournament,Host,Visitor,Score,ref,Date,content,Link,IframeColumn) 
    players_results_data <- players_results_data %>% arrange(desc(Date)) # ordering the games by date from latest to oldest
    
    players_results_data <- players_results_data %>%
      mutate(Score=sapply(strsplit(Score, ":"),
                          function(parts) paste(rev(parts), collapse = ":"))) %>%
      rename(
        "תאריך" = Date,
        "שופט" = ref,
        "תחרות"=Tournament,
        "תוצאה"=Score,
        "אורח"=Visitor,
        "מארח"=Host,
        "עונה"=Season
      )
    
    # print(players_results_data)
    # print(data_summary_cut_goals)
    
    list(players_results_data=players_results_data,
         data_summary_cut_goals=data_summary_cut_goals,
         players_results_data_for_pie=players_results_data_for_pie,
         data_tournament_df=data_tournament_df)
    
    
  })
  
  output$player_card_name <- renderUI({
    player_name <- input$select_player
    HTML(paste0("<div style='text-align: center; font-size: 60px; font-weight: bold;'>", player_name, "</div>"))
  })
  
  
  
  ### INSERTING PLAYER WHO SCORED GAMES INFO
  output$app_maccabi_players_minutes_scored_cut_pie <- renderHighchart({
    
    # Summarize soccer_data
    result_summary <- maccabi_scorres_for_popup()$players_results_data_for_pie %>%
      group_by(Outcome) %>%
      summarise(result_count = n())
    
    # Define your desired colors
    colors <- data.frame(outcome=c("Draw","Lost","Won"),color=c("#4682B4","#FF6347","#17B169"))   
    # keeping colors only for the outomce resulkts
    colors <- colors %>% filter(outcome %in% result_summary$Outcome)
    
    #visualizing the outcome for a player when he scored
    result_summary %>%
      hchart("pie", hcaes(x = Outcome, y = result_count))%>%
      hc_title(text = "התפלגות תוצאות") %>%
      hc_add_theme(hc_theme_ggplot2()) %>%
      hc_colors(colors$color)%>% 
      hc_tooltip(pointFormat = "{point.category}: {point.y}" ,style = list(
        fontSize = "20px",  # Adjust the font size as needed
        fontWeight = "bold"  # Make it bold
      ))
    
    
    
    
  })
  
  
  ### INSERTING PLAYER WHO SCORED GAMES INFO
  output$app_maccabi_players_minutes_scored_cut <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = maccabi_scorres_for_popup()$data_summary_cut_goals$MinuteRange) %>%
      hc_add_series(name = "סך גולים", data = maccabi_scorres_for_popup()$data_summary_cut_goals$total_goals,color = "#17B169") %>%
      hc_tooltip(valueDecimals = 0,style = list(
        fontSize = "20px",  # Adjust the font size as needed
        fontWeight = "bold"  # Make it bold
      )) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE, format = "{point.y}"))) %>%
      hc_yAxis(title = list(text = "כמות גולים")) %>%
      hc_title(text = "גולים כל 15 דקות") %>%
      hc_legend(enabled = FALSE) %>%
      hc_add_theme(hc_theme_ggplot2()) 
    
  })
  
  ## setting goals of the opponent
  output$app_scorres_filter_games_data_graph <- renderHighchart({
    
    # visualizing results
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "התפלגות תחרויות") %>%
      hc_xAxis(categories =   maccabi_scorres_for_popup()$data_tournament_df$tournament,
               labels = list(
                 style = list(
                   fontSize = "15px",  # Adjust the font size as needed
                   fontWeight = "bold"  # Make it bold
                 )
               )) %>%
      hc_yAxis(
        yAxis = list(
          #pointPadding = 0.9,  # Adjust as needed
          groupPadding = 1
          # pointWidth = 50  # Adjust the width as needed
        ),
        title = list(text = ""),
        labels = list(
          style = list(
            fontSize = "15px",  # Adjust the font size as needed
            fontWeight = "bold"  # Make it bold
          )
        )
      ) %>%
      hc_tooltip(pointFormat = '{point.y} סה"כ תחרות',style = list(
        fontSize = "20px",  # Adjust the font size as needed
        fontWeight = "bold"  # Make it bold
      )) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE, format = "{y}"),
          pointPadding = 0.3,  # Adjust as needed
          #groupPadding = 0.1,   # Adjust as needed
          pointWidth = 15  # Adjust the width as needed
        )
      ) %>%
      hc_add_series(
        name = "Player Scores",
        data =   maccabi_scorres_for_popup()$data_tournament_df$Count
        # color = "#17B169"  # Nice green color
      )
    
    
    
  })
  
  
  
  
  ### INSERTING PLAYER WHO SCORED GAMES INFO
  output$app_maccabi_player_data_output <- renderReactable({
    
    
    ### creatin the reactable view
    reactable(maccabi_scorres_for_popup()$players_results_data %>% select(-content,-Link,-IframeColumn),
              details = function(index) {
                
                sub_rows_display <- maccabi_scorres_for_popup()$players_results_data %>%
                  filter(Link==maccabi_scorres_for_popup()$players_results_data$Link[index]) %>% 
                  select(IframeColumn)
                
                htmltools::div(style = "padding: 16px",
                               # displaying using datatable the iframe of the game link 
                               datatable(sub_rows_display, escape = FALSE,colnames = "",
                                         options = list(
                                           searching = FALSE,  # Remove search box
                                           paging = FALSE       # Remove page numbers
                                         ))
                               
                )
              }, ## end details for nexted table
              #searchable = TRUE,
              rownames = FALSE,
              striped = TRUE,
              highlight = TRUE,
              bordered = TRUE,
              filterable = TRUE,
              theme = reactableTheme(
                borderColor = "#dfe2e5",
                stripedColor = "#f6f8fa",
                highlightColor = "#f0f5f9",
                cellPadding = "8px 12px",
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",fontSize = "16px"),
                searchInputStyle = list(width = "100%"),
                rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
              ),
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                #   cell = function(value) format(value, nsmall = 1),
                align = "center",
                #  minWidth = 70,
                headerStyle = list(background = "#f7f7f8"),
                style =list(fontWeight = "bold")
              ),
              columns = list(
                Candidate = colDef(minWidth = 40)  # overrides the default
              ),
              #selection = "multiple", onClick = "select",
              wrap = FALSE,
              resizable = TRUE
              
    ) ## closing main reactable
    
    
  })
  
  ## Visualizing maccabi goal scorrer -------------------------------------------------------------
  
  # setting all goals of maccabi based on filtering
  output$scorer_high_graph_maccabi <- renderHighchart({
    
    goals_maccabi_players_calculation <-  bind_rows(
      do.call(bind_rows,filtered_games_data() %>%  # getting goals when maccabi is hosting
                filter(Host=="מכבי חיפה") %>%
                select(updated_goals_scored_a))
      ,
      do.call(bind_rows,filtered_games_data() %>%  # getting goals when maccabi is visiting
                filter(Visitor=="מכבי חיפה") %>%
                select(updated_goals_scored_b))
    ) %>% select(Name) %>% pull
    
    # creating the list of scoring maccabe
    select_player <- goals_maccabi_players_calculation %>% unique %>% sort
    # getting the table count scored for each plater
    goals_maccabi <- goals_maccabi_players_calculation  %>% table
    
    # setting table of scored gaols count
    player_score_df <- data.frame(Player_Score = names(goals_maccabi), Count = as.numeric(goals_maccabi))
    
    # setting the list of maccabi player from the select option
    select_player <- do.call(c,ifelse(is.null(input$select_player)==TRUE,
                                      list(select_player),
                                      list(input$select_player)))
    
    # arranging and keepijg only selected platyes
    player_score_df <- player_score_df %>% 
      filter(Player_Score %in% select_player) %>%
      arrange(desc(Count))  
    
    # showing up to selected amount of plyayers and removing na if we select some plyaers the
    # are lower then the amount of players we want to see , we get na so we remove them
    player_score_df <- player_score_df[1:input$scorrer_numeric_input_id,] 
    player_score_df <- player_score_df[complete.cases(player_score_df),]
    
    
    
    # visualizing results
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "גולים שחקן מכבי") %>%
      hc_xAxis(categories = as.character(player_score_df$Player_Score),
               labels = list(
                 style = list(
                   fontSize = "15px",  # Adjust the font size as needed
                   fontWeight = "bold"  # Make it bold
                 )
               )) %>%
      hc_yAxis(
        yAxis = list(
          #pointPadding = 0.9,  # Adjust as needed
          groupPadding = 1
          # pointWidth = 50  # Adjust the width as needed
        ),
        title = list(text = ""),
        labels = list(
          style = list(
            fontSize = "15px",  # Adjust the font size as needed
            fontWeight = "bold"  # Make it bold
          )
        )
      ) %>%
      hc_tooltip(pointFormat = "{point.category}: {point.y}" ,style = list(
        fontSize = "20px",  # Adjust the font size as needed
        fontWeight = "bold"  # Make it bold
      )) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE, format = "{point.y}"),
          pointPadding = 0.3,  # Adjust as needed
          #groupPadding = 0.4,   # Adjust as needed
          pointWidth = 15  # Adjust the width as needed
        )
      ) %>%
      hc_add_series(
        name = "Player Scores",
        data = player_score_df$Count,
        color = "#17B169"  # Nice green color
      )
    
    
    
  })
  
  ## Visualizing opponent goal scorrer -------------------------------------------------------------
  
  ## setting goals of the opponent
  output$scorer_high_graph_opponent <- renderHighchart({
    
    
    
    goals_maccabi_players_calculation <-  bind_rows(
      do.call(bind_rows,filtered_games_data() %>%  # getting goals when maccabi is hosting
                filter(Host!="מכבי חיפה") %>%
                select(updated_goals_scored_a))
      ,
      do.call(bind_rows,filtered_games_data() %>%  # getting goals when maccabi is visiting
                filter(Visitor!="מכבי חיפה") %>%
                select(updated_goals_scored_b))
    ) %>% select(Name) %>% pull
    
    select_player <- goals_maccabi_players_calculation %>% unique %>% sort
    goals_maccabi <- goals_maccabi_players_calculation  %>% table
    
    
    
    player_score_df <- data.frame(Player_Score = names(goals_maccabi), Count = as.numeric(goals_maccabi))
    
    
    select_player <- do.call(c,ifelse(is.null(input$select_player)==TRUE,
                                      list(select_player),
                                      list(input$select_player)))
    
    player_score_df <- player_score_df %>% 
      filter(Player_Score %in% select_player) %>%
      arrange(desc(Count))  
    
    player_score_df <- player_score_df[1:input$scorrer_numeric_input_id,] 
    player_score_df <- player_score_df[complete.cases(player_score_df),]
    
    print(player_score_df)
    
    
    
    # visualizing results
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "גולים שחקן יריבה") %>%
      hc_xAxis(categories = as.character(player_score_df$Player_Score),
               labels = list(
                 style = list(
                   fontSize = "15px",  # Adjust the font size as needed
                   fontWeight = "bold"  # Make it bold
                 )
               )) %>%
      hc_yAxis(
        yAxis = list(
          #pointPadding = 0.9,  # Adjust as needed
          groupPadding = 1
          # pointWidth = 50  # Adjust the width as needed
        ),
        title = list(text = "גולים יריבה"),
        labels = list(
          style = list(
            fontSize = "15px",  # Adjust the font size as needed
            fontWeight = "bold"  # Make it bold
          )
        )
      ) %>%
      hc_tooltip(pointFormat = "{point.category}: {point.y}" ,style = list(
        fontSize = "20px",  # Adjust the font size as needed
        fontWeight = "bold"  # Make it bold
      )) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(enabled = TRUE, format = "{y}"),
          pointPadding = 0.3,  # Adjust as needed
          #groupPadding = 0.4,   # Adjust as needed
          pointWidth = 15  # Adjust the width as needed
        )
      ) %>%
      hc_add_series(
        name = "Player Scores",
        data = player_score_df$Count
        #color = "#17B169"  # Nice green color
      )
    
    
    
  })
  
  ## info box for win lose draw  -------------------------------------------------------------
  
  
  # setting cards view of the winds draw and loses
  output$Win <- renderInfoBox({
    
    # setting outcome frequency count
    results=table(filtered_games_data()$Outcome)
    # setting proportation for outcome
    prop_results=prop.table(results) %>% round(4)*100
    
    
    infoBox(
      "ניצחונות",paste0(results[names(results)=="Won"]," יחס אחוזים ", paste0(prop_results[names(prop_results)=="Won"],"%")),
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$Draw <- renderInfoBox({
    # setting outcome frequency count
    results=table(filtered_games_data()$Outcome)
    # setting proportation for outcome
    prop_results=prop.table(results) %>% round(4)*100
    
    
    infoBox(
      "תיקו", paste0(results[names(results)=="Draw"]," יחס אחוזים ", paste0(prop_results[names(prop_results)=="Draw"],"%")), 
      icon = icon("refresh", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  
  output$Lost <- renderInfoBox({
    
    # setting outcome frequency count
    results=table(filtered_games_data()$Outcome)
    # setting proportation for outcome
    prop_results=prop.table(results) %>% round(4)*100
    
    infoBox(
      "הפסדים", paste0(results[names(results)=="Lost"]," יחס אחוזים ", paste0(prop_results[names(prop_results)=="Lost"],"%")),
      icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$total_games <- renderInfoBox({
    
    # setting outcome frequency count
    results=table(filtered_games_data()$Outcome) %>% sum(na.rm = F)
    # setting proportation for outcome
    
    infoBox(
      'סה"כ משחקים',results ,
      icon = icon("credit-card", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  
} # end of server

shinyApp(ui = ui, server = server)
