#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggiraph)
library(gfonts)

source("kilanttrify.R")
list_SP<-c("SP1","SP2","SP3","SP4","SP5","SP6","SP7","SP234")
list_TF<-c("PPGIS","Scenarios","Fieldwork","Communication","NatureFutures")

gdtools::register_gfont(family = "Roboto")
gdtools::register_gfont(family = "Roboto Condensed")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$link(rel = "stylesheet", type = "text/css", href = "css/roboto.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/satisfy.css"),
  tags$style("body {font-family: 'Roboto', sans-serif;}"),
  tags$style("h2 {font-family: 'Satisfy', sans-serif;
  font-size: 50px;
  text-align: center;
  color: #6f2c91;
  text-shadow: 3px 3px 3px #grey; 
  text-decoration: none;}"),
  tags$style("h2 a {text-align:center;
  font-size: 50px;
  text-align: center;
  color: #6f2c91;
  text-shadow: 3px 3px 3px #grey;}"),
  tags$style("h2 a:hover {text-align:center;
  font-size: 50px;
  text-align: center;
  color: #a6ce39;
  text-shadow: 3px 3px 3px #grey;}"),
  tags$style("h3 a {font-family: 'Satisfy', sans-serif; fonts-size: 30px; text-align: center; color: #6f2c91; text-shadow: 3px 3px 3px #grey;"),
  tags$style("h3 a:hover {font-family: 'Satisfy', sans-serif; fonts-size: 30px; text-align: center; color: #a6ce39; text-shadow: 3px 3px 3px #grey;"),
  tags$style("h3 {font-family: 'Satisfy', sans-serif; fonts-size: 30px; text-align: center; color: #a6ce39; text-shadow: 3px 3px 3px #grey;"),
  tags$style("h4 {font-family: 'Roboto', sans-serif; font-weight: bold; text-shadow: 3px 3px 3px #grey;text-decoration: underline; text-align: center;"),
  tags$style("h4 a {font-family: 'Roboto', sans-serif; font-weight: bold; text-shadow: 3px 3px 3px #grey;text-decoration: underline; text-align: center; color: #6f2c91;"),
  tags$style("h4 a:hover {font-family: 'Roboto', sans-serif; font-weight: bold; text-shadow: 3px 3px 3px #grey;text-decoration: underline; text-align: center; color: #a6ce39;"),
  tags$style(".col-sm-8 {text-align: center;}"),
  tags$style(".col-sm-8 table {text-align: center;}"),
  tags$style("#current_project_table {width:95%;text-align: center;}"),
  tags$style("#current_spots_table {width:95%;text-align: center;}"),
  gdtools::addGFontHtmlDependency(family = "Roboto"),
  gdtools::addGFontHtmlDependency(family = "Roboto Condensed"),
  
  shiny::titlePanel(title = tags$a("Kili-SES task management")#, #href='https://github.com/giocomai//'
                    ),#windowTitle = " - A tool by EDJNet"
  
  sidebarLayout(
    sidebarPanel(
      #shiny::radioButtons(inputId = "source_type",
       #                   label = "Source for project information",
        #                  choices = c(#demo = "demo",
         #                             `Google spreadsheet` = "googledrive"
                                      #`CSV files` = "csv",
                                      #`Microsoft Excel spreadsheet` = "xlsx"
          #                            )),
      
     # conditionalPanel(
      #  condition = "input.source_type == 'googledrive'",
       # shiny::textInput(inputId = "googledrive_link",
        #                 label = "Link to Google Drive spreadsheet",
         #                value = ""),# add google sheet link here
    #    shiny::helpText("Note: the Google Drive spreadsheet must be visibile by anyone with the link; to have a properly structured spreadsheet, the easiest way is to copy the example document: 'File' -> 'Create a copy'")
     # ),
     # conditionalPanel(
      #  condition = "input.source_type == 'csv'",
     #   shiny::fileInput(inputId = "project_file", label = "Upload csv of project"),
        #shiny::fileInput(inputId = "spot_file", label = "Upload csv of spots")
     #   ),
      #conditionalPanel(
       # condition = "input.source_type == 'xlsx'",
        #shiny::fileInput(inputId = "project_file_xlsx", label = "Upload xlsx of project")
     #),
      
      shiny::textInput(inputId = "plot_start_date", label = "Starting date of the chart", value = substring( "2025-03-01", first = 1, last = 10)),#text = as.character(Sys.Date())
      shiny::textInput(inputId = "plot_end_date", label = "Ending date of the chart", value = substring("2029-02-28", first = 1, last = 10)), #text = as.character(Sys.Date())
      shiny::checkboxInput(inputId = "within_SP", label = "Show only one subproject", value = FALSE),
      conditionalPanel(
      condition = "input.within_SP == true",
      shiny::selectInput("within_SP_list", "Subproject:",choices=list_SP,
                         multiple=T,selected=c("SP7"))
      ),
      shiny::checkboxInput(inputId = "only_fieldwork", label = "Show only fieldwork tasks", value = FALSE),
      shiny::checkboxInput(inputId = "within_TaskForce", label = "Show only one task force", value = FALSE),
      conditionalPanel(
        condition = "input.within_TaskForce == true",
        shiny::selectInput("within_TF_list", "Taskforce:",choices=list_TF,
                           multiple=F)
      ),
      #shiny::radioButtons(inputId = "by_date_radio", label = "Input timing format", choices = c("By project month number", "By date")),
      #conditionalPanel(
       # condition = "input.by_date_radio == 'By date'",
      #  shiny::radioButtons(inputId = "precision_radio", label = "Precision of the timeline", choices = c("Month", "Day"))),
      shiny::checkboxInput(inputId = "customisation_check", label = "Show additional customisation options", value = FALSE),
      conditionalPanel(
        condition = "input.customisation_check == true",
        shiny::radioButtons(inputId = "text_alignment", label = "Text alignment", choices = c("left", "right"), selected = "right", inline = TRUE),
        shiny::checkboxInput(inputId = "show_dependencies", label = "Include the dependency arrows", value = TRUE),
        shiny::checkboxInput(inputId = "mark_quarters", label = "Add vertical lines to mark quarters", value = TRUE),
        shiny::checkboxInput(inputId = "show_wp", label = "Show the Work package", value = FALSE),
        conditionalPanel(
          condition = "input.show_wp == true",
        shiny::sliderInput(inputId = "size_wp", label = "Thickness of the line for working packages", min = 1, max = 10, value = 6, step = 1, round = TRUE)
        ),
        shiny::sliderInput(inputId = "size_activity", label = "Thickness of the line for activities", min = 1, max = 10, value = 1, step = 1, round = TRUE),
        shiny::sliderInput(inputId = "size_text_relative", label = "Relative size of all text", value = 60, min = 50, max = 250, round = TRUE, post = "%"),
        shiny::sliderInput(inputId = "alpha_activity", label = "Transparency of the line for activities", value = 0.6, min = 0, max = 1, round = TRUE)
        #shiny::selectInput(inputId = "wes_palette", label = "Pick colour palette", choices = names(wesanderson::wes_palettes), selected = "Darjeeling1", multiple = FALSE),
        #shiny::checkboxInput(inputId = "custom_palette_check", label = "Custom colour palette?", value = FALSE),
        #conditionalPanel(
         # condition = "input.custom_palette_check == true",
         # shiny::textInput(inputId = "custom_palette_text", label = "Custom hex values", value = "#a6ce39,#6f2c91,#a6ce39"),
         # shiny::helpText("Insert comma-separated hex values, with no spaces."))
       ),
      shiny::HTML("<hr />"),
      shiny::h3(tags$a("A tool for the Kili-SES", href='https://kili-ses.senckenberg.de/')),
      shiny::h3(tags$a(tags$img(src = "img/Kili_SES-Logo_Farbe.png",style="width: 200px")))
    ),
    mainPanel(
      shiny::actionButton("go", "Update chart"),
      ggiraph::girafeOutput("gantt"), #shiny::plotOutput(outputId = "gantt"),
      shiny::downloadButton(outputId = "download_gantt_png",
                            label =  "Download chart as image (png)"),
      shiny::downloadButton(outputId = "download_gantt_pdf",
                            label =  "Download chart in pdf"),
      shiny::downloadButton(outputId = "download_gantt_svg",
                            label =  "Download chart in svg"),
      shiny::actionButton(inputId = "a4_button", label = "Set download size to horizontal A4"),
      helpText("ⓘ - As the preview above adapts to your screen, the image preview will not match the one you download. Nothing stops you from downloading the preview image if you like it that way."), 
      div(style="display: inline-block; vertical-align:top; width: 200px;",
          shiny::numericInput(inputId = "download_width",
                              label = "Download width (in cm)",
                              value = 18,
                              min = 1)),
      div(style="display: inline-block; vertical-align:top; width: 30px;",HTML("<br>")),
      div(style="display: inline-block; vertical-align:top; width: 200px;",
          shiny::numericInput(inputId = "download_height",
                              label = "Download height (in cm)",
                              value = 9,
                              min = 1)),
      shiny::hr(),
      shiny::h4("Project data"), 
      shiny::tableOutput(outputId = "current_project_table"),
      #shiny::h4("Spot occurrences"), 
     #shiny::tableOutput(outputId = "current_spots_table"),
      #shiny::h4(tags$a("Source code and documentation available on GitHub", href='https://github.com/giocomai/')),
     shiny::h4(tags$a("Adapted by G. Bocksberger from work by Giorgio Comai", href='https://giorgiocomai.eu/')),
      )
  )
  
)
# Define server logic required to draw a histogram
if (requireNamespace("extrafont", quietly = TRUE)) {
  library("extrafont", quietly = TRUE)
  extrafont::loadfonts(device = "pdf", quiet = TRUE)
}

server <- function(input, output, session) {
  
  current_project_df <- shiny::eventReactive(
    {
      input$go
      #input$by_date_radio
      #input$precision_radio
      input$within_SP
      input$within_TaskForce
      
      #updateSelectInput(session, "within_SP_list", 
       #               choices = list_SP, 
       #                 selected = "SP7")
      # input$googledrive_link
      # input$project_file
      # input$project_file_xlsx
      # input$spot_file
    }, {
      
    #  if (input$source_type=="demo") {
     #   if (input$by_date_radio=="By date") {
     #     if (input$precision_radio=="Day") {
     #       ::test_project_date_day
     #     } else {
      #      ::test_project_date_month 
      #    }
      #  } else {
      #    ::test_project
      #  }
        
     # } else 
    #  if (input$source_type=="csv") {
     #   req(input$project_file)
     #   readr::read_csv(file = input$project_file$datapath,na =c("","NA","na"))%>%dplyr::filter(!is.na(start_date),!is.na(end_date))
     # } else 
     # if (input$source_type=="googledrive") {
        googlesheets4::gs4_deauth()
        googlesheets4::read_sheet(ss = "",# enter google sheet name here #input$googledrive_link,
                                  sheet = 1,
                                  col_names = TRUE,na =c("","NA","na"),
                                  col_types = "c")%>%dplyr::filter(!is.na(start_date),!is.na(end_date))
     # } else if (input$source_type=="xlsx") {
     #   req(input$project_file_xlsx)
     #   readxl::read_excel(path = input$project_file_xlsx$datapath, sheet = 1,na =c("","NA","na"))%>%dplyr::filter(!is.na(start_date),!is.na(end_date))
    #  }
    }, ignoreNULL = FALSE)
  
  output$current_project_table <- renderTable(
    if (is.null(current_project_df())==FALSE) {
      if (input$within_SP==TRUE) {
        filter(current_project_df(),SP==input$within_SP_list)
      } else {
      current_project_df()
    }},
    striped = TRUE,
    hover = TRUE, 
    digits = 0,
    align = "c",
    width = "100%")
  

  gantt_chart <- shiny::reactive({
   
    gantt_gg <- kilanttrify(project = current_project_df(),
                                     #plot_start_date = input$plot_start_date,
                                      set_date_limit=c(input$plot_start_date,input$plot_end_date),
                                     #spots = if (length(current_spots_df())>0) {
                                      # if (tibble::is_tibble(current_spots_df())&nrow(current_spots_df()>0)) {
                                      #   current_spots_df()}
                                      # else {NULL}} else {NULL},
                                     by_date = TRUE ,#ifelse(input$by_date_radio=="By date", TRUE, FALSE),
                                     exact_date = TRUE, #ifelse(input$precision_radio=="Day", ifelse(input$by_date_radio=="By date", TRUE, FALSE), FALSE),
                                     mark_quarters = input$mark_quarters,
                                     font_family = "Roboto Condensed",
                                     show_dependencies =  input$show_dependencies,
                                     size_wp = input$size_wp,
                                     within_SP=input$within_SP,
                                     within_SP_list=input$within_SP_list,
                                     only_fieldwork=input$only_fieldwork,
                                     within_TaskForce=input$within_TaskForce,
                                     within_TF_list=input$within_TF_list,
                                     mark_years = T,fieldwork_alpha=0.7,alpha_wp=1,
                                     show_wp =input$show_wp,
                                     alpha_activity = input$alpha_activity,  
                                     size_activity = input$size_activity,
                                     size_text_relative = input$size_text_relative/100,
                                     axis_text_align = input$text_alignment
                                     #colour_palette = unlist(ifelse(test = input$custom_palette_check, strsplit(input$custom_palette_text, split = ","), list(as.character(wesanderson::wes_palette(input$wes_palette)))))
                            )
   # if (ggplot2::is.ggplot(gantt_gg)==FALSE) {
   #   warning("Please make sure that you have provided properly formatted data and selected the relevant option between 'By project month number' and 'By date'. Check the demo file for reference.")
  #  } else {
      gantt_gg
   # }
  })
  
  output$gantt <- renderGirafe({ #renderPlot
    
    gantt_chart()
  })
  
  output$download_gantt_png <- downloadHandler(filename = "gantt.png",
                                               content = function(con) {
                                                 ggplot2::ggsave(filename = con,
                                                                 plot = gantt_chart(),
                                                                 width = input$download_width,
                                                                 height = input$download_height,
                                                                 units = "cm",
                                                                 type = "cairo",
                                                                 bg = "white")
                                               }
  )
  
  output$download_gantt_pdf <- downloadHandler(filename = "gantt.pdf",
                                               content = function(con) {
                                                 ggplot2::ggsave(filename = con,
                                                                 plot = gantt_chart(),
                                                                 width = input$download_width,
                                                                 height = input$download_height,
                                                                 device = cairo_pdf,
                                                                 bg = "white",
                                                                 units = "cm")
                                               }
  )
  
  
  output$download_gantt_svg <- downloadHandler(filename = "gantt.svg",
                                               content = function(con) {
                                                 ggplot2::ggsave(filename = con,
                                                                 plot = gantt_chart(),
                                                                 width = input$download_width,
                                                                 height = input$download_height,
                                                                 units = "cm",
                                                                 bg = "white")
                                               }
  )
  
  shiny::observeEvent(eventExpr = input$a4_button, {
    shiny::updateNumericInput(inputId = "download_width", value = 29.7, session = session)
    shiny::updateNumericInput(inputId = "download_height", value = 21, session = session)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
