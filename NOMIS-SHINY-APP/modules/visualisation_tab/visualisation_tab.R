source('./utils/helper_database.R')
source('./utils/helper_dataframe.R')
source('./utils/template_config.R')
source('./utils/dataframe_generator.R')
source('./utils/helper_file.R')
source('./utils/helper_log.R')
source('./modules/data_module/data_validation_popup.R')

visualisationTabUI <- function(id) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  # tabsetPanel(
  #   tabPanel(
  #     'Visualisation',
  #     manageDataTabUI(ns("data"),"Manage data")
  #   )
    # ),
    # tabPanel(
    #   'Manage expeditions',
    #   managementExpeditionTabUI(ns("expedition"))
    # ),
    # tabPanel(
    #   'Process data',
    #   managementProcessTabUI(ns("process"))
    # ),
    # tabPanel(
    #   'Project progression',
    #   managementProgressTabUI(ns("progress"))
    # )
  # )
  
  div(
    div(
      class= 'main-inputs',
      h1("Visualisation"),
      div(
        class = 'main-actions',
        actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary')
      )),
    # Create the sidebarLayout
    sidebarLayout(
      sidebarPanel(
        id = ns('sidebar'),
        div(
          selectInput(ns("type"),label = "Select a data type",choices = uploadDataTypes),
          radioButtons(ns("selectRange"), "Choose a selection option :",
                       c("Unique glacier" = "simple",
                         "Range of glacier" = "range",
                         "List of glacier" = "list")),
          textInput(ns("glacier"),"Enter glacier ID"),
          hidden(numericRangeInput(ns("glacierRange"),label = "Glacier range", value = c(1, 500))),
          hidden(textInput(ns("glacierList"),"Glacier list (comma separated)")),
          actionButton(ns("generate"),"Generate plot"),
        ),
        width = 3
      ),
      # Create the main panel
      mainPanel(
        id = ns('main'),
        div(id="main-content",
            plotOutput(ns("plot"))       
        ),
        width = 9
      )
    )
  )
  # div(
  #   plotOutput(ns("plot")),
  #   plotOutput(ns("plot1")),
  #   actionButton(ns("btn"),"btn")
  # )
}

visualisationTab <- function(input, output, session,pool){
  # callModule(manageDataTab,"data",pool,dimension,FALSE)
  # callModule(managementProcessTab,"process",pool)
  # callModule(managementProgressTab,"progress",pool)
  # callModule(managementExpeditionTab,"expedition",pool)
  
  # Reactive variable which contains the list of all chosen glacier's id
  ids <- reactive({
    switch (input$selectRange,
            "simple" = {
              ids <- input$glacier
            },
            "range" = {
              range <- input$glacierRange
              ids <- c(paste0("GL",as.character(range[1]:range[2])))
            },
            "list" = {
              ids <- input$glacierList
              ids <- gsub(" ", "", ids, fixed = TRUE)
              ids <- strsplit(ids,',')
              ids <- sapply(ids, function(x){paste0("GL",x)})
            })
  })
  
  # observeEvent that react to selectrange input's update
  # Enable / disable html component 
  observeEvent(input$selectRange,{
    switch (input$selectRange,
            "simple" = {
              showElement("glacier")
              hideElement("glacierRange")
              hideElement("glacierList")
            },
            "range" = {
              hideElement("glacier")
              showElement("glacierRange")
              hideElement("glacierList")
            },
            "list" = {
              hideElement("glacier")
              hideElement("glacierRange")
              showElement("glacierList")
            }
    )
  })
  
  data <- reactive({
    getFieldsFromGlacier(pool,tableName = "microbial_3",fields =c("chla") ,ids = c("GL1","GL2","GL3","GL4","GL5","GL6","GL7"))
  })
  
  observeEvent(input$generate,{
    print(data())
    output$plot <- renderPlot({
    #   ggplot(data(), aes(x = "galcier", y = 1:nrow(data()), fill = "red")) +
    #     geom_boxplot(alpha=0.3) + 
    #     geom_jitter(aes(colour="red",shape="red"),position=position_jitterdodge(0.5),cex=2)+
    #     scale_y_continuous(name = "TER") + 
    #     scale_fill_brewer(palette = "Accent") + scale_colour_brewer(palette = "Accent") +
    #     #ggtitle("") +
    #     theme_bw() +
    #     theme(axis.title.x=element_blank(),
    #           plot.title=element_text(size = 20),
    #           text=element_text(size = 16),
    #           axis.text.x=element_text(colour="black", size = 12,angle = 0, hjust = 0.5),
    #           axis.text.y=element_text(colour="black", size = 12)) 
    #   
    # })
    
    # output$plot1 <- renderPlot({
      df <- data.frame(ba=c(1,2,4,5,6,7,8),chla=c(12,42,5,43,34,34,3),Glacier=c("GL1","GL2","GL3","GL4","GL5","GL6","GL7"))
      ggplot(df,aes(x=ba,y=chla,color="red"))+
        geom_point()+
        facet_wrap(~Glacier,ncol = 2)+
        scale_color_manual(values=c('orangered1', 'purple3'), labels=c("Tributary","Glacier-fed"))+
        guides(color=guide_legend(""))+
        ylim(-1,3)+
        scale_x_continuous(trans = 'log10')+
        scale_y_continuous(trans = 'log10')+
        theme(legend.position="bottom", legend.box = "horizontal",axis.title.x = element_text(margin=margin(t=15),size=14),axis.title.y =element_text(margin=margin(t=20),size=14))

     })
  })
  
  
}
