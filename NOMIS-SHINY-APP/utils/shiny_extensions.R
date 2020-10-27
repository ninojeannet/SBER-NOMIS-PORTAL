## Functions that extend shiny default functions ##################################


## UI function modifiers ##########################################################

navbarPageWithWrapper <- function(navbarPageOutput, wrapperClass = 'content-wrapper', footer = NULL, beforeFooterClass = 'before-footer') {
# Create a shiny navbarPage with a content wrapper class for the navbar and content
# Parameters:
# - navbarPageOutput: Output of shiny navbarPage() function, mendatory
# - wrapperClass: a class name for your wrapper, default 'content-wrapper'
# - footer: an optional custom footer, default NULL
# - beforeFooterClass: a class name for the content before the optional footer, default 'before-footer'
#
# Returns an updated shiny navbarPage UI element
  
  # Add wrapperClass to navabar
  navbarPageOutput[[3]][[1]]$children[[1]]$attribs$class <- str_interp('${navbarPageOutput[[3]][[1]]$children[[1]]$attribs$class} ${wrapperClass}')
  # Add wrapperClass and beforeFooterClass to the content
  navbarPageOutput[[3]][[2]]$attribs$class <- str_interp('${navbarPageOutput[[3]][[2]]$attribs$class} ${wrapperClass} ${beforeFooterClass}')
  
  # If footer is defined, add it after the content
  if(!is.null(footer)) {
    navbarPageOutput[[3]][[length(navbarPageOutput[[3]]) + 1]] <- footer
  }
  
  return(navbarPageOutput)
}



checkboxGroupInputWithClass <- function(checkboxGroupInput, class) {
# Create a shiny checkboxGroupInput with additional custom class names
# Can be used with radioButtons as well
# Parameters:
# - checkboxGroupInput: Output of shiny checkboxGroupInput() function
# - class: String, contains the class names to add, separated by space
#
# Returns an updated shiny checkboxGroupInput UI element
  
  checkboxGroupInput$attribs$class <- str_interp('${checkboxGroupInput$attribs$class} ${class}')
  return(checkboxGroupInput)
}



spinnerPlotOutput <- function(outputId, ...) {
# Create a plotOutput that display a spinner when the plot is computing
# Parameters:
# - outputId: String, the id of the plotOutput
# - ...: all other arguments accepted by the plotOutput function
# 
# Returns a plotOutput with a spinner
  
  # Add spinner to the plotOutput using the fading circle and the primary-color
  addSpinner(plotOutput(outputId = outputId, ...), spin = "fading-circle", color = "#e24727")
}


modalButtonWithClass <- function(label, icon = NULL, class) {
# Create a shiny modalButton with additional custom class names
# Parameters:
# - label: String, the content of the button
# - icon: Shiny icon, optional
# - class: String, contains the class names to add, separated by space
#
# Returns an updated shiny modalButton UI element
  
  # Create modalButton
  button <- modalButton(label = label, icon = icon)
  # Add the additional class
  button$attribs$class <- str_interp('${button$attribs$class} ${class}')
  # Return the updated button
  return(button)
}




## Reusable server logic ##########################################################


renderStatsTablePerSite <- function(session, output, id, data, sites, selectedSites) {
# Create and render a multi tables output that contain stats summary of each selected site
# Parameters:
#  - session: Shiny session, the session where the function is called
#  - output: Shiny output of the current module
#  - id: String, the id of the multi tables output
#  - data: Reactive expression returning a Data.frame of the data to summarise
#  - sites: Data.frame, contains the information of the sites
#  - selectedSites: Reactive expression returning the sites for which to get the stats
# 
# Returns an multi tables UI element to render
  
  # Create an empty tagList
  tablesOutput <- tagList()
  
  # For each selected site
  for (site in selectedSites()) {
    # Generate a table id
    tableId <- str_interp('${id}-${site}')
    # Get the site full name
    site_name <- sites %>% filter(sites_short == site) %>% pull(sites_full)
    
    # Create a stats table UI with a tableOutput and combine it to the existing tablesOutput
    tablesOutput <- tagList(
      tablesOutput,
      tags$div(
        class = 'stats-summary-table',
        h4(site_name),
        tableOutput(session$ns(tableId)
        )
      ))
    
    # Filter data per site
    perSiteData <- data() %>% filter(Site_ID == site)
    
    # Render the stats summary table
    output[[tableId]] <- renderTable(createStatsTable(perSiteData), rownames = TRUE)
  }
  
  # Return the stats tables UI
  return(renderUI(tablesOutput))
}



pointHoverWidgetServer <- function(session, plotId, df, input,
                                   x_label = NULL, y_label = NULL,
                                   override.mapping = NULL, threshold = 5,
                                   secondDf = NULL, secondX = NULL, secondY = NULL) {
# Create an observeEvent that react to either an click or hover input on a plot and send information
# to the client to create an info bubble for the closest point
# Parameters:
#  - session: Shiny session, the session where the function is called
#  - plotId: String, the id of the plot to track
#  - df: Reactive expression returning a Data.frame, the data used to draw the plot
#  - input: reactive expression, returning either a click or a hover input
#  - x_label, y_label: String, either a column name of the df where to get the label
#                      or a new label, if NULL (default) values found in the input()$mapping is used
#  - override.mapping: Named list, containing the new mapping to use for the widget. Default NULL
#                      (i.e. list('x' = 'new_x_mapping', 'y' = 'new_y_mapping'), list('x' = 'new_x_mapping'))
#  - threshold: Int, threshold in pixels to determined the nearest point, default 5.
#  - secondDf: Reactive expression returning a Data.frame, a secondary data frame used to draw additional points
#  - secondX, secondY: String, the column name of the x or y data column of the secondDf
# 
# 
# Returns an observeEvent
  
  # Create an observeEvent that react to input() changes even if it is NULL
  # Is returned by the function
  observeEvent(input(), {
    # Parse the potId to add the current namespace to it
    plotId <- session$ns(plotId)
    # Extract the mapping information from the input()
    mapping <- input()$mapping
    
    # If there is a mapping
    if (length(mapping) > 0) {
      altDf <- FALSE
      
      # Get the nearest point
      # If a second df is specified look for a point in this one
      if (!is.null(secondDf)) {
        point <- nearPoints(secondDf(), input(), maxpoints = 1, threshold = threshold,
                            xvar = secondX, yvar = secondY)
        # If a point is fount set altDf to TRUE
        if (nrow(point) == 1) altDf <- TRUE
      }
      
      # If altDf is FALSE look for a point in the primary df
      if (!altDf) point <- nearPoints(df(), input(), maxpoints = 1, threshold = threshold)
      
      # If there is a point process it and return
      if (nrow(point) == 1) {
        
        # Correct the mapping if overrided
        if (typeof(override.mapping) == 'list') {
          if (!is.null(override.mapping$x)) mapping$x <- override.mapping$x
          if (!is.null(override.mapping$y)) mapping$y <- override.mapping$y
        }
        
        # Correct the mapping for the secondary df
        if (altDf) {
          mapping$x <- secondX
          mapping$y <- secondY
        }
        
        # Extract relevant point information
        pointInfo <- point %>% select(Site_ID, mapping$x, mapping$y)
        
        # Predefine the x and y labels with the mapping info
        x_y_labels = list(
          'x' = mapping$x,
          'y' = mapping$y
        )
        
        # If a label is specified for x or y process it
        if (!is.null(x_label)) {
          # If the label is a name of a column
          if (x_label %in% colnames(point)) {
            # Use the value stored in that column
            x_y_labels$x <- point %>% pull(x_label)
          } else {
            # Otherwise use the label
            x_y_labels$x <- x_label
          }
        }
        
        # The same for y label
        if (!is.null(y_label)) {
          if (y_label %in% colnames(point)) {
            x_y_labels$y <- point %>% pull(y_label)
          } else {
            x_y_labels$y <- y_label
          }
        }
        
        # Create a JSON message to send to the client containing:
        #  - pointInfo: the point information (use unbox() to get an object rather than an array)
        #  - mapping: the mapping information
        #  - coords_img: the coordinates of the input location on the plot
        #  - x_y_labels: the x and y labels
        #  - plotId: the id of the concerned plot
        messageJSON <- toJSON(list(
          'pointInfo' = unbox(pointInfo),
          'mapping' = mapping,
          'coords_img' = input()$coords_img,
          'x_y_labels' = x_y_labels,
          'plotId' = plotId
        ), auto_unbox = TRUE)
        
        # Send the shiny custom message to create a widget
        # Linked to some JavaScript defined in './assets/js/point_hover_widget.js'
        session$sendCustomMessage('addHoverWidget', messageJSON)
        
        # Return to stop the expression execution
        return()
      } # End if dim(point)[1] == 1
    } # End if length(mapping) > 0
    
    
    
    # This part is executed only if length(mapping) ≤ 0 or no nearest point were found
    
    # Create a JSON message to send to the client containing:
    #  - plotId: the id of the concerned plot
    messageJSON <- toJSON(list(
      'plotId' = plotId
    ), auto_unbox = TRUE)
    
    # Send the shiny custom message to remove widgets
    # Linked to some JavaScript defined in './assets/js/point_hover_widget.js'
    session$sendCustomMessage('removeHoverWidget', messageJSON)
    
  }, ignoreNULL = FALSE)
}

withLoginAction <- function(navbarPageOutput, loginUI) {
  # Return a Shiny navbarPage with a login UI on the right of the nav bar
  # Parameters:
  # - navbarPageOutput: Output of shiny navbarPage() function, mandatory
  # - loginUI: UI element to insert, ideally output from the loginUI module function, mandatory
  #
  # Returns an updated shiny navbarPage UI element
  
  # Add the loginUI to the navbar of the navbarPageOutput
  navbarPageOutput[[3]][[1]]$children[[1]]$children[[length(navbarPageOutput[[3]][[1]]$children[[1]]$children) + 1]] <- loginUI
  # Return the updated UI
  return(navbarPageOutput)
}


createInputs <- function(df, pool, table, session = getDefaultReactiveDomain()) {
  # Create inputs based on a df column names, types and values
  # Parameters:
  #  - df: Data.frame, the data used to create the inputs, should either be empty or have only one row
  #  - pool: Pool connection, the connection to an SQL Database
  #  - table: String, the database corresponding table
  #  - session: Shiny session, the session in which to create the inputs, default: getDefaultReactiveDomain()
  # 
  # Returns a tagList of inputs
  
  # Create the inputs tagList
  inputs <- tagList()
  # Get the df column types
  columnTypes <- df %>% lapply(type_sum) %>% unlist()
  
  # For each column
  for (i in c(1:length(columnTypes))) {
    # Get the type and the column name
    type <- columnTypes[i]
    column <- names(type)
    
    # If the df is empty, set the value to NULL
    # Otherwise, set it to the column value unless it is an NA, then set it to NULL
    if (nrow(df) == 0) {
      value <- NULL
    } else {
      value <- df %>% pull(column)
      if (is.na(value)) value <- NULL
    }
    
    # Create the input and add it to the list
    inputs <- tagList(
      inputs,
      createInput(
        type = type,
        label = column,
        value = value,
        pool = pool,
        table = table,
        session = session
      )
    )
  }
  
  # Return the inputs list
  return(inputs)
}




createInput <- function(type, label, value = NULL, table, pool, session = getDefaultReactiveDomain()) {
  # Create an input based on a df column name, type and value
  # Parameters:
  #  - type: String, the columns type used to choose the input
  #  - label: String the column name used to create the input label and id
  #  - value: 'type' dependent, the value of the column used to populate the input
  #  - table: String, the database corresponding table
  #  - pool: Pool connection, the connection to an SQL Database
  #  - session: Shiny session, the session in which to create the inputs, default: getDefaultReactiveDomain()
  # 
  # Returns the adequate input
  
  # Choose the input in function of the column type
  if (type == 'dbl' | type == 'int') {
    numericInput(session$ns(label), label = label, value = value)
  } else if (type == 'chr') {
    if (label == 'description') {
      textAreaInput(session$ns(label), label = label, value = value)
    } else {
      textInput(session$ns(label), label = label, value = value)
    }
  } else if (type == 'lgl') {
    checkboxInput(session$ns(label), label = label, value = value)
  } else if (type == 'fct') {
    # For a select input get the possible values from the SQL database
    selectInput(session$ns(label), label = label, choices = getEnumValues(pool, table, label), selected = value)
  } else if (type == 'dttm') {
    airDatepickerInput(
      inputId = session$ns(label),
      label = paste(label, '(GMT)'),
      value = value,
      timepicker = TRUE,
      timepickerOpts = timepickerOptions(timeFormat = 'hh:ii')
    )
  }
}


confirmationModal <- function(text = '', session = getDefaultReactiveDomain()) {
  # Create and display a modal for action confirmation purpose
  # Confirmation need to be done with an 'observeEvent' listening for an 'input$YES' button
  # And modal need to be closed within this observer with 'removeModal()'
  # Parameters:
  #  - text: String, the optional additional text to display
  #  - session: Shiny session, the session in which to create the inputs, default: getDefaultReactiveDomain()
  # 
  # Returns NULL
  
  # Create and display the modal
  showModal(modalDialog(
    size = 's',
    div(
      class = 'confirmation-modal',
      h1('Are you sure ?'),
      p(text),
      div(
        class = 'confirmation-buttons',
        actionButton(session$ns('YES'), 'YES', class = 'btn-success'),
        modalButtonWithClass('NO', class = 'btn-danger')
      )
    ),
    footer = NULL
  ), session = session)
}

