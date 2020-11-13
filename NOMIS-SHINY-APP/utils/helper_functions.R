## Regroup all helpers R functions

# Function that create a data frame of one column with some summary statistics of a given data
# Parameters:
#  - columnData: Vector, data of a given column to get stats from
#  - columnName: String, the name of the data column
# 
# Returns a one column data frame containing the statistics values
getStats <- function(columnData, columnName) {
  # Create an empty data.frame with adequate row names
  newColumn <- data.frame(row.names = c(
    'Time Points',
    'N',
    "NA's",
    'Median',
    'Mean',
    'SD',
    'Min.',
    'Max.'
  ))
  
  # Calculate all the stats
  newColumn['Time Points', columnName] <- length(columnData)
  newColumn['N', columnName] <- length(columnData) - sum(is.na(columnData))
  newColumn["NA's", columnName] <- sum(is.na(columnData))
  newColumn['Median', columnName] <- median(columnData, na.rm = TRUE)
  newColumn['Mean', columnName] <- mean(columnData, na.rm = TRUE)
  newColumn['SD', columnName] <- sd(columnData, na.rm = TRUE)
  newColumn['Min.', columnName] <- if (min(columnData, na.rm = TRUE) == Inf) NA else min(columnData, na.rm = TRUE)
  newColumn['Max.', columnName] <- if (max(columnData, na.rm = TRUE) == -Inf) NA else max(columnData, na.rm = TRUE)
  
  # Return the data.frame
  return(newColumn)
}


# Function that create a data frame of multiple column with some summary statistics of a given data
# Parameters:
#  - df: Data.frame, data to get stats from, must have the following columns:
#        + parameter: Factors, the parameter of the data point
#        + value: Numeric, the data values to summarise
# 
# Returns a data frame containing the statistics values for each parameter as a column
createStatsTable <- function(df) {

  # Create a vector with the parameter data column name references
  columns <- df$parameter %>% unique()
  
  # Create empty data frame
  statsTable <- data.frame()
  
  # For each value in columns
  # Filter the data by parameter and get stats from the values
  for (dataColumn in columns) {
    # If statsTable is an empty data frame
    if (statsTable %>% dim() %>% sum() == 0) {
      # Assign the output to the statsTable
      statsTable <- df %>% filter(parameter == dataColumn) %>% pull(value) %>% getStats(dataColumn)
    } else {
      # Else combine both data frame
      newCol <- df %>% filter(parameter == dataColumn) %>% pull(value) %>% getStats(dataColumn)
      statsTable <- cbind(statsTable, newCol)
    }
  }
  
  ## Return stats summary table
  return(statsTable)
}


# Function to parse options for select input with section
# Parameters:
#  - optionsInfo: Data.frame, containing the info to create the select input options. Columns format:
#                 + sectionColumn: Containing the name of the section.
#                 + optionColumn: Containing the name of the option.
#                 + valueColumn: A column with the same name as specified in valueColumn.
#                                Containing the value of the option.
#  - valueColumn: String, name of the column containing the options value
#  - sectionColumn: String, name of the column containing the section names, default 'section_name'
#  - optionColumn: String, name of the column containing the options names, default 'option_name'
# 
# Returns a named list of named lists to be used as choices parameter for shiny selectInput()
parseOptionsWithSections <- function(optionsInfo, valueColumn, sectionColumn = 'section_name', optionColumn = 'option_name') {

  # Create an empty list
  optionsList <- list()
  
  # For each row in the optionsInfo
  for (i in c(1:dim(optionsInfo)[1])) {
    # Extract current row, section, option and value
    currentRow <- optionsInfo[i,]
    currentSection <- currentRow %>% pull(sectionColumn)
    currentOption <- currentRow %>% pull(optionColumn)
    currentValue <- currentRow %>% pull(valueColumn)
    
    # Add a list to optionsList if the corresponding section name list is not already created
    if (optionsList[[currentSection]] %>% is.null()) {
      optionsList[[currentSection]] <- list()
    }
    
    # Add the option to the corresponding section name list
    optionsList[[currentSection]][[currentOption]] <- currentValue
  }
  
  return(optionsList)
}


# Function that create a simple options list for select input
# Parameters:
#  - optionsInfo: Data.frame, containing the info to create the select input options. Columns format:
#                 + optionsColumn: A column with the same name as specified in optionsColumn
#                                Containing the name and value (value == name) of the option.
#  - optionsColumn: String, name of the column containing the options value (and name)
# 
# Returns a named list to be used as choices parameter for shiny selectInput()
parseOptions <- function(optionsInfo, optionsColumn) {

  return(
    optionsInfo[[optionsColumn]] %>% unique()
  )
}



# Function that compute the linear regression equation and r2 of a given dataset and variables
# Parameters:
#  - df: Data.frame, the data to perform the linear regression with
#  - x: String, the column containing the x values
#  - y: String, the column containing the y values
# 
# Returns a sgtring containg the formated linear regression equation and r2
lm_eqn <- function(df, x, y){
  
  # If either x or y contains only NAs return an empty string
  if (all(is.na(pull(df, all_of(x)))) | all(is.na(pull(df, all_of(y))))) return('')
  
  # Compute the linear regression
  m <- lm(as.formula(str_interp('${y} ~ ${x}')), df)
  # Create the equation and r2 expression
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  # Convert it to string and return it
  as.character(as.expression(eq))
}


# Parse and combine all JavaScript files present referenced in the assets/js/manifest.json file
# Minify it and saves it in the www/ folder as metalpdataportal.js
# Parameters:
#  - inputDir: String, the path to the input directory, default: 'assets/js'
#  - outputDir: String, the path to the output directory, default: 'www'
#  - wd: String, the the full path to the working directory, default: getwd()
# 
# Returns NULL
js_parser <- function(inputDir = 'assets/js', outputDir = 'www', wd = getwd()) {
  
  # Create full path using the wd
  inputDir <- file.path(wd, inputDir)
  outputDir <- file.path(wd, outputDir)
  
  # Load the manifest file
  manifest <- read_json(file.path(inputDir, 'manifest.json'), simplifyVector = TRUE)
  
  # Create an empty variable for the compiled JS
  compiled <- ''
  
  # For each file in in the manifest
  # Read the file and append the content to the compiled variable
  for (filename in manifest) {
    compiled <- paste0(compiled, read_file(file.path(inputDir, filename)))  
  }
  
  # Create a temporary file with the compiled JS
  tmp <- tempfile()
  write_file(compiled, tmp)
  
  # Create the full path to the output file
  outputFile = file.path(outputDir, 'nomisportal.js')
  
  # Minify the compiled JS and saves it in the output file
  processx::run('terser.cmd',echo = TRUE,args = c(tmp, '-c', '-o', outputFile))
  
  # Delete the temporary file
  file.remove(tmp)
}


# Check if string is valid email format
# Parameters:
#  - x: String, email to verify
# 
# Return a boolean value
isValidEmail <- function(x) {
  
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

