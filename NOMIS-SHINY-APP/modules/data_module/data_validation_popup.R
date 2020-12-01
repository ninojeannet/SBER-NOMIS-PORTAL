
# Validation popup function
# Create  a modal to validate the update in the database
# Parameters : 
# - button_id : the id of the submit button
# - text_id : the id of the textouput which displays updated rows
validation_popup <- function(button_id,text_id){
  
  showModal(
    modalDialog(
      div(id=("validate"),
          tags$head(tags$style(".modal-dialog{ width:600px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), 
          fluidPage(
            fluidRow(
              h2("Validate modifications"),
              div(class="text_out",textOutput(text_id,inline = TRUE)),
              actionButton(button_id, "Submit")
            ),
            easyClose = TRUE
          )
        )
      )
    )
}

# Show validation popup
# Parameters : 
# - tablename : name of the selected table
# - updatedValues : list of updated values from the data table
# - output : output variable to access shiny elements
# - ns : current namespace
# - isFileUpload :  boolean indicating if it's file or data upload
# - out : not mandatory. The data table to retrieve the updated  values from
show_validation_popup <- function(tablename,updatedValues,output,ns,isFileUpload,out=NULL){
  if (isFileUpload){
    l <- updatedValues
  }
  else{
    l <- out[updatedValues,1]
    l <- sort(l[!duplicated(l)])
  }
  validation_popup(ns("submit"),ns("updated_values"))
  size <-length(l)
  if(size > 20){
    l <- l[1:20]
    l <- c(l,paste0("\n+ ",size-20," updated rows"))
  }
  output$updated_values <- renderText({l},sep = "\t")
}
