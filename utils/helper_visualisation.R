

VSPlot <- function(df,valueToGroupBy,valueForColors, x ,y){
  plot <- ggplot(df,aes(x=!!sym(x),y=!!sym(y),color=!!sym(valueForColors)))+
    geom_point()+
    scale_color_manual(values=c('orangered1', 'purple3'), labels=c("UP","DN"))+
    guides(color=guide_legend(""))+
    ylim(-1,3)+
    scale_x_continuous(trans = 'log10')+
    scale_y_continuous(trans = 'log10')+
    theme(legend.position="bottom", legend.box = "horizontal",axis.title.x = element_text(margin=margin(t=15),size=14),axis.title.y =element_text(margin=margin(t=20),size=14))
  return(plot)
}

histPlot <- function(df,param,valueForColors){
  plot <- ggplot(df, aes(x=!!sym(param), fill = !!sym(valueForColors))) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#4db8ff", "#2f4e72"))
  return(plot)
}


generateMergedDF <- function(fields,ids,pool){
  tables <- unlist(lapply(fields,getTableNameFromValue))
  nbEntries <- max(levels[tables])
  nbRow <- nbEntries*length(ids)
  
  df <- data.frame(matrix(ncol = 0, nrow = nbRow))
  if(nbEntries == 6){
    lis <- getFieldsFromGlacier(pool,tableName = "patch" ,fields = c("id_patch"),ids = ids)
    df[["patch"]] <- reg_sort(lis[[1]],"^GL\\d+",-"UP|DN","_\\d")
    df[["glacier"]] <-  gsub("_.+","",df[["patch"]])
    df[["site"]] <- gsub("[^(UP|DN)]","",df[["patch"]])
  }
  else if(nbEntries == 2){
    lis <- getFieldsFromGlacier(pool,tableName = "location" ,fields = c("id_location"),ids = ids)
    df[["location"]] <- reg_sort(lis[[1]],"^GL\\d+",-"UP|DN")
    df[["glacier"]] <-  gsub("_.+","",df[["location"]])
    df[["site"]] <- gsub("[^(UP|DN)]","",df[["location"]])
  }
  else{
    lis <- unlist(ids)
    df[["glacier"]] <-  reg_sort(lis,"^GL\\d+")
  }
  
  for (field in fields) {
    table <- getTableNameFromValue(field)
    fieldsToRetrieve <- setdiff(getFieldsFromValue(field),mandatoryFields[[table]])
    nbReplicates <- length(tableOptions[[table]][["replicates"]])
    
    values <-getFieldsWithFKFromGlacier(pool,tablename = table ,fields = fieldsToRetrieve,ids = ids)
    fk <- tableOptions[[table]][["FK"]]
    if(table != "glacier")
      colToSummarise <-names(values %>% select(-all_of(fk)))
    
    if(nbReplicates > 1)
      values <- reduce(values,table,fk,colToSummarise)
    else
      values <- formatDFforDownload(values)
    
    if(levels[table] == 6){
      values <- values %>% arrange(as.numeric(gsub("\\D+", "\\1", gsub("_.+","\\1",values[[1]]))),
                                   desc(gsub("[^(UP|DN)]", "\\1", values[[1]])), gsub("^.+_", "\\1", values[[1]]))
    }
    else if(levels[table] == 2){
      for (i in 1:length(values[[1]])) {
        if(i %% 2 == 1)
          values[i,1] <- paste0(values[i,1],"_DN")
        else
          values[i,1] <- paste0(values[i,1],"_UP")
      }
      values <- values %>% arrange(as.numeric(gsub("\\D+", "\\1", gsub("_.+","\\1",values[[1]]))),
                                   desc(gsub("[^(UP|DN)]", "\\1", values[[1]])))
      values[[1]] <- unlist(lapply(values[[1]],function(x){str_remove(x,"_UP|_DN")}))
    }
    else{
      values <- values %>% arrange(as.numeric(gsub("\\D+", "\\1", gsub("_.+","\\1",values[[1]]))))
    }
    
    if(isUpOnly(table))
      values <- insertEmptyDownEntries(values,fk,colToSummarise)
    
    if(table != "glacier")
      values <- removeFK(values,table)
    column <- scale(values,table,nbEntries)
    if(is.null(ncol(column))){
      name <- convertToDLName(field)
      df[[name]] <- unlist(column)
      
    }
    else{
      for(i in 1:ncol(column)){
        name <- convertToDLName(names(column[i]))
        df[[name]] <- unlist(column[i])
      }
    }
  }
  
  return(df)
}

