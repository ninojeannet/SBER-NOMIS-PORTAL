

copyDFValuesTo <- function(dfToCopy,newdf,primary,shift){
  # Insert existing values into the new data frame
  ids <- newdf[[primary]]
  for (i in 1:nrow(dfToCopy)) {
    row <-dfToCopy[i,]
    id <- row[[primary]]
    if(id %in% ids)
    {
      se <- newdf[[primary]]
      rownumber <- which(se == id,arr.ind = TRUE)
      newdf[rownumber,] <- row
    }
  }
  return(newdf)
}

addRows <- function(df,start,stop,nbCol){
  for (i in (start+1):stop) {
    row <- c(rep(NA, nbCol))
    df[i,] <- row
    # df <- rbind(df,row)
    # df <- rbindlist(list(df,as.list(row)))
    # df %>% add_row(tribble_row())
  }
  return(df)
}