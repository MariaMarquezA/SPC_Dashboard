readFromDB <- function(query = '', display = FALSE){
  library(httr)
  url <- 'Insert the API URL here!'
  body <- charToRaw(gsub(' ', '%', query))
  response <- POST(url)
  if(display){
    print(response)
  }
  csv <- content(response, 'text')
  df <- read.csv(text = csv)
  return(df)
}