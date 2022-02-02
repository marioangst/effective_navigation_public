normalize_to_range <- function(input, 
                               from, to,
                               maximum = NULL, 
                               minimum = NULL){
  if(is.null(maximum)){
    maximum <- max(input)
  }
  if(is.null(minimum)){
    minimum <- min(input)
  }
  normalized <- ((input - minimum) / (maximum - minimum)) * (to - from) + from
  normalized
}

rename_based_on_codebook <- Vectorize(function(input,codebook,rawvar,codevar){
  #make sure there is only one coded entry in rawvar for input
  z <- codebook[[as.character(rawvar)]] %in% as.character(input)
  numberofentries <- sum(z, na.rm = TRUE)
  if (numberofentries > 1) {
    replacement <- paste("Warning: More than one entry for","",as.character(gsub(input,pattern = ",",replacement = "")),"","in codebook")
  }
  if (numberofentries == 0) {
    replacement <- paste("No entry for", as.character(input), "in codebook")
  }
  if (numberofentries == 1) {
    replacement <- as.character(codebook[[as.character(codevar)]][codebook[[as.character(rawvar)]] %in% as.character(input)])
  }
  return(replacement)
},vectorize.args = c("input"))

