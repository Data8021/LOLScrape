toSeconds <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)
  
  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3) 
               i[1]*3600 + i[2]*60 + i[3]
             else if (length(i) == 2) 
               i[1]*60 + i[2]
             else if (length(i) == 1) 
               i[1]
           }  
    )  
  )  
} 

fromSeconds <- function(x){
  if (!is.numeric(x)) stop("x must be numeric")
  if (length(x)<=0)return(x)
  
  unlist(
    lapply(x,
           function(i){
             if (i >= 3600) {
               y <- seconds_to_period(i)
               sprintf('%02d:%02d:%02d', y@hour, minute(y), second(y))
             } else {
               y <- seconds_to_period(i)
               sprintf('%02d:%02d', minute(y), second(y))
             }
           }  
    )  
  )  
} 