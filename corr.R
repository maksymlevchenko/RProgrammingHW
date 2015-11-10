corr <- function(directory, threshold = 1) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!total<-numeric()
    
    directory <- ("/Users/Maksym/Desktop/DS Coursera/specdata")
    
    files <- list.files( path = directory )
    files <- files[grepl('.csv', files, fixed=TRUE)]
    
    cr <- c()
    
    for(f in 1:length(files)){
        data <- read.csv( paste(directory, "/", files[f], sep="") )
        #data <- data[complete.cases(data),]
        if ( nrow(data) > threshold ) {
            x <- data[['sulfate']]
            y <- data[['nitrate']]
            stopifnot(length(x) > 0)
            stopifnot(length(y) > 0)
            stopifnot(length(x) == length(y))
            cr <- c(cr, cor(x, y, use='pairwise.complete.obs') ) # append corralations
        } else{
            stop(paste('dataset', f, 'has <=', threshold, 'rows'))
        }
    }
    
    return( cr )
}