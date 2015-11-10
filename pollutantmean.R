pollutantmean <- function(directory, pollutant, id = 1:332) {
    filenames <- sprintf("%03d.csv", id)
    filenames <- paste(directory, filenames, sep="/")
    ldf <- lapply(filenames, read.csv)
    ldf <- do.call(rbind,ldf)
    # df is your list of data.frames
    mean(ldf[, pollutant], na.rm = TRUE)
}