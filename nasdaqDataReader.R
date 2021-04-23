#'read data gotten from csv-Files on nasdaq.com , e.g. for Tesla: https://www.nasdaq.com/market-activity/stocks/tsla/historical
nasdaqDataReader <- function(filePath) {
  
  nasdaqData <- read.table(file = filePath, header = TRUE, as.is = TRUE, sep = ",")
  
  #get nice formatted dates, Nasdaq saves them in "month/day/Year" - format
  niceDate <- as.Date(nasdaqData$Date, format = "%m/%d/%Y")
  
  #convert prices
  lowPrices <- convertNasdaqPrice(nasdaqData$Low)
  highPrices <- convertNasdaqPrice(nasdaqData$High)
  
  #sort everything by dates
  timeOrder <- order(niceDate, decreasing = FALSE)
  niceDate <- niceDate[timeOrder]
  lowPrices <- lowPrices[timeOrder]
  highPrices <- highPrices[timeOrder]
  
  #create nicely formatted dataframe
  cleanData <- data.frame(
    day = niceDate,
    price = (lowPrices + highPrices)/2 #price as mean of daily low and high
  )
  
  #remove Prices of 0, data seems to have a lack there
  nonZeroPriceIndices <- cleanData$price != 0
  cleanData <- cleanData[nonZeroPriceIndices,]
  
  cleanData$Return <- c(0,cleanData$price[-1]/cleanData$price[-NROW(cleanData)] -1)
  cleanData$logReturn <- c(0,log(cleanData$price[-1]) - log(cleanData$price[-NROW(cleanData)]))
  
  cleanData
}

#Convert values from price columns gotten from a Nasdaq-csv-export
#In case of an index (like SPX), its just a number and is read as numeric, so nothing to do
#In case of stock (like Tesla), it is a character with dollar-sign, which needs to be removed and converted to numeric
convertNasdaqPrice <- function(x) {
  if(is.numeric(x)) return(x)
  if(is.character(x)) return(as.numeric(gsub("\\$","",x)))
  stop("Unsupported type.")
}