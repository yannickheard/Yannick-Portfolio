library(xts)

# The following function reads a weekly demand file and 
# outputs a time-series with monthly figures
# it assumes the first column is the IRI week and 
# it converts the figures in the last column of the file
# You can call it as:
#
#  w2m("input-file-name", "label")
# where the input file is in a .csv format, and 
# the "label" is the name you want to give to the variable
# the produced time-series


w2m <- function(file.name, col.name){
  # Read the .csv data file
  x <- read.csv(paste0(file.name,".csv"))
  # Create an index to translate IRI-Weeks into calendar dates
  CW <- seq(as.POSIXct("1979-9-3", "GMT"), as.POSIXct("2011-12-19", "GMT"), by = "week")
  # create a calendar index with the IRI weeks in the first column of the file
  cal.ix <- CW[x[,1]]
  # Create dem as an xts objext indexed by "cal.ix"
  demand <- xts(x[,ncol(x)], order.by=cal.ix)
  # Create a time index with a daily increment and no-data i.e., NA
  st.day <- first(cal.ix)
  end.day <- as.POSIXct(as.Date(last(cal.ix))+7)
  demd <- xts(,seq(st.day,end.day,"day"))
  # Merge both time series to create a series with valies and some NA
  demd <- merge(demand,demd)
  # Replace NA with prior available number and divide by 7
  demd <- na.locf(demd)/7
  # Accumulate demand by month
  mdem <- apply.monthly(demd,sum)
  mdem <- ts(mdem, start=2001, frequency=12)
  colnames(mdem) <- col.name
return(mdem)  
}



d <- w2m("Input.File","Output.File")
plot(d)
