# plot3() - This function creates the third plot, 
# a representation of the different sub-meterings over time
plot3 <- function(){

  # We download and load the required data for the plotting
  data <- loadAndSubsetData()
 
  # Generate first plot as png file
  png(filename="plot3.png")
  plot(type="n", x=data$Timestamp, y=data$Sub_metering_1, ylab="Energy sub metering", xlab="")
  with(data, lines(Timestamp,Sub_metering_1,col="black"))
  with(data, lines(Timestamp,Sub_metering_2,col="red"))
  with(data, lines(Timestamp,Sub_metering_3,col="blue"))
  legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty=c(1,1,1), col=c("black", "red","blue"))
         
  dev.off()
}

# loadAndSubsetData() - This function downloads the data file for the assignment,
# loads it into memory and subsets the piece we need for the assignment, returning that 
# data frame. It also adds a new field, Timestamp, with the date/time of each measure in R format
loadAndSubsetData <- function(){
  
  zipfilename <- "exdata-data-household_power_consumption.zip"
  datafilename <- "household_power_consumption.txt"
  
  # Download and unzip the dataset, if it is not in the working directory
  if(!file.exists(datafilename)){
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                  dest="exdata-data-household_power_consumption.zip",
                  method="curl")
    unzip(zipfilename)
  }
  
  # Calculate and output memory needs, approximate using the linux command wc
  lines <- 0
  columns <- 0
  countLines <- system(paste("wc -l",datafilename), intern=T)
  lines <- as.numeric(strsplit(countLines," ")[[1]][[1]])
  if(!is.na(lines)){ # if the command was success, lines contains the number of lines
    # We read the first line and count the columns
    head <- readLines(con <- file(datafilename),n=1)
    close(con)
    
    # We get the number of columns, splitting by the separator (;)
    columns <- length(strsplit(head,";")[[1]])
  }else{ # probably the command failed, maybe we are not in a linux machine
    # We use the value for the lines provided by the assignment statement
    lines <- 2075259+1 # We add the header line also
    columns <- 9
  }
  dummy <- readline(paste("The rough estimation for memory consumption is (MB) ",
                          as.character(lines*columns*8/(1024*1024)),
                          ". Push [Enter] to continue if that's OK", 
                          sep=""))
  
  # Load and subset the dataset, and print actual memory usage of the dataset
  bigdata <- read.csv2(datafilename,na.strings=c("?",""," "),stringsAsFactors=F)
  bigdata$Timestamp <- paste(bigdata$Date,bigdata$Time)
  bigdata$Date <- as.Date(bigdata$Date, format="%d/%m/%Y")
  data <- bigdata[bigdata$Date >= as.Date("01/02/2007", format="%d/%m/%Y") & bigdata$Date <= as.Date("02/02/2007", format="%d/%m/%Y"),]
  rm(bigdata) # We remove the big dataset from memory
  dummy <- readline(paste("Data read and subset. Actual memory usage (MB) ",
                          as.numeric(object.size(data))/(1024*1024),
                          ". Push [Enter] to continue if that's OK", 
                          sep=""))
  
  data$Timestamp <- strptime(data$Timestamp, "%d/%m/%Y %H:%M:%S")
  
  # We return the dataset
  data
  
}