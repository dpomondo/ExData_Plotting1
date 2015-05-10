## plot1.R
## home: github.com/dpomondo/... {to be filled in  later}
##
plot1 <- function(verbose=FALSE) {
    ### Function creates a plot named 'plot1.png' in the current dir.
    ###
    ### Use: Called with 'verbose=T' to get detailed (?!) feedback.
    ### Otherwise, the function will:
    ###     1. determine if './data' exists, creating it if necessary
    ###     2. determine if the unzipped csv file is present
    ###         a. download the file if not present
    ###         b. unzip after downloading
    ###     3. determine if the 'household_power' data frame is already in memory
    ###         a. import 'sqldf' -- note that this function WILL fail if 'sqldf' isn't installed
    ###         b. read only those rows with '1/2'2007' or '2/2/2007' in the 'Date' col
    ###         c. convert the 'Date' and 'Time' cols into a single 'POSIXct' class
    ###         d. replace 'Date' and 'Time' cols withthe 'date_time' col
    ###         e. store the cleaned 'household_power' data fram in the global
    ###            environment, to speed up further calls, or calls of the 'plot[234].R' funcs
    ###     4. make the appropriate plot and save it as a '.png' file
    ### Note that the cleaned data frame is NOT saved/archived; later calls
    ### will require reimporting and recleaning.
    ###
    ### Also, I'm tired and hungry, so I'm not going to yank/put these
    ### comments into the 'plot[234].R' files. 
    targetfile <- './data/household_power_consumption.txt'
    ## First, we get the file in the right place!
    if (!file.exists('data')) {
        if (verbose==TRUE) { print("creating data directory") }
        dir.create('data')
    }
    if (!file.exists(targetfile)) {
        if (verbose==TRUE) { print("Downloading dataset...") }
        fileUrl <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
        ## download and unzip
        dest <- './data/dataset.zip'
        download.file(fileUrl, destfile=dest, method='curl')
        if (verbose==TRUE) { print("Unzipping dataset...") }
        unzip(dest, exdir='./data/')
    }
    ## Now that we know we have the file where we want it, we read it into memory
    ## First, make sure it's not already IN memeory:
    if (!exists('household_power') || !is.data.frame(household_power)) {
        ## use a SQL query to pull out the specific rows we want
        if (verbose==TRUE) {print("Loading sqldf library...")}
        library('sqldf')
        if (verbose==TRUE) {print("Loaded.")}
        ## load 'household_power' into a global variable
        household_power <<- read.csv2.sql(targetfile, 
                            sql = 'select * from file where Date="1/2/2007" or Date="2/2/2007" ', 
                            colClasses="character") }
    else if (verbose==TRUE) {print("returning cached 'household_power' dataset")
    }
    ## test to see if there's a date_time column
    if (is.null(household_power$date_time)) {
        household_power$date_time <- as.POSIXct(paste(household_power[, 'Date'], 
                                                      household_power[, 'Time']),
                                                format='%d/%m/%Y %H:%M:%S')
        # reorder the columns, just because
        household_power <<- cbind(household_power[10], 
                                  household_power[3:9])
    }
    ## And finally make the plot!
    png(file='rplot1.png')
    plot(hist(as.numeric(household_power$Global_active_power), 
        breaks=12), col='red', main='Global Active Power', 
        xlab='Global Active Power (kilowatts)', 
        ylab = 'Frequency', mar=c(4, 3, 2, 2))
    dev.off()
}
