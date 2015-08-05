processData <- function(infile) {
        
        ## Read in tabular data, semi-colon delimited
        df <- read.table(infile, header=T, sep=";", quote="", na.strings="?")
        df$Date <- as.Date(df$Date, format="%d/%m/%Y")
        
        ## Keep only the desired dates
        df <- df["2007-02-01" <= df$Date & df$Date <= "2007-02-03", ]
        
        ## Merge the date and time columns for POSIX conversion
        df <- within(df, DateTime <- paste(df$Date, df$Time, sep=" "))
        
        ## Remove unwanted columns
        df <- df[,c(-1,-2)]
        
        ## Convert DateTime column to POSIX
        df$DateTime <- strptime(df$DateTime, format="%Y-%m-%d %H:%M:%S")
        
        
        ## Return the analytical data frame
        return(df[ ,c(length(names(df)), 1:length(df)-1)])
}


## Plot1.png code
plot1 <- function(df) {
        
        hist(
                df$Global_active_power,
                main="Global Active Power", 
                xlab="Global Active Power (kilowatts)",
                ylab="Frequency",
                axes=T,
                col="red"
        )
        
}

## png(filename="plot1.png", width=480, height=480)

## plot1(processData(infile))

## dev.off()