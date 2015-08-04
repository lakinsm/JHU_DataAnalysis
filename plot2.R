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
        
        ## Convert DateTime column to POSIX and keep inclusive times
        df$DateTime <- strptime(df$DateTime, format="%Y-%m-%d %H:%M:%S")
        df <- df[df$DateTime <= "2007-02-03 00:00:00", ]
        
        
        ## Return the analytical data frame
        return(df[ ,c(length(names(df)), 1:length(df)-1)])
}


## Plot2.png code
plot2 <- function(df) {
        
        png(filename="plot2.png", width=480, height=480)
        
        plot(
                x=df$DateTime,
                y=df$Global_active_power,
                type="l",
                ylab="Global Active Power (kilowatts)",
                xlab="",
                axes=F
        )
        
        box()
        
        axis(
                side=1, 
                at=as.POSIXct(
                        c(
                                min(df$DateTime),
                                min(df$DateTime[df$DateTime > "2007-02-01 23:59:59"]), 
                                min(df$DateTime[df$DateTime > "2007-02-02 23:59:59"])
                        )
                ),
                labels=unique(weekdays(df$DateTime, abbreviate=T))
        )
        
        axis(side=2, at=c(0,2,4,6))
        
        dev.off()
        
}

## plot2(processData(infile))