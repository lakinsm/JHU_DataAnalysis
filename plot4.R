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

plot3 <- function(df) {
        
        plot(
                x=df$DateTime,
                y=df$Sub_metering_1,
                type="l",
                ylab="Energy sub metering",
                xlab="",
                axes=F
        )
        
        lines(
                x=df$DateTime,
                y=df$Sub_metering_2,
                col="red"
        )
        
        lines(
                x=df$DateTime,
                y=df$Sub_metering_3,
                col="blue"
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
        
        axis(side=2, at=c(0,10,20,30))
        
        legend(
                x="topright",
                lty=1,
                col=c("black", "red", "blue"),
                legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                bty="n"
        )
        
}


## Plot4.png code
plot4 <- function(df) {
        
        source('~/Coursera/ExpAnalysis/plot2.R')
        
        par(mfrow=c(2,2))
        
        if(!is.data.frame(df)) {
                df <- processData(infile)
        }
        
        plot2(df)
        
        plot(
                x=df$DateTime,
                y=df$Voltage,
                type="l",
                ylab="Voltage",
                xlab="datetime",
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
        
        axis(side=2, at=seq(234,246, length.out=7))
        
        plot3(df)
        
        
        plot(
                x=df$DateTime,
                y=df$Global_reactive_power,
                type="l",
                ylab="Global_reactive_power",
                xlab="datetime",
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
        
        axis(side=2, at=seq(0,0.5,0.1))
        
}

## png(filename="plot4.png", width=480, height=480)

## plot4(df)

## dev.off()