# Homework 2: Working with CRSP Data

# read the data
crsp <- read.csv("data/crsp.csv", header=TRUE)

if (is.null(crsp$date)) {
    crsp$date <- crsp$DATE
    crsp$DATE <- NULL
}

# separate YYYYMMDD dates into years, months and days
crsp$year <- as.numeric(substr(as.character(crsp$date), 1, 4))
crsp$month <- as.numeric(substr(as.character(crsp$date), 5, 6))
crsp$day <- as.numeric(substr(as.character(crsp$date), 7, 8))
crsp$date <- NULL

# keep only stocks listed on NYSE (1), AMEX (2) or NASDAQ (3)
crsp <- crsp[crsp$EXCHCD >= 1 & crsp$EXCHCD <= 3 & !is.na(crsp$EXCHCD),]

# keep only ordinary stocks (share code 11)
crsp <- crsp[crsp$SHRCD == 11 & !is.na(crsp$SHRCD),]

# keep only non-financial stocks
crsp <- crsp[(crsp$SICCD < 6000 | crsp$SICCD > 6999) & !is.na(crsp$SICCD),]

# remove any duplicate observations for a given stock in a given month
crsp <- crsp[!duplicated(crsp[, c("month", "year", "PERMNO")]),]

# count the number of stocks
crsp$PERMNO <- factor(crsp$PERMNO)
stocks <- levels(crsp$PERMNO)
nstock <- length(stocks)

# remove any characters from key values
destring <- function(x, columns=names(crsp)) {
    tmp <- x
    tmp[, columns] <- suppressWarnings(lapply(lapply(x[, columns], as.character), as.numeric))
    return (tmp)
}
crsp <- destring(crsp, c("RET", "RETX", "DLRET", "DLRETX", "PRC", "VOL", "SHROUT"))

# replace missing returns with delisting returns
crsp$RET[is.na(crsp$RET)] <- crsp$DLRET[is.na(crsp$RET)]
crsp$RETX[is.na(crsp$RETX)] <- crsp$DLRETX[is.na(crsp$RETX)]

# remove unused data
crsp$DLRET <- NULL
crsp$DLRETX <- NULL
crsp$SHRCD <- NULL
crsp$SICCD <- NULL

# remove observations with missing returns
crsp <- crsp[!is.na(crsp$RET) & !is.na(crsp$RETX),]

# calculate portfolio dates
crsp$pyear <- ifelse(crsp$month < 7, crsp$year - 1, crsp$year)
crsp$pmonth <- ifelse(crsp$month < 7, crsp$month + 6, crsp$month - 6)

# calculate market capitalization
## june only
junes <- crsp$month == 6
size <- data.frame(PERMNO=crsp$PERMNO[junes], pyear=crsp$year[junes], EXCHCD=crsp$EXCHCD[junes], mktcap=(crsp$SHROUT[junes] * abs(crsp$PRC[junes])))
crsp <- merge(crsp, size[,c("PERMNO", "pyear", "mktcap")], by=c("PERMNO", "pyear"))

# sort the cleaned data and remove unused variables
crsp.clean <- crsp[order(crsp$PERMNO, crsp$year, crsp$month),]
rm(crsp, junes)

# calculate lagged monthly market cap
crsp.clean$lagmktcap <- c(NA, (crsp.clean$SHROUT * abs(crsp.clean$PRC))[-nrow(crsp.clean)])
lags <- data.frame(permno=c(-1, diff(crsp.clean$PERMNO)), pyear=c(-1, diff(crsp.clean$pyear)), pmonth=c(-1, diff(crsp.clean$pmonth)))
crsp.clean$lagmktcap[!(lags$permno == 0 & ((lags$pyear == 0 & lags$pmonth == 1) | (lags$pyear == 1 & lags$pmonth == -11)))] <- NA

# given an array of returns, computes the compounded returns using a 
# sliding window which runs from (t-from) to (t-to)
trailing.compound.return <- function(ret, from, to) {
    nper <- length(ret)
    
    if (nper <= from)
        return(NA)
    
    # take advantage of cumsum to greatly speed up calculations
    cum.ret <- cumsum(log(1 + ret))
    compound.ret <- vector(mode="numeric", length=nper)
    compound.ret[1:from] <- NA
    compound.ret[from + 1] <- cum.ret[from - to + 1]
    # use indexing to subtract cumulative sums rather than looping
    if (nper >= from + 2)
        compound.ret[(from + 2):nper] <- cum.ret[(from - to + 2):(nper - to)] - cum.ret[1:(nper - from - 1)]
    return (exp(compound.ret) - 1)
}

options(warn=2)

# calculate compound momentum and reversal returns
tic <- proc.time()
for (i in 1:nstock) {
    s <- stocks[i]
    print(paste(round(i / nstock * 100, 2), "% - ", s, sep=""))
    rows <- crsp.clean$PERMNO == s
    returns <- crsp.clean$RET[rows]
    crsp.clean$momentum[rows] <- trailing.compound.return(returns, 12, 2)
    crsp.clean$reversal[rows] <- trailing.compound.return(returns, 60, 13)
}
toc <- proc.time()
print(toc - tic) # time momentum/reversal calculations

# save the results
save(crsp.clean, file=paste(getwd(), "output/smr.Rdata", sep="/"))
save(size, file=paste(getwd(), "output/size.Rdata", sep="/"))
