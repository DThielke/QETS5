# Homework 3
# David Thielke, Dejing Huang, Michael Casini, Chris Penney

# load the data and rename columns
comp <- read.csv("data/crsp.csv", header=TRUE)
names(comp) <- tolower(names(comp))
names(comp)[2] <- "permno"
names(comp)[4] <- "year"

# use cusip to remove non-ordinary shares
comp$cusip <- as.character(comp$cusip)
nchar <- nchar(comp$cusip)
comp <- comp[nchar >= 8,]
comp$cusip[nchar == 8] <- paste("0", comp$cusip[nchar == 8], sep="")
comp$cic <- substr(comp$cusip, 7, 8)
comp <- comp[comp$cic == "10" | comp$cic == "11",]
comp$cusip <- NULL

# remove financial stocks based on SIC
comp$sich <- ifelse(is.na(comp$sich), comp$sic, comp$sich)
comp <- comp[comp$sich < 6000 | comp$sich > 6999,]
comp$sic <- NULL
comp$sich <- NULL

# remove duplicate entries
comp <- comp[!duplicated(comp[, c("year", "permno")]),]

# remove any characters from key values
destring <- function(x, columns=names(crsp)) {
    tmp <- x
    tmp[, columns] <- suppressWarnings(lapply(lapply(x[, columns], as.character), as.numeric))
    return (tmp)
}
comp <- destring(comp, c("act", "at", "che", "csho", "dlc", "dp", "dvp", "ib", "lct", "lt",
                         "pstkl", "pstkrv", "txdi", "txditc", "upstk", "adjex_f"))

# reduces columns of a matrix into a single vector, choosing the first value of each row
# from left to right that is non-zero and not missing
best.available <- function(x) {
    if (is.null(dim(x))) {
        return (ifelse(is.na(x), 0, x))
    } else {
        best <- apply(x, MARGIN=1, FUN=best.available.helper)
        return (best)
    }
}

# helper function for best.available
best.available.helper <- function(row) {
    best <- which(row != 0)[1]
    if (is.na(best))
        return (0)
    else
        return (row[best])
}

# calculate book equity (in millions)
comp$be <- comp$at - comp$lt + best.available(comp$txditc) - best.available(cbind(comp$pstkl, comp$pstkrv, comp$upstk))

# calculate profitability (ROA)
comp$roa <- (comp$ib - best.available(comp$dvp) + best.available(comp$txdi)) / comp$at

stocks <- levels(factor(comp$permno))
for (s in stocks) {
    stock <- comp$permno == s # row indices of stock
    len <- length(comp[stock,1]) # number of periods for this stock
    trim <- len - 1 # used to trim the last entry (since we are dealing with y-1 and y-2)
    
    # calculate asset growth
    at <- comp$at[stock]
    comp$agr[stock][2:len] <- (diff(at) / at[-len])
    
    # calculate net stock issues
    shares <- comp$csho[stock] * comp$adjex_f[stock]
    comp$nsi[stock][2:len] <- (shares[-1] / shares[-len])
    
    # calculate accruals
    dp <- comp$dp[stock]
    comp$acc[stock][2:len] <- ((
        diff(comp$act[stock]) - 
        diff(comp$lct[stock]) - 
        diff(comp$che[stock]) + 
        diff(comp$dlc[stock]) - 
        dp[-1]) / at[-len])
}

# load CRSP data
load("data/smr.Rdata")
names(crsp.clean) <- tolower(names(crsp.clean))
# grab december data only
crsp <- crsp.clean[crsp.clean$month == 12, c("permno", "year", "month", "prc", "shrout")]
# calculate december market caps
crsp$mktcap <- abs(crsp$prc) * crsp$shrout / 1000
# merge the results back in
comp <- merge(comp, crsp, by=c("permno", "year"))
# calculate book to market equity ratios
comp$btm <- comp$be / comp$mktcap
comp$btm[comp$btm < 0] <- NA

# save the results
comp <- comp[,c("permno","year","month", "be","roa","agr","nsi","acc","btm","mktcap")]
save(comp, file=paste(getwd(), "output/comp.Rdata", sep="/"))