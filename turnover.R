setwd("C:/Users/David/Dev/QETS5")

file <- "turnover2010"
crsp.turnover <- read.csv(paste("data/", file, ".csv", sep=""), header=TRUE)

names(crsp.turnover) <- tolower(names(crsp.turnover))

crsp.turnover$year <- as.numeric(substr(as.character(crsp.turnover$date), 1, 4))
crsp.turnover$month <- as.numeric(substr(as.character(crsp.turnover$date), 5, 6))
crsp.turnover$day <- as.numeric(substr(as.character(crsp.turnover$date), 7, 8))
crsp.turnover$date <- NULL

crsp.turnover$turnover <- crsp.turnover$vol / crsp.turnover$shrout

turnover <- aggregate(crsp.turnover$turnover, crsp.turnover[, c("permno", "year", "month")], mean)
turnover <- turnover[order(turnover$permno, turnover$year, turnover$month),]
names(turnover)[4] <- "turnover"

save(turnover, file=paste(getwd(), paste("output/", file, ".rdata", sep=""), sep="/"))