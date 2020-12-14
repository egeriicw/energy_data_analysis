
# Import, formating and binding of the Daily Global Weather Measurements, 1929-2009 (NCDC, GSOD)
# Description of files available at [http://www7.ncdc.noaa.gov/CDO/GSOD_DESC.txt]
# by: T. Hengl
# last update: 6 Jun 2010.

# ---------------------------------------------------------
# STEP 1: Download the data from server
# ---------------------------------------------------------

library(rgdal)

# pick up a year:
yr <- 2008
ftp.GSOD <- "ftp://ftp.ncdc.noaa.gov/pub/data/gsod/"

# download data for the whole year:
download.file(paste(ftp.GSOD, yr, "/gsod_", yr, ".tar", sep=""), destfile=paste(getwd(), "/gsod_", yr, ".tar", sep=""), mode='wb', method='wget')

# unzip files:
download.file("http://downloads.sourceforge.net/sevenzip/7za465.zip", destfile=paste(getwd(), "/", "7za465.zip", sep=""))
unzip("7za465.zip")
system(paste("7za e -ttar gsod_", yr, ".tar", sep=""))

# list all files:
GSOD.list <- dir(path=getwd(), pattern=glob2rx("*.gz"), full.names=FALSE)
# 9938 files!

# ---------------------------------------------------------
# STEP 2: Reformat and tidy up
# ---------------------------------------------------------

# create an empty list:
GSOD_TP.list <- as.list(rep(NA, length(GSOD.list)))

# run in a loop - unzip values, trim white spaces and write to a single file:
for(j in 1:length(GSOD.list)){
  system(paste("7za e -tgzip", GSOD.list[j], " *.op -aos"), show.output.on.console=FALSE)
  tmp <- readLines(paste(getwd(), "/", strsplit(GSOD.list[j], ".gz")[[1]][1], sep=""))
  tmp <- gsub(pattern='[[:space:]]+', replacement=',', tmp)
  write.table(t(matrix(unlist(strsplit(tmp, ",")[-1]), nrow=22)), "tmp.txt", col.names=FALSE, row.names=FALSE)
  tmp.f <- read.table("tmp.txt", col.names=c("STN", "WBAN", "YEARMODA", "TEMP", "TEMP.count", "DEWP", "DEWP.count", "SLP", "SLP.count", "STP", "STP.count", "VISIB", "VISIB.count", "WDSP", "WDSP.count", "MXSPD", "GUST", "MAX", "MIN", "PRCP", "SNDP", "FRSHTT"), stringsAsFactors=FALSE, as.is=TRUE)
  # tidy up:
  tmp.f$TEMPC <- round(ifelse(tmp.f$TEMP==9999.9, NA, (tmp.f$TEMP-32)*5/9), 1)  # convert to Celsius
  tmp.f$PREC <- as.numeric(substr(tmp.f$PRCP, 1, nchar(tmp.f$PRCP)-1))
  tmp.f$PREC <- round(ifelse(tmp.f$PREC==99.9, NA, tmp.f$PREC*25.4), 2)  # convert to mm
  tmp.f$PREC.flag <- substr(tmp.f$PRCP, nchar(tmp.f$PRCP), nchar(tmp.f$PRCP))
  tmp.f$PREC.flag <- ifelse(tmp.f$PREC.flag=="I"|tmp.f$PREC.flag=="A"|tmp.f$PREC.flag=="B"|tmp.f$PREC.flag=="C"|tmp.f$PREC.flag=="D"|tmp.f$PREC.flag=="E"|tmp.f$PREC.flag=="F"|tmp.f$PREC.flag=="G"|tmp.f$PREC.flag=="H", tmp.f$PREC.flag, "no flag") 
  GSOD_TP.list[[j]] <- tmp.f[,c("STN", "WBAN", "YEARMODA", "TEMPC", "TEMP.count","PREC", "PREC.flag")]
}
# This can take cca 20-40 minutes!

# bind all individual stations together:
gc(); gc()
GSOD.2008 <- do.call(rbind, GSOD_TP.list)  # this takes cca 10 minutes!
gc()
str(GSOD.2008)  # 3,414,183 records!
GSOD.2008$DATE <- as.Date(paste(substr(GSOD.2008$YEARMODA, 1, 4), substr(GSOD.2008$YEARMODA, 5, 6), substr(GSOD.2008$YEARMODA, 7, 8), sep="-"))
GSOD.2008$STNID <- as.factor(paste(GSOD.2008$STN, GSOD.2008$WBAN, sep="-"))  # 9938 stations

# ---------------------------------------------------------
# STEP 3: Attach coordinates and export to a table/shapefile
# ---------------------------------------------------------

# read the location coordinates of stations:
download.file(paste(ftp.GSOD, "ish-history.csv", sep=""), destfile=paste(getwd(), "/ish-history.csv", sep=""), mode='wb', method='wget')
stations <- read.csv("ish-history.csv")
str(stations)
stations$STNID <- as.factor(paste(stations$USAF, stations$WBAN, sep="-"))  # WMO/DATSAV3 number
# format coordinates to decimal degrees:
stations$LAT <- ifelse(stations$LAT > 90.0*1000|stations$LAT < -90.0*1000, NA, stations$LAT/1000)
stations$LON <- ifelse(stations$LON > 180*1000|stations$LON < -180*1000, NA, stations$LON/1000)
stations$ELEV..1M. <- ifelse(stations$ELEV..1M.==-99999|stations$ELEV..1M.==-999.999, NA, stations$ELEV..1M./10)
plot(stations$LON, stations$LAT)

# merge two tables and write to a shape file:
GSOD.2008.XY <- merge(GSOD.2008, stations[,c("STNID", "STATION.NAME", "LAT", "LON", "ELEV..1M.")], by=c("STNID"))
# this takes cca 10 minutes!
str(GSOD.2008.XY)
GSOD.2008.XY <- subset(GSOD.2008.XY, !is.na(GSOD.2008.XY$LAT)&!is.na(GSOD.2008.XY$LON))
coordinates(GSOD.2008.XY) <- ~LON+LAT
proj4string(GSOD.2008.XY) <- CRS("+proj=longlat +datum=WGS84")

GSOD.20080501.XY <- subset(GSOD.2008.XY, GSOD.2008.XY$DATE==as.Date("2008-05-01"))
# bubble(GSOD.20080501.XY, "TEMPC")
writeOGR(GSOD.20080501.XY[,c("STNID", "TEMPC", "PREC")], "GSOD20080501_XY.kml", "GSOD20080501_XY", "KML")
writeOGR(GSOD.20080501.XY[,c("STNID", "YEARMODA", "TEMPC", "TEMP.count", "PREC", "PREC.flag")], "GSOD20080501_XY.shp", "GSOD20080501_XY", "ESRI Shapefile")
# unlink("GSOD_20080501_XY.shp")

# subset Bilogora case study:
bilogora.bbox <- data.frame(x=c(466500, 874500), y=c(4878500, 5286500))
coordinates(bilogora.bbox) <- ~x+y
proj4string(bilogora.bbox) <- CRS("+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
bilogora.ll <- spTransform(bilogora.bbox, CRS("+proj=longlat +datum=WGS84"))
GSOD.2008.bilogora <- subset(GSOD.2008.XY, GSOD.2008.XY@coords[,1]>floor(bilogora.ll@coords[1,"x"])&GSOD.2008.XY@coords[,1]<ceiling(bilogora.ll@coords[2,"x"])&GSOD.2008.XY@coords[,2]>floor(bilogora.ll@coords[1,"y"])&GSOD.2008.XY@coords[,2]<ceiling(bilogora.ll@coords[2,"y"]))
# 38889 records
bubble(subset(GSOD.2008.bilogora, GSOD.2008.bilogora$DATE==as.Date("2008-05-01")&!is.na(GSOD.2008.bilogora$PREC)), "PREC")

# export as csv file:
write.table(GSOD.2008.bilogora@data, "GSOD_2008_bilogora.csv", row.names=FALSE, sep=";", quote=FALSE) 

# to read data to R:
# GSOD.2008.bilogora <- read.table("GSOD_2008_bilogora.csv", sep=";", header=TRUE)

# end of script;