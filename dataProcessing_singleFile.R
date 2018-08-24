#' ---
#' title: "Data processing for TRAFx vehicle counters"
#' author: "Matthew W. Van Scoyoc"
#' date: "04 November, 2016"
#' ---
#' 
#' Original date: 04 November, 2016  
#' Revision date: 09 November, 2016  
#'
#' Project: Southeast Utah Group Visitor Use  
#' Data files:  
#'   - various text files from TRAFx vehicle counters  
#'   - SEUG_TRAFx.accdb  
#' Associated R files: dataProcessing_directory.R  
#' Associated directories:  
#' References:  
#' Notes:  
#'----------------------------------------------------------------------

#+ Setup ----
#-- Packages
#install.packages(c("RODBC", "dplyr", "tidyr", "lubridate"))
library("RODBC") # Connect to databases (Windows OS)
library("tidyverse") # Data management tools
library("lubridate") # Makes dating easier

#-- Custom functions
count.nas <- function(dat){
  x = apply(dat, 2, function(x) sum(is.na(x)))
  return(x)
}

#+ Data ----
#-- Load data from database
my.dir <- "P:/1_ResourceStewardshipScience/Visitor Use Program/4 Data/TRAFx"
data.db <- odbcConnectAccess2007(paste(my.dir, "SEUG_TRAFx.accdb", sep = "/"))
# Load tables from database
tblDwnlds <- sqlFetch(data.db, "tblCounterDownloads")
tblData <- sqlFetch(data.db, "tblData")


#-- Read in *.TXT file
thisFile <- file.choose()
txt <- as.vector(read.table(thisFile, sep = "\t", strip.white = T))
# Determine number of counters in file
counters <- length(txt[grep("*Serial Number ", txt$V1), ])

#-- Subset download information
info <- c("*Serial Number ", "*Counter name ", "*Mode ", "PERIOD", "DELAY", 
          "THRESHOLD", "RATE")
dwnlds <- data.frame(matrix(NA, counters, length(info)))
colnames(dwnlds) = c("DeviceID", "CounterName", "Mode", "Period", 
                             "Delay", "Threshold", "Rate")
for (i in 1:length(info)){
  x <- data.frame(V1 = txt[grep(info[i], txt$V1), ]) %>%
    separate(V1, into = c("Description", "Value"), sep = ":")
  dwnlds[, i] = x$Value
}
dwnlds$RID <- (max(tblDwnlds$RID) + 1):(max(tblDwnlds$RID) + nrow(dwnlds))
myFile <- strsplit(thisFile, "\\\\")
dwnlds$File <- myFile[[1]][length(myFile[[1]])]

#-- Subset and format data
locs <- data.frame(Starts = (dat.start <- grep("Calibration", txt$V1) + 1), 
                   Ends = (dat.end <- grep("END OF DATA", txt$V1) - 1))%>%
  mutate(Rows = Ends - Starts)
dat <- data.frame(matrix(NA, 0, 5))
colnames(dat) <- c("DownloadID", "DeviceID", "Date", "Time", "Value")
for (i in 1:nrow(locs)){
  x = tbl_df(data.frame(V1 = txt[locs[i, 'Starts']:locs[i, 'Ends'], ])) %>%
    separate(V1, into = c("Date", "Time", "Value"), sep = ",") %>%
    mutate(Date = ymd(Date), 
           Value = as.numeric(Value), 
           DeviceID = dwnlds$`DeviceID`[i]) %>%
    mutate(DownloadID = gsub(" |-", "", paste0(DeviceID, max(Date)))) %>%
    select(DownloadID, DeviceID, Date, Time, Value) %>%
    arrange(Date, Time)
  dat = bind_rows(dat, x)
}
dat$RID <- (max(tblData$RID) + 1):(max(tblData$RID) + nrow(dat))

#-- Create download ID
dwnld.sum <- dat %>%
  select(DownloadID, DeviceID, Date) %>%
  group_by(DownloadID, DeviceID) %>%
  summarise(FirstDate = min(Date), 
            LastDate = max(Date))%>%
  select(DeviceID, FirstDate, LastDate, DownloadID)
dwnlds <- left_join(dwnlds, dwnld.sum)
dwnlds$Notes <- ifelse(is.na(dwnlds$DownloadID), 
                       "Counter error, no data collected.", 
                       "")


#+ QAQC ----
dat.sum <- dat %>%
  group_by(DeviceID) %>%
  summarise(n = n(), 
            Min = min(Value, na.rm = T), 
            Median = median(Value, na.rm = T), 
            Mean = round(mean(Value, na.rm = T), 3), 
            Max = max(Value, na.rm = T),
            SD = round(sd(Value, na.rm = T), 3), 
            CV = round((SD/Mean) * 100, 3),
            NAs = sum(is.na(Value)), 
            Start.RID = min(RID), 
            End.RID = max(RID))
nas <- count.nas(dat)
print(unique(dwnlds$File)) #delimit on "/"
print("Device information"); print(dwnlds[, c(1:2, 10:13)])
print("Summary statistics"); print(data.frame(dat.sum))
print("Count of NA's"); print(nas)
readline(prompt="Press [enter] to continue.")

#+ Save ----
#-- Format dat for saving to database
dwnlds$FirstDate <- as.character(dwnlds$FirstDate)
dwnlds$LastDate <-  as.character(dwnlds$LastDate)
dat$Date <- as.character(dat$Date)

#-- Save to database
sqlSave(data.db, dwnlds, "tblCounterDownloads", append = T, rownames = F)
sqlSave(data.db, dat, "tblData", append = T, rownames = F)


#+ End script ----
#-- Close database connections
odbcClose(data.db)
rm(list = ls(all.names = T))

sessionInfo()
Sys.time()