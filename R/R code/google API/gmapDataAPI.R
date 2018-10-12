# Google Geo Data --- unlimited access without restrictions
#
#
library("data.table")
library("httr")
library("stringr")
library("XML")

# Read Data example (Data example provided in the header)
newdata <- read.csv("C:\\Users\\edward.wang\\Documents\\R code\\google API\\r_geocodes.csv", header = TRUE, sep=";")

newdata$URL <- with(newdata, paste("https://www.google.de/maps/dir/",lat1,"+",lon1,"/",lat2,",",lon2, sep=""))
newdata$URL <- as.character(newdata$URL)

  
  # Function Extracting the last n characters from a string 
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
#######################################################################
# Function to request google maps driving distance
download.maybe <- function(url, refetch=FALSE, path=".") {
  cnamet <- as.data.table(as.character(GET(url)))
  cnamet <- as.character(cnamet)
  # Compute Distance
  dis<-substring((strsplit(substrRight(strsplit(cnamet,"km")[[1]][1], 9), ",")[[1]])[2], 2)
  dis
  # Compute Time
  # Minutes
  dur_m <- as.numeric(gsub( "[^[:alnum:],]", "", substrRight(strsplit(cnamet,"Min.")[[1]][1], 4)))
  dur_m
  # Hours (if applicable)
  durh_h_new<-as.numeric(gsub( "[^[:alnum:]]", "",ifelse(grepl("Std", substrRight(strsplit(cnamet,"Min")[[1]][1], 15))=="TRUE",str_extract_all(substrRight(strsplit(substrRight(strsplit(cnamet,"Std")[[1]][1], 3),"Std.")[[1]][1], 5),"\\(?[0-9,.]+\\)?")[[1]],"0")))
  durh_h_new
  # Change in Minutes
  dur_fin<-dur_m+(durh_h_new*60)
  dur_fin
  # Combine all
  fin<- as.character(paste (dis, dur_fin, sep = " ", collapse = NULL))
  fin
}


# First Row: Google URL
# Second Column: Distance
# Third Column: Driving Time (Hint: Always the current driving time . might differ due traffic!!!)
files <- as.data.frame(t(as.data.frame(strsplit(sapply(newdata$URL, download.maybe, path=path), "\\, |\\,| "))))
colnames(files)[1] <- "Distance in km"
colnames(files)[2] <- "Driving Time in minutes"