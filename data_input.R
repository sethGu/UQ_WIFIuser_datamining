library(jsonlite)

file_name <- 'C:/Users/J.G/Desktop/Rworkshop/smartcampus_data/uq-wireless-locations-w41-2018.json'
DT <-jsonlite::stream_in(textConnection(readLines(file_name, n=100000)),verbose=F)
# DT <- read.table(file_name, sep = ",")

# unset variable if necessary: rm(variable)

DT.pos <- DT$`_source`$source$geoip$location
DT.id <- DT$`_source`$user$uid


# in this case, SOME of them are with a " " (blank) in the front
DT.pos
temp.lon <- substring(DT.pos, 20, 39)
temp.lat <- substring(DT.pos, 1, 19)
# As there is , in string DT.pos, it is infeasible to convert
# the string into numerical variable by DT.pos strings.


# In this step, there is regular expression [, ] 
# which replace the ", " in the string. Then convert
# the string into numeric, thus, these data can be 
# operated
options(digits = 18)
DT.pos.lon <- as.numeric(gsub("([, ])", "", temp.lon))
DT.pos.lat <- as.numeric(gsub("([, ])", "", temp.lat))
range(DT.pos.lon)
range(DT.pos.lat)
DT.pos.lon[1:200]
DT.pos.lat[1:200]
# *** keep processing, filter out ***
# *** those clearly noise point ***
# There is a problem of significant digit which is 
# by using as.numeric, only 7 significant is kept.
# To address it, options(digit=) function is used to set
# the environment.
options(digits = 18)
class(DT.pos.lon)

# for loop will not change the numeric set itself
# i.e. i will change if it was printed, but at last
# the set does not change
# so use a new numeric vector to receive the data

length(DT.pos.lon)
for(i in 1:99999) {
  if(DT.pos.lon[i]<150) {
    if(DT.pos.lon[i]<4 & DT.pos.lon[i]>3) {DT.pos.lon[i]=DT.pos.lon[i]+150}
    if(DT.pos.lon[i]<54 & DT.pos.lon[i]>53) {DT.pos.lon[i]=DT.pos.lon[i]+100}
    # if not in the range of 3.xx or 53.xx or 153.xx, choose a random value
    else {DT.pos.lon[i]=DT.pos.lon[1]}
  }
}
range(DT.pos.lon)
DT.pos.lon[1:200]

###    End of preprocessing data.    ###



# see the basic information of them
summary(DT.pos.lon)
# print type of class
#lapply(DT.pos.lat, class)

#table(unlist(z))

# show the detailed number of Internet usage for a user
lon.stat <- table(DT.id)
lon.stat


names(DT) <- c("data")
names(DT$data) <- c("timestamp","source","user","clientType")

# transport all these into data frame, which will be used later
###    DT.UQ    ###
DT.UQ <- data.frame(
  timestamp = DT$data$timestamp,
  clientType = DT$data$clientType$clientType,
  building = DT$data$source$location$building_comp,
  service = DT$data$user$service,
  id = DT.id,
  lon = DT.pos.lon,
  lat = DT.pos.lat
)
DT.UQ
DT
class(DT)
class(DT.UQ)
DT.UQ$clientType

### output the file
getwd()
rscpt <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(rscpt)

save(DT.UQ,file = "./preprocessed_data/DT_UQ.Rda")


