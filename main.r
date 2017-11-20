setwd('~/Documents/ABwolf_r/')
dat=read.csv('snowfall_telemdata.csv')
calendar=read.csv('snowfallDateMOD.csv')
dat$Device_ID = as.factor(dat$Device_ID)
calendar$Device_ID= as.factor(calendar$Device_ID)

device = c() 
## Break the file into individual-based chunks of movement and store to a new directory
for (device in 1:length(levels(dat$Device_ID)))
{
  dat_temp=subset(dat, subset = (dat$Device_ID == levels(dat$Device_ID)[device]))
  write.table(dat_temp, file = paste(c("collars/",levels(dat$Device_ID)[device],".txt"),collapse = ""), sep = "\t")
}

#clean-up
rm(dat)
rm(dat_temp)

##create a list of all the devices we found and were wrote to directory 
collar_list = list.files(path = "collars/")

#Now we need to go through and determine what the preceeding snow event was and update the files accordingly
## Big loop - move amongst files/devices
for (device in 1:length(collar_list))
{
  dat = read.delim(paste(c("collars/",collar_list[device]),collapse = ""),sep = "\t")
  dat$event = rep(NA,nrow(dat))

##Subset the calendar to just the snow events that occured in that wolf's range during the deployment window
  calendar_temp=subset(calendar, subset = (calendar$Device_ID == dat$Device_ID[1]))

##I had to resort to using POSIX statements. God have mercy on my soul.
  dat$jday=as.POSIXlt(as.POSIXct(dat$DT, tz="Canada/Alberta", format="%Y-%m-%d %H:%M:%S"), tz="Canada/Alberta")$yday+1
  calendar_temp$CurSnoPOS = as.POSIXlt(as.POSIXct(calendar_temp$CurSno, tz="Canada/Alberta", format="%Y-%m-%d"), tz="Canada/Alberta")$yday+1
  calendar_temp$NxtSnoPOS = as.POSIXlt(as.POSIXct(calendar_temp$NxtSno, tz="Canada/Alberta", format="%Y-%m-%d"), tz="Canada/Alberta")$yday+1  
##For an individual snow event in 1:number of snow events the device encountered...
  for (snowevent in 1:nrow(calendar_temp))
  {
##For the length of the individual device...
    for (j in nrow(dat))
    {
##Is the jday within range of the event (between CurSnoPOS and NxtSnoPOS)?
      if((dat$jday[j] >= calendar_temp$CurSnoPOS[snowevent]) && (dat$jday[j] <= calendar_temp$NxtSnoPOS[snowevent]))
##Update if true.
          {dat$event[j] <- snowevent} else { dat$event[j] <- dat$event[j]}
    }
  }
##Once the stuff is coded by event, I will probably a) write to disk, b) subset the dat according to LEVELS and then c) go from 1:length running MCP from adehabitatHR   
}
