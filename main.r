####DF STRUCTURE	
#ID	Evt	Time X	Y
#A	1	10	1	1
#A	1	20	1	2
#A	1	30	2	2
#A	1	40	1	2
#A	1	50	0	2
#A	2	10	0	0
#A	2	20	0	0
#A	2	30	0	0
#A	2	40	0	0
#A	2	50	0	0
#B	1	10	0	13 
#B	1	20 [...]

  ##Set your working directory here
wd = "S:/Alberta_Wolves"
  ##I always set a folder to store my temporary files in so R doesn't write the grids to my system disk. I don't have room for most spatial manipulations on my SSD.
scratch = "S:\temp"
  ## Initial R functions from http://quantitative-ecology.blogspot.com/2007/05/anglefun-function-xxyy-bearing-true-as.html
  ## These are pretty straight forward, and I'm re-using them to save time.
  ## The function we want is bearing.ta()
anglefun <- function(xx,yy,bearing=TRUE,as.deg=FALSE){
  
  ## calculates the compass bearing of the line between two points
  ## xx and yy are the differences in x and y coordinates between two points
  ## Options:
  ## bearing = FALSE returns +/- pi instead of 0:2*pi
  ## as.deg = TRUE returns degrees instead of radians
  c = 1
  if (as.deg){
    c = 180/pi
  }
  
  b<-sign(xx)
  b[b==0]<-1  #corrects for the fact that sign(0) == 0
  tempangle = b*(yy<0)*pi+atan(xx/yy)
  if(bearing){
    #return a compass bearing 0 to 2pi
    #if bearing==FALSE then a heading (+/- pi) is returned
    tempangle[tempangle<0]<-tempangle[tempangle<0]+2*pi
  }
  return(tempangle*c)
}

bearing.ta <- function(loc1,loc2,loc3,as.deg=FALSE){
  ## calculates the bearing and length of the two lines
  ##    formed by three points
  ## the turning angle from the first bearing to the
  ##    second bearing is also calculated
  ## locations are assumed to be in (X,Y) format.
  ## Options:
  ## as.deg = TRUE returns degrees instead of radians
  if (length(loc1) != 2 | length(loc2) != 2 | length(loc3) !=2){
    print("Locations must consist of either three vectors, length == 2,
          or three two-column dataframes")
    return(NaN)
  }
  c = 1
  if (as.deg){
    c = 180/pi
  }
  
  locdiff1<-loc2-loc1
  locdiff2<-loc3-loc2
  bearing1<-anglefun(locdiff1[1],locdiff1[2],bearing=F)
  bearing2<-anglefun(locdiff2[1],locdiff2[2],bearing=F)
  
  if(is.data.frame(locdiff1)){
    dist1<-sqrt(rowSums(locdiff1^2))
    dist2<-sqrt(rowSums(locdiff2^2))
  }else{
    dist1<-sqrt(sum(locdiff1^2))
    dist2<-sqrt(sum(locdiff2^2))
  }
  
  ta=(bearing2-bearing1)
  
  ta[ta < -pi] = ta[ta < -pi] + 2*pi
  ta[ta > pi] = ta[ta > pi] - 2*pi
  return(list(bearing1=unlist(bearing1*c),bearing2=unlist(bearing2*c),
              ta=unlist(ta*c),dist1=unlist(dist1),dist2=unlist(dist2)))
}






	##Pseudo-code below
Area_MCP
for i in 1:length(levels(SPDF$ID)) #How many individuals do we have within the dataset? 1:i
	{
	for j in 1:length(levels(subset(SPDF, subset = (ID == i)))$Events) #How many events within individual I? (1:j)
		{ #Alternative coding: concatenate individual_event and go through 1:i_j 
			for k in 2:(length(subset(SPDF, subset = (ID == i)&&(Event == j)))-1) #For step 2 through one step before the last (because we look -/+ 1 step we can't do the first or last...) as k steps...
			{
				#What lay ahead is pseduo code to just sketch my idea of how it's going to work.
				SPDF$TURN[,k] = bearing.ta(subset(SPDF, subset = (ID == i)&&(Event == j))[,k-1],subset(SPDF, subset = (ID == i)&&(Event == j))[,k],subset(SPDF, subset = (ID == i)&&(Event == j))[,k+1]) #Calculate turning angle for a given step from a) what angle it came from and b) what angle it's going to.
				Area_MCP=c(Area_MCP, MCP_FUNC(subset(SPDF, subset = (ID == i)&&(Event == j))[,1:k]) #Calculate MCP for 1:k steps and then record it. I guess we should also note the number of steps (thoughts? rbind(rest of object,c(k, MCP))
				#Once I actually flesh it in with real functions... this code is goign to be incredibly inefficient. ALl this redundant subsetting is going to just chew through cycles like it's no one's business. Also, who makes three nested loops? That's like rookie scripting right there. Luckily I'm not an analyist, so I won't get fired for writing down such bad code.
				#That said, It'll achieve the goals of examinining i:k MCP size. We can include something to look at cumulative distance, as well. 
				#Quite frankly,I forgot why I was thinking about turning angle. It seemed important at the time.
			}
		}#
	}

	
	
	
