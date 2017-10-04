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

Area_MCP
for i in 1:length(levels(SPDF$ID)) #How many individuals do we have within the dataset? 1:i
	{
	for j in 1:length(levels(subset(SPDF, subset = (ID == i)))$Events) #How many events within individual I? (1:j)
		{ #Alternative coding: concatenate individual_event and go through 1:i_j 
			for k in 2:(length(subset(SPDF, subset = (ID == i)&&(Event == j)))-1) #For step 2 through one step before the last (because we look -/+ 1 step we can't do the first or last...) as k steps...
			{
				#What lay ahead is pseduo code to just sketch my idea of how it's going to work.
				SPDF$TURN[,k] = ANGLEFUNC(subset(SPDF, subset = (ID == i)&&(Event == j))[,k-1],subset(SPDF, subset = (ID == i)&&(Event == j))[,k],subset(SPDF, subset = (ID == i)&&(Event == j))[,k+1]) #Calculate turning angle for a given step from a) what angle it came from and b) what angle it's going to.
				Area_MCP=c(Area_MCP, MCP_FUNC(subset(SPDF, subset = (ID == i)&&(Event == j))[,1:k]) #Calculate MCP for 1:k steps and then record it. I guess we should also note the number of steps (thoughts? rbind(rest of object,c(k, MCP))
				#Once I actually flesh it in with real functions... this code is goign to be incredibly inefficient. ALl this redundant subsetting is going to just chew through cycles like it's no one's business. Also, who makes three nested loops? That's like rookie scripting right there. Luckily I'm not an analyist, so I won't get fired for writing down such bad code.
				#That said, It'll achieve the goals of examinining i:k MCP size. We can include something to look at cumulative distance, as well. 
				#Quite frankly,I forgot why I was thinking about turning angle. It seemed important at the time.
			}
		}#
	}

	
	
	
