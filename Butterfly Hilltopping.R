setwd("~/Desktop/Agent-Based-Models")

#parameters
#maximum elevation of landscape
elevation = c(0, 400) 
#landscape size
landscape = 150   
#number of individuals on the landscape
nindvs = 50      
#number of steps allowed by an individual
nsteps = 500  
#probability of moving to a higher hilltop
move  = 0.8     

#set randomly generated numbers at seed 1000 so landscape appears the same each time
set.seed(1000)

#create landscape function with increasing elevation
#use function for landscape
landscapeinit=function(elevation,landscape){
  #matrix size for landscape
  land=matrix(nrow=landscape, ncol=landscape)
  #peak values for x axis
  xpeak=floor(runif(1, min=1, max=landscape))
  #peak values for y axis
  ypeak=floor(runif(1, min=1, max=landscape))
  #subset of peaks with an elevation of 400
  land[xpeak,ypeak]=elevation[2]

  #assign  peak values to matrix using gamma distribution
    land[xpeak,1:(ypeak-1)]=round(seq(elevation[1],elevation[2],(elevation[2]-elevation[1])/(ypeak-2))+rgamma((ypeak-1),1,0.5),0)
  #assign peak values to matrix using gamma distribution 
    land[xpeak,(ypeak+1):landscape]=round(rev(seq(elevation[1],elevation[2],(elevation[2]-elevation[1])/(landscape-ypeak-1))+rgamma((landscape-ypeak),7.5,1)),0)
   
    #loop to assign values
    for(i in (xpeak-1):1){
      land[i,]=land[(i+1),]-round(rgamma(landscape,7.5,1),0)
    }
    #loop to assign values
    for(i in (xpeak+1):landscape){
      land[i,]=land[(i-1),]-round(rgamma(landscape, 7.5, 1), 0)
    }
    return(land)
}
#input elevation and lanscape into the function
land=landscapeinit(elevation,landscape)
#image to see visual representation of the landscape
image(land)

#initialize individuals on the landscape 
#function for randomly placing individuals  
newpop=function(nindvs,landscape){
  #set up a matrix for coordinates of individuals
  pop=matrix(nrow=nindvs,ncol=2) 
 
  #determine spacing of the individuals 
  var=44
 
  #x and y coordinates of the individuals 
  x= floor(runif(1, min=0,max=landscape-var))
  y= floor(runif(1, min=0,max=landscape-var))
  
  #assign coordinates to population adding variance through a poisson distribution  
  pop[,1]= x + rpois(nindvs, var)
  pop[,2]= y + rpois(nindvs, var)
  
  
  return(pop)
}
#establish population of individuals and name it pop
pop = newpop(nindvs, landscape)

#image of the landscape
image(land)
#plot the individuals on the landscape
points(pop[,1]/150, pop[,2]/150,pch=18, cex=1)
  
#create function to have individuals move to the hilltop 
movement=function(x){
  #movement matrix
  mover=matrix(NA,nsteps,2)
  mover[1,]=pop[x,]
  probability=NA
  #loop for individuals
  for ( i in 2:nsteps){
    #probability of movement
    probability[i]=rbinom(1,1,move)
    #define the adjacent values to the individuals 
    adjacent=land[ifelse((mover[i-1,1])==1,1,(mover[i-1,1]-1)):ifelse((mover[i-1,1])==150,150, (mover[i-1,1]+1)),
                  ifelse((mover[i-1,2])==1,1,(mover[i-1,2]-1)):ifelse((mover[i-1,2])==150,150,(mover[i-1,2]+1))]
    #define value at which individuals will move 
    shift=max(which(adjacent==max(adjacent)))
    #use if else to determine movement probabilities and which 'adjacent' spot they move
    mover[i,2] =if (probability[i]==1){ifelse(shift %in% c(1,2,3),mover[(i-1),2]-1,
                                              ifelse(shift%in% c(7,8,9), mover[(i-1),2]+1,
                                                     mover[(i-1),2]))                    
    }else mover[(i-1),2]+sample(c(1,0,-1),1)
    mover[i,1] = if (probability[i]==1){ifelse(shift %in% c(1,4,7),mover[(i-1)]-1,
                                               ifelse(shift %in% c(3,6,9), mover[(i-1),1]+1,
                                                      mover[(i-1),1]))
    }else mover[(i-1),1]+sample(c(1,0,-1),1)
    
  }
  #return mover with movement probabilities
  return(mover)
}


#execute movements with new movement function
movements = lapply(1:nindvs, movement)  

quartz()
#plot landscape and starting points of butterflies
image(land)
points(pop[,1]/150, pop[,2]/150,pch=18, cex=1)

#plot movements of the butterflies to the hilltop
movements2=lapply(movements,function(x)x/150)
lapply(movements2, lines)

#cryptic corridor emerges at the center as the individuals all move left then follow it to the top of the landscape 
