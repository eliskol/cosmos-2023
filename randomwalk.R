#
# randomwalk.R -- simulation of a single particle undergoing a random walk
#   on an integer lattice in 1D
#   
Nt = 200         # number of steps 
p = 0.4          # probability of jumping left or right, p must be <= 0.5
Xstart = 0       # starting position

# loop in time
#
X=Xstart;
cat(sprintf('step=%4i, position=%4i \n',0,X))        
for (n in 1:(Nt-1)){
    r = runif(1,0,1)
    if( r < p ) {
     	X = X+1    # jump right
    }
    else if( r < 2*p ){
        X = X-1    # jump left
    }            	 
    else {
        X = X      # stay home
    }
    
    # output to screen
    #
    cat(sprintf('step=%4i, position=%4i, rand=%f \n',n,X,r))

}

