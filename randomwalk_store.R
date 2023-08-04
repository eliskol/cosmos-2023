#
# randomwalk_store.R -- simulation of a single particle undergoing a random walk
#   on an integer lattice in 1D
#   store all positions and make a plot

Nt = 200         # number of steps 
p = 0.4          # probability of jumping left or right, p must be <= 0.5

X = array(0,Nt)  # array of positions
T = 0:(Nt-1)     # array of time values

# loop in time
#
for (n in 1:(Nt-1)){
    r = runif(1,0,1)
    if( r < p ) {
     	X[n+1] = X[n]+1    # jump right
    }
    else if( r < 2*p ){
        X[n+1] = X[n]-1    # jump left
    }            	 
    else {
        X[n+1] = X[n]      # stay home
    }        

}

# plot the result
#
plot(T,X,ylim=max(abs(X))*c(-1,1))  # space vs. time
#plot(X,T,xlim=max(abs(X))*c(-1,1)) # time vs. space
