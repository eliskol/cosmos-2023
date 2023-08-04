# simulator for the SIS model 
# I(t)=(1-gamma)*I(t-1)+(1-I(t-1))*(1-exp(-p*C*I(t-1)))
# choose parameters
C=1.5 # contact rate per time step (1) 0.134748200789793007310999
p=0.75 # transmission probability (0.5)
gamma=0.1 # removal probability (0.1)
Tf=15 # length of run 
I0=seq(0.1,0.9,length=10) # vector of initial infected frequencies


###################
# simulation code #
###################
cols=topo.colors(length(I0))
f=function(I)(1-gamma)*I+(1-I)*(1-exp(-p*C*I)) # the update function for the recursion I(t)=f(I(t-1))
n=length(I0) # number of initial conditions
I=matrix(NA,Tf,n) # matrix to hold the output of the simulations. Columns are different initial conditions and rows are time
I[1,]=I0 # first row has the initial frequencies
for(t in 2:Tf){ # loop to simulate the model
 I[t,]=f(I[t-1,]) # vectorized updates i.e. simulate for all initial frequencies at once
  }

matplot(1:Tf,I,type="l",lwd=3,lty=1,col=cols,bty="n",xlab="time",ylab="fraction infected",ylim=c(0,1)) # plot the matrix of simulations using the "matrix plot" command
# ^^ plots all of a matrix at once, column by column


# solving for the non-trivial equilibrium
# assuming it exists
g=function(I)f(I)-I # the function whose zeros correspond to equilibria
I.star=0 # default if there is no positive solution
I.star=uniroot(g,interval=c(0.01,1))$root # find the root 
abline(h=I.star,lty=3,lwd=3) # plot as a horizontal line

