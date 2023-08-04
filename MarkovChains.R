#Markov chain simulator
# inputs 
# the matrix of transition probabilities 
# for the rbind command, you enter the matrix row by row
# P=rbind(c(7/8,1/8),
#         c(1/4,3/4)) 

P=rbind(c(1, 0, 0, 0, 0, 0),
        c(0.5, 0, 0.5, 0, 0, 0),
        c(0, 0.5, 0, 0.5, 0, 0),
        c(0, 0, 0.5, 0, 0.5, 0),
        c(0, 0, 0, 0.5, 0, 0.5),
        c(0, 0, 0, 0, 0, 1))
X1=4 # initial condition
Tf=200 # number of time steps to run the model 
reps=1000 # number of replications 
# matrix to hold the output
# all entrices are filled with the initial condition
X.mat=matrix(X1,nrow=Tf,ncol=reps)
# run the simulations
# the outer loop goes across the replications
# the inner loop runs each simulation from time 2 to Tf 
for(i in 1:reps){
  for(n in 2:Tf){
    k=dim(P)[1] # the number of states
    p=P[X.mat[n-1,i],] # the vector of probabilities for updating given the current state of the Markov chain
    X.mat[n,i]=sample.int(n=k,size=1,prob=p) # find the next state
  }
}
# plotting using the matrix plot command
# type="l" plots each column using connected line segments
# lty=1 sets all lines to the same style - solid
matplot(X.mat,type="l",lty=1)
# fraction of winners

print(sum(X.mat[Tf,]==6) / reps)