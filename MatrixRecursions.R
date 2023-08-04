# Linear finite difference equations
# This file solves difference equations of the form 
# v(n)=v(n-1)%*%P
# where v(n) is a vector of length k, P is a kxk matrix and %*% denotes matrix multiplication



####
# inputs
####
# define our transition matrix 
# P=rbind(c(0,0.6),
#         c(3.6,0.6))
a = 1
b = 1 
P=matrix(c(0, 0, 0,0, 127, 4, 80,
           0.6747 * a, 0.737, 0, 0, 0, 0, 0,
           0, 0.0486, 0.6610, 0, 0, 0, 0,
           0, 0, 0.0147, 0.6907, 0, 0, 0,
           0, 0, 0, 0.0518 * b, 0, 0, 0,
           0, 0, 0, 0, 0.8091 * b, 0, 0,
           0, 0, 0, 0, 0, 0.8091 * b, 0.8089 * b),7,byrow=FALSE)

# define initial condition
v1=c(100, 0, 0, 0, 0, 0, 0)

# define length of run 
Tf=10

#####
# output
#####

v.mat=matrix(NA,nrow=Tf,ncol=length(v1))

###
# simulate
###
v.mat[1,]=v1
for(n in 2:Tf)v.mat[n,]=v.mat[n-1,]%*%P

# plot
matplot(v.mat,type="l",lty=1)

# # plot ratio of densities (juveniles)
# top=v.mat[-1,1]
# bottom=v.mat[-Tf,1]
# 
# plot(top/bottom, type="l")
# 
# print(top[Tf-1]/bottom[Tf-1])
# 
# # pull out the "right" eigenvalue of P (dominant eigenvalue)
# 
print(Re(eigen(P)$values[1]))