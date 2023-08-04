#
# numerically solve logistic equation with allee effect
#   dv/dt = r*v*(1-v/K)*(v-a)
#   for predation add -bx to dv/dt with parameter b


# set parameters
# --------------

dt     = 0.1        # time step
Nt     = 600        # number of time steps

r     = 0.25          # low pop growth rate 
K     = 10.0         # carrying capacity
a     = 2           # idk, other parameter
b     = 0.2         # predation/death rate
v0    = 10            # initial condition

# logistic growth function
#
fv=function(v) r*v*(1-v/K)*(v-a)-b*x


# initialize storage for solution and time vector
#--------------------------------
v   = numeric(Nt+1)
t   = (0:Nt)*dt

# set initial conditions
# ----------------------
v[1] = v0

# run simulation
# --------------

for (n in 1:Nt){
  
  # update
  #
  v[n+1] = v[n] + dt*fv(v[n])
  
  
}  # end loop in time

# plot the solution
#------------------
plot(t,v,xlab='time')


