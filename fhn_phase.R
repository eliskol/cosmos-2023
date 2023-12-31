#
# numerically solve FitzHugh-Nagumo equations
#    dv/dt = r*v*(1-v/K)*(v/a-1) - b*w + I0
#    dw/dt = ep*(v-w)
#
#  plot the solution in the phase plane along with the null clines
#

# set parameters
# --------------
dt     = 0.1        # time step
Nt     = 5000        # number of time steps

r     = 0.1         # low v decay rate
K     = 1.0         # carrying capacity
a     = 0.1         # threshold
v0    = 0.2      # initial condition

ep    = 0.005        # recovery rate (inverse time scale)
b     = 1           # rate w lowers v
I0    = 0.05          # input current

#  dv/dt = fv(v,w)
#  dw/dt = fw(v,w)
#
fv=function(v,w) r*v*(1-v/K)*(v/a-1) - b*w + I0
fw=function(v,w) ep*(v-w)

# initialize storage for solution and time vector
#--------------------------------
v   = numeric(Nt+1)
w   = numeric(Nt+1)
t   = (0:Nt)*dt

# set initial conditions
# ----------------------
v[1] = v0

# run simulation
# --------------

for (n in 1:Nt){
  
  # update
  #
  v[n+1] = v[n] + dt*fv(v[n],w[n])
  w[n+1] = w[n] + dt*fw(v[n],w[n])
  
}  # end loop in time

# plot the nullclines in the phase plane
#
vv=seq(from=-0.4,to=1.2,by=0.01)
vdot0 = r*vv*(1-vv)*(vv/a-1)/b + I0/b
wdot0 = vv

# plot v nullcline
#
plot(vv,vdot0,type='l',xlim=c(-0.4,1.2),ylim=c(-0.05,1.5),col='blue',xlab='v',ylab='w')

# plot w nullcline
#
lines(vv,wdot0,lty=5,col='red')

# plot the solution in phase space
#
lines(v,w,col='black',type='b')




