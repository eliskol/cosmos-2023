# parameters
r = 5
a = 0.1
b = 10
c = 0
mu = 0.01
delta_t = 0.05
time_steps = 10000
V0 = 0.2
w0 = 0

dVdt = function(V, w) r * (V - a) * (1 - V) * V - w
dwdt = function(V, w) mu * (b * (V - c) - w)

V = numeric(time_steps)
w = numeric(time_steps)

V[1] = V0
w[1] = w0

for (n in 1:time_steps) {
  V[n + 1] = V[n] + dVdt(V[n], w[n]) * delta_t
  w[n + 1] = w[n] + dwdt(V[n], w[n]) * delta_t
}

plot(V, type="l", ylim = c(-0.5, 1))