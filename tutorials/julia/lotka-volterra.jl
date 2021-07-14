#--------------------------------------
# This script sets out to produce a
# basic Lotka-Volterra equations
# animation in Julia
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 14 July 2021
#--------------------------------------

using DifferentialEquations, Plots

# Define the Lotka-Volterra model

params = (2.0, .5, .2, .6)

function myLV(Δu, u, params, t)

    α, β, γ, δ = params

    Δu[1] = α*u[1] - β*u[1]*u[2]
    Δu[2] = -δ*u[2]+ γ*u[1]*u[2]
end

u0 = [10.0; 10.0]
tspan = (0.0, 100.0)

prob = ODEProblem(myLV, u0, tspan, params)
sol = solve(prob)

# Static plot of the ODE solution

gr()

odeOutput = plot(sol, title = "Basic Lotka-Volterra Model", 
                 xlabel = "Time", ylabel = "Animal Numbers")

display(odeOutput)

# Phase space of the ODE solution

phasespace = plot(sol, title = "Phase Diagram", vars = (1,2))
display(phasespace)

# Animate the ODE solution

animate(sol)
