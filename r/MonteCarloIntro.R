library(mosaic)

# First, a riskless asset
Horizon = 40
ReturnAvg = 0.05

Wealth = 100
# Sweep through each year and update the value of wealth
for(year in 1:Horizon) {
	ThisReturn = ReturnAvg
	Wealth = Wealth * (1 + ThisReturn)
}
Wealth


# Now a risky asset with a positive expected return
ReturnAvg = 0.05
ReturnSD = 0.05
Horizon = 40

Wealth = 100
# Sweep through each year and update the value of wealth
for(year in 1:Horizon) {
	# Generate a random return
	ThisReturn = rnorm(1, ReturnAvg, ReturnSD)
	
	# Update wealth
	Wealth = Wealth * (1 + ThisReturn)
}
Wealth


# Now a Monte Carlo simulation
ReturnAvg = 0.05
ReturnSD = 0.05
Horizon = 40
sim1 = do(1000)*{
	Wealth = 100
	# Sweep through each year and update the value of wealth
	for(year in 1:Horizon) {
		# Generate a random return
		ThisReturn = rnorm(1, ReturnAvg, ReturnSD)
		
		# Update wealth
		Wealth = Wealth * (1 + ThisReturn)
	}
	# Output the value of wealth for each simulated scenario
	Wealth
}
hist(sim1$result)

# We can easily calculate expected value from the MC simulation.
# Just take a simple average of the simulated futures.
mean(sim1$result)


#  We can also save each year's result in each simulated scenario
ReturnAvg = 0.05
ReturnSD = 0.025
Horizon = 40
sim1 = do(1000)*{
	Wealth = 100
	# Create a placeholder to keep track of your running wealth
	RunningWealth = rep(0, Horizon)
	# Sweep through each year and update the value of wealth
	for(year in 1:Horizon) {
		# Generate a random return
		ThisReturn = rnorm(1, ReturnAvg, ReturnSD)
		
		# Update wealth
		Wealth = Wealth * (1 + ThisReturn)
		
		# Save this year's wealth in the corresponding place in RunningWealth
		RunningWealth[year] = Wealth
	}
	# Output the value of wealth for each simulated scenario
	RunningWealth
}
head(sim1)

# Plot a few simulated scenarios
plot(1:Horizon, sim1[1,], type='l')
lines(1:Horizon, sim1[2,], type='l')
lines(1:Horizon, sim1[3,], type='l')

# Now look at terminal wealth
hist(sim1[,40])
mean(sim1[,40])


# Now we add the wrinkle of a utility function
ReturnAvg = 0.05
ReturnSD = 0.025
Horizon = 40
sim1 = do(1000)*{
	Wealth = 100
	# Sweep through each year and update the value of wealth
	for(year in 1:Horizon) {
		# Generate a random return
		ThisReturn = rnorm(1, ReturnAvg, ReturnSD)
		
		# Update wealth
		Wealth = Wealth * (1 + ThisReturn)
	}
	# Output utility of wealth for this scenario
	log(Wealth)
}
hist(sim1$result)

# Calculated expected utility
mean(sim1$result)

