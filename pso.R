cost <- function(x) {
	return (x^2)
	lbest = rep(0, numParticles)
}

pso <- function(numParticles=10, iterations=100, omega=0.9, 
			phip=0.7, phig=0.3, lower=-100, upper=100) {

	particles = rep(0, numParticles)
	veloc = rep(0, numParticles)
	lbest = rep(0, numParticles)
	costlbest = rep(0, numParticles)
	gbest = 0

	for (i in 1:numParticles) {
		particles[i] = runif(min=lower, max=upper, n=1)
		lbest[i] = particles[i]
		costlbest[i] = cost(particles[i])
	}

	id = which.min(costlbest)
	gbest = particles[id]

	for (i in 1:numParticles) {
		veloc[i] = 
			runif(min=-abs(upper-lower), max=abs(upper-lower), n=1)
	}

	for (i in 1:iterations) {

		for (j in 1:numParticles) {
			rp = runif(min=0, max=1, n=1)
			rg = runif(min=0, max=1, n=1)
			veloc[j] = omega * veloc[j] +
				phip * rp * (lbest[j]-particles[j]) +
				phig * rg * (gbest-particles[j])
			particles[j] = particles[j] + veloc[j]

			if (cost(particles[j]) < costlbest[j]) {
				lbest[j] = particles[j]
				costlbest[j] = cost(particles[j])
			}
		}

		id = which.min(costlbest)
		if (particles[id] < cost(gbest)) {
			gbest = particles[id]
		}
	}

	ret = list()
	ret$gbest = gbest

	return (ret)

} 
