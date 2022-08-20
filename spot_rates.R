require(dplyr)
require(ggplot2)

# Below in p_t are the zero-coupon bond prices
# From that, give the spot rates for terms 1 to 5

terms = 1:5
p_t = c(85.35, 80.17, 75.88, 71.03, 66.89)

spot_rate = function(p, t){
  (p_t / 100)**-(1/t) - 1
}

yearly_effective_spot_rates = spot_rate(p_t, terms)


# Compute one-year forward rate where t = 0, 1, 2, 3, 4

one_year_forward = rep(0, 5)
one_year_forward[1] = yearly_effective_spot_rates[1] # One-year forward rate at t=0 is just y_1

for(i in 2:length(terms)){
  one_year_forward[i] = (yearly_effective_spot_rates[i-1] / yearly_effective_spot_rates[i]) - 1
}


one_year_forward


# Given that there is about a 96% probability of survival each year
# And a five year term life annuity due of 500 dollars per year
# Compute the EPV


term_life_annuity = 500
p_survival = 0.96

epv_function = function(p_survival,p, t){
  
  sum_part = (p / 100) * (p_survival**t)
  
}

epv = c()

for(i in 1:length(terms)-1){
  
  epv[i] = sum( epv_function(p_survival, p_t[i], terms[i]) )
  
}

epv = term_life_annuity * (1 + sum(epv) )

epv # Computed at EPV = 1916.42 dollars of the five-term annuity due


