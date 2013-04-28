## in all examples we generate weight_seq only for comparison with existing random_portfolio methods
## our implementation doesn't require it

require("PortfolioAnalytics")


## little number of assets, wide box constraints, just simple illustration
constraints.little.free <- constraint(assets     = 10,
                                      min_mult   = -Inf,
                                      max_mult   = Inf,
                                      min_sum    = .99,
                                      max_sum    = 1.01,
                                      min        = .01,
                                      max        = .4,
                                      weight_seq = generatesequence())

any.portfolio <- FeasiblePortfolio(constraints.little.free)
any.portfolio
CheckConstraint(any.portfolio, constraints.little.free)
random.portfolio <- RandomizePortfolio(constraints.little.free)
random.portfolio
barplot(random.portfolio)
CheckConstraint(random.portfolio, constraints.little.free)

## negative weights, weights sum to zero
constraints.little.negative <- constraint(assets     = 10,
                                          min_mult   = -Inf,
                                          max_mult   = Inf,
                                          min_sum    = -.01,
                                          max_sum    = .01,
                                          min        = -.01,
                                          max        = .4,
                                          weight_seq = generatesequence())

any.portfolio <- FeasiblePortfolio(constraints.little.negative)
CheckConstraint(any.portfolio, constraints.little.negative)
random.portfolio <- RandomizePortfolio(constraints.little.negative)
barplot(random.portfolio, ylim=c(-0.02, 0.1))
CheckConstraint(random.portfolio, constraints.little.negative)


## little number of assets, but constraints are very strict - only one solution exists
constraints.little.tight <- constraint(assets    = 10,
                                      min_mult   = -Inf,
                                      max_mult   = Inf,
                                      min_sum    = 1,
                                      max_sum    = 1,
                                      min        = .1,
                                      max        = .4,
                                      weight_seq = generatesequence())


any.portfolio <- FeasiblePortfolio(constraints.little.tight)
any.portfolio
CheckConstraint(any.portfolio, constraints.little.tight)
random.portfolio <- RandomizePortfolio(constraints.little.tight)
random.portfolio
barplot(random.portfolio)
CheckConstraint(random.portfolio, constraints.little.tight)


## more assets, not strict constraints
constraints.big.free <- constraint(assets     = 1000,
                                   min_mult   = -Inf,
                                   max_mult   = Inf,
                                   min_sum    = .99,
                                   max_sum    = 1.01,
                                   min        = .00001,
                                   max        = .4,
                                   weight_seq = generatesequence(.001, 1))

random.portfolio <- RandomizePortfolio(constraints.big.free)
barplot(random.portfolio)
CheckConstraint(random.portfolio, constraints.big.free)


## constraints without solution
constraints.little.nonexistent <- constraint(assets     = 10,
                                             min_mult   = -Inf,
                                             max_mult   = Inf,
                                             min_sum    = .99,
                                             max_sum    = 1.01,
                                             min        = .11,
                                             max        = .4,
                                             weight_seq = generatesequence(.001, 1))

any.portfolio <- FeasiblePortfolio(constraints.little.nonexistent)
any.portfolio

## performance tests
constraints.middle.free <- constraint(assets     = 100,
                                      min_mult   = -Inf,
                                      max_mult   = Inf,
                                      min_sum    = .99,
                                      max_sum    = 1.01,
                                      min        = .0001,
                                      max        = .4,
                                      weight_seq = generatesequence(.0001, 1))

## takes several seconds
system.time(RandomPortfolios(constraints.middle.free, 1000))

## takes several minutes (old code for comparison)
system.time(random_portfolios(constraints.middle.free, 1000))
