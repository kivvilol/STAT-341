library(regclass)

data(SALARY)

# initial models
Naive <- lm(Salary~1, data = SALARY)
Intitial <- lm(Salary ~ Experience, data = SALARY)
Complex = lm(Salary ~ .^2, data = SALARY)

# forward selection
result <- step(Intitial, scope = list(lower = Naive, upper = Complex), direction = 'forward')

# backward selection
result <- step(Complex, scope = list(lower = Naive, upper = Complex), direction = 'backward')

