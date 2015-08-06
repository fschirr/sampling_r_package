# Creating example datasets

# only one year

plots <- rep(1:100)
num.individuals <- sample(0:100, 100, replace = T)
x.coord <- rep(1:10, 10)
y.coord <- rep(1:10, each = 10)
years <- rep(1, each = 100)
species <- rep("Papilio machaon", 100)

Papilio.machaon <- data.frame(plots, x.coord, y.coord, years, num.individuals, 
                              species)#, months, days)

Papilio <- PrepareDataset(Papilio.machaon, 1, 5, 6, 2, 3, 4, 0, 0)

# 30 years

plots <- rep(1:100, 30)
x.coord <- rep(1:10, 300)
y.coord <- rep(1:10, each = 10,30)
years <- rep(0:29, each = 100)
species <- rep("Papilio machaon", 3000)

num.individuals <- NULL

for (j in 1:30) {
  individuals <- rnorm(100, (5000 - j * 125), 2000)
  num.individuals <- c(num.individuals, individuals)
}

num.individuals[num.individuals < 0] <- 0
num.individuals <- round(num.individuals, 0)

Papilio.machaon <- data.frame(plots, x.coord, y.coord, years, num.individuals, 
                              species)#, months, days)

Papilio <- PrepareDataset(Papilio.machaon, 1, 5, 6, 2, 3, 4, 0, 0)

# 30 years and 12 month

plots <- rep(1:100, 192)
x.coord <- rep(1:10, 1920)
y.coord <- rep(1:10, each = 10, 192)
years <- rep(2000:2015, each = 1200)
months <- rep(1:12, each = 100, 16)
species <- rep("Papilio machaon", 19200)
year.distribution <- c(0, 0, 0.1, 2, 2, 2, 4, 4, 1, 0.1, 0, 0)

num.individuals <- NULL

for (j in 1:16) {
  for (i in 1:12) {
    individuals <- sample(0:((500-j*5)*year.distribution[i]), 100, replace=T)
    num.individuals <- c(num.individuals, individuals)
  }
}

Papilio.machaon <- data.frame(plots, x.coord, y.coord, years, num.individuals, 
                              species, months)#, days)

Papilio <- PrepareDataset(Papilio.machaon, 1, 5, 6, 2, 3, 4, 7, 0)

# 30 years, 12 month and 4 days

plots <- rep(1:100, (192*4))
x.coord <- rep(1:10, 7680)
y.coord <- rep(1:10, each = 10, 768)
years <- rep(2000:2015, each= 4800)
months <- rep(1:12, each = 400, 16)
days <- rep(c(1, 8, 16, 22), 192, each =100)
species <- rep("Papilio machaon", 76800)
year.distribution <- c(0, 0, 0.1, 2, 2, 2, 4, 4, 1, 0.1, 0, 0)

num.individuals <- NULL

for (j in 1:16) {
  for (i in 1:12) {
    for (k in 1:4) {
      individuals <- sample(0:((100-j*2)*year.distribution[i]), 100, replace=T)
      num.individuals <- c(num.individuals, individuals)
    }
  }
}

Papilio.machaon <- data.frame(plots, x.coord, y.coord, years, num.individuals, 
                              species, months, days)

Papilio <- PrepareDataset(Papilio.machaon, 1, 5, 6, 2, 3, 4, 7, 8)
