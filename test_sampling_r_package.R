plots <- rep(1:100,16)
#num.individuals <- sample(0:100,160,replace=T)
x.coord <- rep(1:10,160)
y.coord <- rep(1:10,each = 10,16)
years <- rep(2000:2015, each=100)
#month <- rep(1:12,each = 100,16)
species <- rep("Papilio machaon",1600)

plots <- rep(1:100, 192)
#num.individuals <- sample(0:100,160,replace=T)
x.coord <- rep(1:10,1920)
y.coord <- rep(1:10,each = 10,192)
years <- rep(2000:2015, each=1200)
months <- rep(1:12,each = 100,16)

species <- rep("Papilio machaon",19200)
num.individuals <-0

plots <- rep(1:100, (192*4))
#num.individuals <- sample(0:100,160,replace=T)
x.coord <- rep(1:10, 7680)
y.coord <- rep(1:10, each = 10,768)
years <- rep(2000:2015, each= 4800)
months <- rep(1:12,each = 400,16)
species <- rep("Papilio machaon", 76800)
num.individuals <-0
days <- rep(c(1,8,16,22),192,each =100)

year.distribution <- c(0,0,0.1,2,2,2,4,4,1,0.1,0,0)

for (j in 1:16){
    individuals <- sample(0:((100-j*2)),100,replace=T)
    num.individuals <- c(num.individuals,individuals)
}

for (j in 1:16){
  for (i in 1:12) {
    individuals <- sample(0:((100-j*2)*year.distribution[i]),100,replace=T)
    num.individuals <- c(num.individuals,individuals)
  }
}

for (j in 1:16){
  for (i in 1:12) {
    for (k in 1:4) {
    individuals <- sample(0:((100-j*2)*year.distribution[i]),100,replace=T)
    num.individuals <- c(num.individuals,individuals)
    }
  }
}

num.individuals <- num.individuals[-1]

Papilio.machaon <- data.frame(plots, x.coord, y.coord, years, num.individuals, 
                              species, months, days)

Papilio <- PrepareDataset(Papilio.machaon,1,5,6,2,3,4,7,8)

volunteerdata <- CreateEcologist(50,70,50,20,10)
expertdata <- CreateEcologist(50,75,10,5,200)

samplingresult <- Sampling(Papilio, 10, expertdata, volunteerdata, 2, 1, 1, 1, 
                           outputall=F)
samplingresult

debug(Sampling)
undebug(Sampling)
