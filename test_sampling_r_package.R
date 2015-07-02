plots <- rep(1:100,16)
#num.individuals <- sample(0:100,160,replace=T)
x.coord <- rep(1:10,160)
y.coord <- rep(1:10,each = 10,16)
years <- rep(2000:2015, each=100)
#month <- rep(1:12,each = 100,16)
species <- rep("Papilio machaon",1600)

num.individuals <-0

year.distribution <- c(0,0,1,2,4,4,5,4,1,0.5,0,0)

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
num.individuals <- num.individuals[-1]

Papilio.machaon <- data.frame(plots,x.coord,y.coord,years,num.individuals,species)

Papilio <- PrepareDataset(Papilio.machaon,1,5,6,2,3,4,0,0)

volunteerdata <- CreateEcologist(50,70,50,20,10)
expertdata <- CreateEcologist(50,75,10,5,200)

samplingresult <- Sampling(Papilio,10,expertdata,volunteerdata,2,1,0,0)
samplingresult
rm(samplingresult)

debug(Sampling)
undebug(Sampling)

#Error by using one plot and one expert