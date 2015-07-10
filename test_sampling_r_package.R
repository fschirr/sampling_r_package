plots <- rep(1:100,30)
#num.individuals <- sample(0:100,160,replace=T)
x.coord <- rep(1:10,300)
y.coord <- rep(1:10,each = 10,30)
years <- rep(2000:2029, each=100)

species <- rep("Papilio machaon",3000)

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
num.individuals <-0
for (j in 1:30){
    individuals <- rnorm(100,(100-j*2),25-j*0.5)#sample(0:((100-j*2)),100,replace=T)
    num.individuals <- c(num.individuals,individuals)
}
num.individuals <- num.individuals[-1]
num.individuals[num.individuals < 0] <- 0
num.individuals <- round(num.individuals,0)

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
                              species)#, months, days)

Papilio <- PrepareDataset(Papilio.machaon,1,5,6,2,3,4,0,0)
boxplot(num.of.individuals ~ year,data=Papilio)
plot(Papilio$year,Papilio$num.of.individuals)
reg1 <- lm(Papilio$num.of.individuals ~ Papilio$year)
summary(reg1)
coef(reg1)
abline(reg1)
reg2 <- lm(samplingresult$num.of.individuals ~ samplingresult$year)
summary(reg2)
coef(reg2)
tap1 <- tapply(samplingresult[,2],samplingresult[,6],mean)

ggplot(summaryresult, aes(x=year, y=num.of.individuals)) + 
  geom_errorbar(aes(ymin=num.of.individuals-sd, ymax=num.of.individuals+sd), width=.2) +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 4094.269194, slope =-1.998118, colour ="red") +
  geom_abline(intercept = 3508.683571, slope =-1.708214, colour ="green")
 

Mittelwert <- tapply(Papilio[,2],Papilio[,6],mean)

Standardfehler <- tapply(Papilio[,2],Papilio[,6],sd)
Jahr <- c(2000:2029)
MittelStandard <- data.frame(Mittelwert,Standardfehler,Jahr)

plot(c(1:30),Mittelwert)
plot(c(1:30),BeidesNegativ,add=T)
plot(tapply(samplingresult[,2],samplingresult[,6],mean))

volunteerdata <- CreateEcologist(100,100,0,0,10)
expertdata <- CreateEcologist(100,100,0,0,200)

samplingresult <- Sampling(Papilio, 10, expertdata, volunteerdata, 10, 1, 0, 0, 
                            outputall=F)
samplingresult

summaryresult <- summarySE(samplingresult, measurevar = "num.of.individuals", groupvars = "year")

debug(Sampling)
undebug(Sampling)
