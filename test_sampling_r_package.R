
plots <- rep(1:100)
num.individuals <- sample(0:100,100,replace=T)
x.coord <- rep(1:10,10)
y.coord <- rep(1:10,each = 10)
years <- rep(1, each=100)
species <- rep("Papilio machaon",100)
#year

plots <- rep(1:100,30)
#num.individuals <- sample(0:100,160,replace=T)
x.coord <- rep(1:10,300)
y.coord <- rep(1:10,each = 10,30)
years <- rep(0:29, each=100)

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
num.individuals <-NULL
days <- rep(c(1,8,16,22),192,each =100)

year.distribution <- c(0,0,0.1,2,2,2,4,4,1,0.1,0,0)
num.individuals <-NULL
for (j in 1:30){
    individuals <- rnorm(100,(5000-j*125),400)
    #individuals <- sample(0:((1000-j*10)),100,replace=T)
    num.individuals <- c(num.individuals,individuals)
}
# num.individuals <- num.individuals[-1]
num.individuals[num.individuals < 0] <- 0
num.individuals <- round(num.individuals,0)

for (j in 1:16){
  for (i in 1:12) {
    individuals <- sample(0:((500-j*5)*year.distribution[i]),100,replace=T)
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

Papilio.machaon <- data.frame(plots, x.coord, y.coord, years, num.individuals, 
                              species)#, months, days)

Papilio <- PrepareDataset(Papilio.machaon,1,5,6,2,3,4,0,0)

boxplot(num.of.individuals ~ year,data=Papilio)
plot(Papilio$year,Papilio$num.of.individuals)
reg1 <- lm(Papilio$num.of.individuals ~ Papilio$year)
summary(reg1)
coef(reg1)
abline(reg1)

volunteerdata <- CreateEcologist(100,100,0,0,10)
expertdata <- CreateEcologist(100,100,0,0,200)

samplingresult <- Sampling(Papilio, 2, expertdata, volunteerdata, 2, 1, 0, 0, 
                           outputall=F)

evaluationresult <- Evaluation(Papilio, 100, expertdata, volunteerdata, 0, 1, 0, 0, 
                               outputall=F, 20)
evaluationresult
#summaryresult <- summarySE(evaluationresult, measurevar = "num.of.individuals", groupvars = c("num.plots","years"))
summaryresult <- summarySE(evaluationresult[-1,], measurevar = "slope.in.percent", groupvars = "num.plots")

ggplot(evaluationresult, aes(x=num.plots, y=slope.in.percent)) + 
  geom_point() 

ggplot(summaryresult, aes(x=num.plots, y=slope.in.percent)) + 
  geom_errorbar(aes(ymin=slope.in.percent-ci, ymax=slope.in.percent+ci), width=.1, colour="blue") +
  geom_line(colour="red") +
  geom_point(size=3)

ggplot(summaryresult, aes(x=num.plots, y=slope.in.percent)) + 
  geom_errorbar(aes(ymin=slope.in.percent-se, ymax=slope.in.percent+se), width=.1, colour="blue") +
  geom_line(colour="red") +
  geom_point(size=3)

ggplot(summaryresult, aes(x=num.plots, y=slope.in.percent)) + 
  geom_errorbar(aes(ymin=slope.in.percent-sd, ymax=slope.in.percent+sd), width=.1, colour="blue") +
  geom_line(colour="red") +
  geom_point(size=3)

debug(Evaluation)
undebug(Evaluation)

debug(Sampling)
undebug(Sampling)
