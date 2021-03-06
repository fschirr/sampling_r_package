# Prepare and run simulations

volunteerdata <- CreateEcologist(50, 50, 60, 50, 10)
expertdata <- CreateEcologist(50, 80, 10, 0, 200)

system.time(samplingresult <- Sampling(Papilio, 100, expertdata, volunteerdata,
                                       100, 1, 0, 0, outputall=F))

system.time(evaluationresult <- Evaluation(Papilio, 100, expertdata,
                                           volunteerdata, 0, 1, 0, 0, 
                                           outputall=F, 100, 5))

evaluationresult <- NULL
evaluationresult <- Evaluation(Papilio, 100, expertdata, volunteerdata,
                               20, 1, 0, 0, outputall=F, 10, 5)

volunteer.evalu <-evaluationresult
expert.evalu <-evaluationresult

volunteer.evalu$slope.reduction <- evaluationresult[, 4] / 
  evaluationresult[, 2] * -100 * 30

expert.evalu$slope.reduction <- evaluationresult[, 4] / 
  evaluationresult[, 2] * -100 * 30

evaluationresult$slope.reduction <- evaluationresult[, 4] / 
                                    evaluationresult[, 2] * -100 * 30


dataoutput100

#summaryresult <- summarySE(evaluationresult, measurevar = "num.of.individuals",
#groupvars = c("num.plots","years"))
summaryresult <- summarySE(volunteer.evalu[-1, ],
                           measurevar = "slope.reduction",
                           groupvars = "num.plots")

ggplot(summaryresult, aes(x = num.plots, y = slope.reduction)) + 
  geom_errorbar(aes(ymin = slope.reduction - sd, ymax = slope.reduction + sd),
                width = .1, colour="blue") +
  geom_line(colour = "red") +
  geom_point(size = 3) +
  xlab("Number of plots") +
  ylab("Percent of lost population") + 
  geom_hline(yintercept = volunteer.evalu[1, 7]) +
  coord_cartesian(ylim = c(45, 85)) 


summaryresult <- summarySE(expert.evalu[-1, ],
                           measurevar = "slope.reduction",
                           groupvars = "num.plots")

ggplot(summaryresult, aes(x = num.plots, y = slope.reduction)) + 
  geom_errorbar(aes(ymin = slope.reduction - sd, ymax = slope.reduction + sd),
                width = .1, colour="blue") +
  geom_line(colour = "red") +
  geom_point(size = 3) +
  xlab("Number of plots") +
  ylab("Percent of lost population") + 
  geom_hline(yintercept = expert.evalu[1, 7]) +
  coord_cartesian(ylim = c(45, 85)) 

#plot "truth"
boxplot(num.of.individuals ~ year,data = Papilio)
plot(Papilio$year,Papilio$num.of.individuals, xlab = "Time in years",
     ylab = "Number of individuals") 
reg1 <- lm(Papilio$num.of.individuals ~ Papilio$year)
summary(reg1)
coef(reg1)
abline(reg1)

ggplot(Papilio, aes(x = year, y = num.of.individuals)) + 
  geom_point(size = 3) +
  xlab("Time in years") +
  ylab("Number of individuals") +
  stat_smooth(method = "lm", se = FALSE, size = 2)

#plot sampling

#evaluation

ggplot(summaryresult, aes(x = num.plots, y = slope.reduction)) + 
  geom_errorbar(aes(ymin = slope.reduction - sd, ymax = slope.reduction + sd),
                width = .1, colour="blue") +
  geom_line(colour = "red") +
  geom_point(size = 3) +
  xlab("Number of plots") +
  ylab("Percent of lost population") + 
  geom_hline(yintercept = volunteer.evalu[1, 7]) #+
  coord_cartesian(ylim = c(45, 85)) 

ggplot(evaluationresult, aes(x = num.plots, y = slope.in.percent)) + 
  geom_point() 

ggplot(summaryresult, aes(x = num.plots, y = slope.in.percent)) + 
  geom_errorbar(aes(ymin = slope.in.percent - ci, ymax = slope.in.percent + ci),
                width = .1, colour = "blue") +
  geom_line(colour = "red") +
  geom_point(size = 3) +
  xlab("Number of plots") +
  ylab("Quotient of sample and real slope with 95% confidence interval") #+
#   coord_cartesian(ylim = c(93, 107)) 

ggplot(summaryresult, aes(x = num.plots, y = slope.in.percent)) + 
  geom_errorbar(aes(ymin = slope.in.percent - se, ymax = slope.in.percent + se),
                width = .1, colour="blue") +
  geom_line(colour = "red") +
  geom_point(size = 3) +
  xlab("Number of plots") +
  ylab("Quotient of sample and real slope with 95% confidence interval") #+
#   coord_cartesian(ylim = c(93, 107)) 

ggplot(summaryresult, aes(x = num.plots, y = slope.in.percent)) + 
  geom_errorbar(aes(ymin = slope.in.percent-sd, ymax = slope.in.percent+sd),
                width = .1, colour="blue") +
  geom_line(colour = "red") +
  geom_point(size = 3) +
  xlab("Number of plots") +
  ylab("Quotient of sample and real slope in percent with standard deviation") #+
#   coord_cartesian(ylim = c(90, 110)) 

debug(Evaluation)
undebug(Evaluation)

debug(Sampling)
undebug(Sampling)
