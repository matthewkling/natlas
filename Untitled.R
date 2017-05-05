#Option 2: Analysis of Covarience

library("ggplot2")


oats.intercept <- 100 #arbitrary oat yield
global.residual.sd <- 10
global.oat.effect <- 25
global.moisture.slope <- .5

#####Simulate one oats function ####
simulate.one.oats <- function(n, moisture.slope, oat.effect, residual.sd, oats.intercept){
  
  soil.moisture <- runif(2*n, min = 20, max = 80) #our x values- a uniform ditribution of soil moisture from 20 to 80 %
  oat.type = factor(c(rep("oat1", n), rep("oat2", n))) #vector naming the oat type
  oat.effect.vector <- c(rep(0, n), rep(1, n)) #vector of oat type effects
  yield <- oats.intercept + soil.moisture*moisture.slope + oat.effect*oat.effect.vector + rnorm(n, 0, residual.sd) #simulates data
  
  n.replicates <- c(rep(n, n)) # makes vector the length of n, of value n- to keep track of number of replicates when this function is iterated over

  oat.data <- data.frame(yield = yield, soil.moisture = soil.moisture, oat.type = oat.type, n.replicates = n.replicates) #makes dataframe
  
  return(oat.data)
}



#Simulation scatterplot
scatter.n = 500
scatter.moisture.slope = global.moisture.slope; 
scatter.oat.effect = global.oat.effect; 
scatter.residual.sd = global.residual.sd; 
oats.data <- simulate.one.oats(n = scatter.n, moisture.slope = scatter.moisture.slope, oat.effect = scatter.oat.effect, residual.sd = scatter.residual.sd, oats.intercept = oats.intercept)
qplot(oats.data$soil.moisture, oats.data$yield, data = oats.data, 
      xlab = "% Soil Moisture", ylab = "Oat Yield",
      color = oat.type) + labs(title = "Oat Yield", subtitle = paste0("n=",scatter.n,", moisture.slope=", scatter.moisture.slope, ", oat.effect=", scatter.oat.effect, "\noats.intercept=", oats.intercept, ", residual.sd=", scatter.residual.sd), color = "Oat Type") +  theme_bw()


####Fit many simulations####
fit.many.simulations <- function(number.of.replicates, iterations, oat.effect, moisture.slope, residual.sd)
{
  multiple.sims.dataframe <- data.frame() #make dataframe to put results in
  
  for (n in number.of.replicates){ #runs for each number of replicates
    count <- 0
    
    while (count <= iterations){ #runs simulation, fits model iterations times. 
      oats.data <- simulate.one.oats(n = n, moisture.slope = moisture.slope, oat.effect = oat.effect, residual.sd = residual.sd, oats.intercept = oats.intercept)
      
      fit <- lm(yield ~ soil.moisture + oat.type + soil.moisture*oat.type ,oats.data)
      
      multiple.sims.dataframe <- rbind(multiple.sims.dataframe,  #extracts model results, binds into dataframe
                                       cbind(rbind(fit$coefficients),n, 
                                             oat.p.value = summary(fit)$coefficients["oat.typeoat2", "Pr(>|t|)"], 
                                             true.oat.effect = oat.effect,
                                             moisture.p.value = summary(fit)$coefficients["soil.moisture", "Pr(>|t|)"], 
                                             true.moisture.slope = moisture.slope
                                             ))
      count <- count + 1
    }
  }
  multiple.sims.dataframe$n <- factor(multiple.sims.dataframe$n)
  return(multiple.sims.dataframe)
}



####Boxplot of replicates and estimated oat effect####
boxplot.iterations <- 500
boxplot.residual.sd <- global.residual.sd
boxplot.moisture.slope <- global.moisture.slope
boxplot.oat.effect <- global.oat.effect
many.oats <- fit.many.simulations(number.of.replicates = c(3,5,10,25,50,100,500), iterations = boxplot.iterations, oat.effect = boxplot.oat.effect, moisture.slope = boxplot.moisture.slope, residual.sd = boxplot.residual.sd)

ggplot(many.oats, aes(x = n, y=oat.typeoat2, fill=n)) + geom_boxplot() + geom_hline(yintercept = boxplot.oat.effect, linetype = 2, color = "red") + theme_bw() + xlab("Number of replicates") + ylab("Estimated Oat Effect")  + labs(title= "Estimated oat effect", subtitle = paste0("True effect = ", many.oats$true.oat.effect[1], ", ", boxplot.iterations, " simulations each"), fill = "# plots")

plot.many.oats <- many.oats[-which(many.oats$n == 3),]
ggplot(plot.many.oats, aes(x = n, y=oat.typeoat2, fill=n)) + geom_boxplot() + geom_hline(yintercept = boxplot.oat.effect, linetype = 2, color = "red") + theme_bw() + xlab("Number of replicates") + ylab("Estimated Oat Effect")  + labs(title= "Estimated oat effect", subtitle = paste0("True effect = ", many.oats$true.oat.effect[1], ", ", boxplot.iterations, " simulations each"), fill = "# plots")


#####Determine power function####
determine.power <- function(number.of.replicates, iterations, oat.effects, moisture.slopes, residual.sd){
  power.data <- data.frame()
  
  for (m in moisture.slopes){
    for(n in number.of.replicates){
      for (o in oat.effects){
        simulated <- fit.many.simulations(number.of.replicates = n, iterations = iterations, oat.effect = o, moisture.slope = m, residual.sd = residual.sd)
        oat.power <- mean(simulated$oat.p.value < .05)
        moisture.power <- mean(simulated$moisture.p.value < .05)
        power.data <- rbind(power.data, cbind(replicates = n, true.oat.effect = o, oat.power = oat.power, moisture.power = moisture.power,  true.moisture.effect = m))
      }
    }
  }
  power.data$replicates <- factor(power.data$replicates)
  return(power.data)
}

power.replicates <- c(3,5,10,25,50,100)
power.iterations <- 500
power.residual.sd <- global.residual.sd
#Plot oat power
oat.powers <- determine.power(number.of.replicates = power.replicates, iterations = power.iterations, oat.effects <- seq(0, 100, by = 5), moisture.slopes = global.moisture.slope, residual.sd = power.residual.sd)
qplot(oat.powers$true.oat.effect, oat.powers$oat.power, data = oat.powers, 
      xlab = "True Oat Effect", ylab = "Power",
      color = replicates) + labs(title = "Power for Oat Effect", color = "Replicates") +  theme_bw()

#Plot moisture power
moisture.powers <- determine.power(number.of.replicates = power.replicates, iterations = power.iterations, oat.effects <- global.oat.effect, moisture.slopes = seq(0, 2, by = .2), residual.sd = power.residual.sd)
qplot(moisture.powers$true.moisture.effect, moisture.powers$moisture.power, data = moisture.powers, 
      xlab = "True Moisture Effect", ylab = "Power",
      color = replicates) + labs(title = "Power for Moisture Levels", color = "Replicates") +  theme_bw()

####Interactions####
simulate.with.interaction <- function(n, moisture.slope, oat.effect, residual.sd, oats.intercept, interaction.strength){
  
  soil.moisture <- runif(2*n, min = 20, max = 80) #our x values- a uniform ditribution of soil moisture from 20 to 80 %
  oat.type = factor(c(rep("oat1", n), rep("oat2", n))) #vector naming the oat type
  oat.effect.vector <- c(rep(0, n), rep(1, n)) #vector of oat type effects
  
  interaction.effect.vector <- c(rep(interaction.strength, n), rep(-interaction.strength, n)) #vector of sum to 0 interaction effects
  
  yield <- oats.intercept + soil.moisture*moisture.slope + oat.effect*oat.effect.vector + rnorm(n, 0, residual.sd) + interaction.effect.vector*soil.moisture  #simulates data
  
  n.replicates <- c(rep(n, n)) # makes vector the length of n, of value n- to keep track of number of replicates when this function is iterated over
  
  oat.data <- data.frame(yield = yield, soil.moisture = soil.moisture, oat.type = oat.type, n.replicates = n.replicates) #makes dataframe
  
  return(oat.data)
}
interaction.replicates <- 1000000
interaction.moisture.slope <- global.moisture.slope
interaction.oat.effect <- global.oat.effect
interaction.residual.sd <- global.residual.sd

interaction.simulation <- simulate.with.interaction(n = interaction.replicates, moisture.slope = interaction.moisture.slope, oat.effect = interaction.oat.effect, residual.sd =interaction.residual.sd, oats.intercept, interaction.strength = .05)
fit <- lm(yield ~ soil.moisture + oat.type + soil.moisture*oat.type ,interaction.simulation);summary(fit)


qplot(interaction.simulation$soil.moisture, interaction.simulation$yield, data = interaction.simulation, 
      xlab = "% Soil Moisture", ylab = "Oat Yield",
      color = oat.type) + labs(title = "Oat Yield With Interactions", subtitle = paste0("n=",interaction.replicates,", moisture.slope=", interaction.moisture.slope, ", oat.effect=", interaction.oat.effect, "\noats.intercept=", oats.intercept, ", residual.sd=", interaction.residual.sd), color = "Oat Type") +  theme_bw()




