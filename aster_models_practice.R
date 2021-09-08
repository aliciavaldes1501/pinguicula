#### Example ####

library(aster)
data(echinacea)
names(echinacea)
sapply(echinacea, class)

# Reshaping data

vars <- c("ld02", "ld03", "ld04", "fl02", "fl03", "fl04",
          "hdct02", "hdct03", "hdct04")
redata <- reshape(echinacea, varying = list(vars),
                  direction = "long", timevar = "varb",
                  times = as.factor(vars), v.names = "resp")
redata <- data.frame(redata, root = 1)

names(echinacea)
names(redata)
nrow(echinacea)
nrow(redata)
nrow(echinacea) * length(vars)
sapply(redata, class)
levels(redata$varb)
length(unique(redata$id))

# Setting up the graphical model

pred <- c(0, 1, 2, 1, 2, 3, 4, 5, 6)
# The predecessor of the first element of vars which is ld02 is root, 
# because zero in the pred vector is the code for root, 
# and the corresponding root value is the constant 1 
# (because we set all elements of the root variable in our data frame to be 1),
# hence this is really an unconditional distribution because it is constant.
# The predecessor of the second element of vars which is ld03 
# is the first element of vars which is ld02, 
# because the second element of the pred vector is 1 
# indicating the first element of the vars vector. lAnd so forth.

fam <- c(1, 1, 1, 1, 1, 1, 3, 3, 3)
# All the ld0x and fl0x variables are conditionally Bernoulli 
# given their predecessors. 
# This is because the corresponding element of family is 1, 
# which is the default code for Bernoulli. 
# This default can be changed, if necessary.
# The hdct0x variables are conditionally zero-truncated Poisson 
# given their predecessors. (Same explanation).

vars[fam == 1]
vars[fam == 3]
fam.default()

foo <- c("root", vars)
pvars <- foo[pred + 1]
bar <- cbind(pvars, vars)
colnames(bar) <- c("pred", "succ")
bar

# Fitting Aster Models

# Little model

level <- gsub("[0-9]", "", as.character(redata$varb))
redata <- data.frame(redata, level = as.factor(level))

levels(redata$level)

# Level indicates the layer of the graph, or the kind of variable 
# (mortality indicator, flowering indicator, head count indicator) 
# of each component of the response.

# varb, is a factor that indicates which original variable (which node of the graph) 
# an element of the response corresponds to. 
# This is tantamount to having a different intercept for each node of the graph. 
# This makes sense because they are different kinds of variables 
# having nothing to do with each other 
# (except for being measurements on the same individual).

# level:(nsloc + ewloc) is the interaction of level, 
# which is a factor that indicates which layer of the graph 
# an element of the response corresponds.
# nsloc, which is the north-south location of the individual
# ewloc, which is the east-west location of the individual

# The parentheses in the interaction just group. 
# The term level:(nsloc + ewloc) indicates the same thing as the two terms 
# level:nsloc + level:ewloc.

# The interaction of a factor and a numerical variable (like these) 
# is the same thing as saying the linear predictor depends linearly on nsloc and ewloc 
# and the linear relationship has a different slope in each layer 
# (and also a different intercept because varb is in the model).

out1 <- aster(resp ~ varb + level:(nsloc + ewloc),
              pred, fam, varb, id, root, data = redata)
summary(out1, show.graph = TRUE)

# Middle model

hdct <- grep("hdct", as.character(redata$varb)) # hdct = indicator of the head count "level"
hdct <- is.element(seq(along = redata$varb), hdct)
redata <- data.frame(redata, hdct = as.integer(hdct))

class(redata$hdct)
unique(redata$hdct)

out2 <- aster(resp ~ varb + level:(nsloc + ewloc) +
                hdct:pop, pred, fam, varb, id, root, data = redata)
summary(out2, show.graph = TRUE)

# The variable pop is scientifically important. 
# It is the prairie remnant from which the individual came. 
# All of the individuals were grown in a common garden, 
# but grown from seeds that came from different remnants. 
# The question of interest is whether population of origin 
# has an effect on fitness. 
# Because we are fitting unconditional aster models (the default), 
# we want to put pop in the model only at the bottom layer 
# of the graphical model, at the head count nodes, 
# the sum of which is observed fitness 
# (pedantically, the best available surrogate or proxy for fitness). 

# Tests of Model Comparison

anova(out1, out2)

# The larger model fits considerably better than the smaller one. 
# Hence population has a statistically significant effect on fitness (P = 0.01).

#### Our data ####

# Reshaping data

vars <- c("fitness_01", "n_seeds")
redata <- reshape(data.frame(subset(ping_20_data,
                                    !is.na(n_seeds)&!is.na(ros_area_log))), 
                  varying = list(vars),
                  direction = "long", timevar = "varb",
                  times = as.factor(vars), v.names = "resp")
redata <- data.frame(redata, root = 1)

names(ping_20_data)
names(redata)
nrow(ping_20_data)
nrow(redata)
nrow(ping_20_data) * length(vars)
sapply(redata, class)
levels(redata$varb)
length(unique(redata$id))

# Setting up the graphical model

pred <- c(0, 1)

fam <- c(1, 3)

vars[fam == 1]
vars[fam == 3]
fam.default()

foo <- c("root", vars)
pvars <- foo[pred + 1]
bar <- cbind(pvars, vars)
colnames(bar) <- c("pred", "succ")
bar

# Fitting Aster Models

# redata <- data.frame(redata, level = as.factor(ifelse(redata$varb=="fitness_01",
#                                                       "sd","nsd")))
# 
# levels(redata$level) 

# Use Poisson

# fit using MASS fitdistr function
frt.param <- fitdistr(subset(ping_20_data, n_seeds > 0)$n_seeds, "poisson")
frt.param$estimate

fam <- c(1,2)
famlist <- list(fam.bernoulli(), 
                fam.truncated.poisson(truncation = 0))

# Center predictors
redata$temp_c<-scale(redata$temp,center=T,scale=F)
redata$FFD_c<-scale(redata$FFD_corr,center=T,scale=F)
redata$ros_area_c<-scale(log(redata$ros_area),center=T,scale=F)



model1 <- aster(resp ~ varb+temp_c*FFD_c*P*F+ros_area_c,
                pred, fam, famlist=famlist, varb, id, root, data = redata)
summary(model1, show.graph = TRUE, info.tol = 1e-11)




# Use negative binomial

# fit NegBinom size and mu using MASS fitdistr function
frt.param <- fitdistr(subset(ping_20_data, n_seeds > 0)$n_seeds, "negative binomial")
frt.param$estimate

# what does the observed distribution look like
hist(subset(ping_20_data, n_seeds > 0)$n_seeds)

# how about a simulated negative binomial distribution with the estimated parameters? 
# !Note! This is not zero-truncated
hist(rnbinom(130, size = 5.882334, mu = 234.069231))
# hist(rpois(130, lambda = 234.069231))


# Negative Binomial seems to reflect the observed data better; set size parameter
frt.alpha <- round(frt.param$estimate[1],2)

# assign distribution families to life history stages
fam <- c(1,2)
famlist <- list(fam.bernoulli(), 
                fam.truncated.negative.binomial(truncation = 0, size = frt.alpha))

model2 <- aster(resp ~ temp*FFD_corr_std*P+ros_area_std,
                pred, fam, famlist=famlist, varb, id, root, data = redata)
summary(model2, show.graph = TRUE)

model2 <- aster(resp ~ temp*FFD_corr_std*P+ros_area_std,
                pred, fam, famlist=famlist, varb, id, root, data = redata)
summary(model2, show.graph = TRUE)

# anova(model1,model2) # It is not OK to compare models with different families!

# Use normal

# what does the observed distribution look like --> actually looks normal!
hist(subset(ping_20_data, n_seeds > 0)$n_seeds)

# fit normal sd using MASS fitdistr function
frt.param <- fitdistr(subset(ping_20_data, n_seeds > 0)$n_seeds, "normal")
frt.param$estimate

# how about a simulated normal distribution with the estimated parameters? 
hist(rnorm(130, mean = 234.069231, sd = 100.808057))

fam <- c(1,2)
famlist <- list(fam.bernoulli(), 
                fam.normal.location(sd=100.808057))

model4 <- aster(resp ~ temp*FFD_corr_std*P+ros_area_std,
                pred, fam, famlist=famlist, varb, id, root, data = redata)
summary(model4, show.graph = TRUE)

model5 <- aster(resp ~ temp*FFD_corr_std*P*F+ros_area_std,
                pred, fam, famlist=famlist, varb, id, root, data = redata)
summary(model5, show.graph = TRUE)

# Detail all the terms to make it easier to remove terms

model5 <- aster(resp ~ temp+FFD_corr_std+P+F+ros_area_std+
                  temp:FFD_corr_std+temp:P+FFD_corr_std:P+temp:F+
                  FFD_corr_std:F+P:F+temp:FFD_corr_std:P+temp:FFD_corr_std:F+
                  temp:P:F+FFD_corr_std:P:F+temp:FFD_corr_std:P:F,
                pred, fam, famlist=famlist, varb, id, root, data = redata)
summary(model5, show.graph = TRUE) # Our final model?

# LR tests

# Remove temp:FFD_corr_std:P
model6 <- aster(resp ~ temp+FFD_corr_std+P+F+ros_area_std+
                  temp:FFD_corr_std+temp:P+FFD_corr_std:P+temp:F+
                  FFD_corr_std:F+P:F+temp:FFD_corr_std:F+
                  temp:P:F+FFD_corr_std:P:F+temp:FFD_corr_std:P:F,
                pred, fam, famlist=famlist, varb, id, root, data = redata)

anova(model6,model5) # LR test to assess significance of temp:FFD_corr_std:P (*)

# Remove temp:FFD_corr_std:P:F
model7 <- aster(resp ~ temp+FFD_corr_std+P+F+ros_area_std+
                  temp:FFD_corr_std+temp:P+FFD_corr_std:P+temp:F+
                  FFD_corr_std:F+P:F+temp:FFD_corr_std:P+temp:FFD_corr_std:F+
                  temp:P:F+FFD_corr_std:P:F,
                pred, fam, famlist=famlist, varb, id, root, data = redata)
summary(model7, show.graph = TRUE)

anova(model7,model5) # LR test to assess significance of temp:FFD_corr_std:P:F (NS)

# Repeat for each term...

# Prediction

conf.level <- 0.95
crit <- qnorm((1 + conf.level) / 2)
fred <- summary(model5)
dimnames(fred$coef)

fred$coef["temp:FFD_corr_std:P1", "Estimate"] + 
  c(-1, 1) * crit * fred$coef["temp:FFD_corr_std:P1", "Std. Error"]

# This gives an asymptotic (large sample) 95% confidence interval for the
# unknown true regression coefficient for the dummy variable temp:FFD_corr_std:P1
# (assuming this model is correct!)

predict(model2, x, root, modmat, amat,
        parm.type = c("mean.value", "canonical"),
        model.type = c("unconditional", "conditional"),
        is.always.parameter = FALSE,
        se.fit = FALSE, info = c("expected", "observed"),
        info.tol = sqrt(.Machine$double.eps), newcoef = NULL,
        gradient = se.fit, ...)


