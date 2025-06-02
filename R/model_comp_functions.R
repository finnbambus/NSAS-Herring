
herring <- read.csv("SA_herring_2021.csv",
                    sep = ",")
herring <- arrange(herring, SSB)
#herring <- arrange(herring, SSB)
color_regimes_herring <- NULL
color_regimes_herring[herring$Year %in% c(1947:1966)] <- "steelblue3"
color_regimes_herring[herring$Year %in% c(1966:1983)] <- "darkorange"
color_regimes_herring[herring$Year %in% c(1983:2021)] <- "purple"
herring$color_regimes_herring <- color_regimes_herring



RR_herring <- ggplot(data = herring)+
  geom_point((aes(x = SSB/1000, y = R_0/1000000)),col = color_regimes_herring)+
  theme_test()+
  labs(title = "Herring")
SRR_herring


ns_data <- herring
ns_data$R <- ns_data$R_0
ns_data$ssb <- ns_data$SSB

#1.1. Indipendence Model ----
m1 <- lm(ns_data$R~ns_data$ssb)
summary(m1)
fitI <- fitted(m1)
#high overdispersion

#1.2. Beverton Holt ----
svR <- srStarts(R ~ ssb, data=ns_data, type="BevertonHolt")
svR
###stock-recruitement function (NB, the function is log based!)
bh <- srFuns("BevertonHolt")
srR_beverton <- nls(log(R)~log(bh(ssb,a,b)), data=ns_data, start=svR)


##results tell you significance of parameters and residual standard error
#resisual standard error is the square root of the residual sum of squares
#divided by degrees of freedom
summary(srR_beverton)
cbind(estimates=coef(srR_beverton), confint(srR_beverton))
### make predictions to then plot!
pR_beverton <- bh(ns_data$ssb, a=coef(srR_beverton))
ns_data$pR_beverton <- pR_beverton
###quasi-r2 value, if low model does not fit well!
cor(bh(ns_data$ssb, a=coef(srR_beverton)), ns_data$R)^2

#1.3. Ricker ----
svR <- srStarts(R ~ ssb, data=ns_data, type="Ricker")
svR
##fit the Ricker function to data
rckr <- srFuns("Ricker")
srR_ricker <- nls(log(R)~log(rckr(ssb,a,b)), data=ns_data, start=svR)
#for a and b
cbind(estimates=coef(srR_ricker),confint(srR_ricker))
###prediction
#plot
pR_ricker <- rckr(ns_data$ssb, a=coef(srR_ricker))
ns_data$pR_ricker <- pR_ricker

#1.4. Segmented ----
mean(herring$SSB, na.rm = T) #1930650
seg_herring <- segmented::segmented(lm(R_0 ~SSB, data = herring), seg.Z =  ~SSB, psi = mean(herring$SSB, na.rm = T))
summary(seg_herring)
summary(seg_herring)$psi
coef_herring <-c(seg_herring$fitted.values)
brpt_herring <- seg_herring$psi[2]
brpt_ste_herring <- seg_herring$psi[3]

#1.5. Segmented log ----
herring$r_log <- log(herring$R_0)
herring$SSB_lag_log <- log(herring$SSB)
#View(herring)

mean(herring$SSB_lag_log, na.rm = T) #14.01547
seg_herring_log <- segmented::segmented(lm(r_log ~ SSB_lag_log, data = herring), seg.Z =  ~ SSB_lag_log, psi = mean(herring$SSB_lag_log, na.rm = T))
summary(seg_herring_log)
summary(seg_herring_log)$psi

coef_herring_log <-c(seg_herring_log$fitted.values)
brpt_herring_log <- seg_herring_log$psi[2]
brpt_ste_herring_log <- seg_herring_log$psi[3]

#1.6. Segmented with best performing GLM ----
#1.6.1. glm with gaussian
herring_gaus <- glm(R_0 ~ SSB, data = herring, family = gaussian)
summary(herring_gaus)
1.6091e+16/73 #2.204247e+14, clear overdispersion
par(mfrow = c(2,2))
plot(herring_gaus)
# bad model fit, dont necessary to visualize

#1.6.2. glm with poisson
herring_pois <- glm(R_0 ~ SSB, data = herring, family = poisson)
summary(herring_pois)
569510983/73 #7801520, clear overdispersion

plot(herring_pois)
#better, but overdispersed

#1.6.3. glm with quasipoisson
herring_qpois <- glm(R_0 ~ SSB, data = herring, family = quasipoisson)
summary(herring_qpois)

plot(herring_qpois)
569510983/73#569510983
#not better modelfit, but no overdispersion

#1.6.4. glm with negative binomial

herring_negbi <- glm.nb(R_0 ~ SSB, data = herring)
summary(herring_negbi)
78.842/73 #1.080027 -> no over or underdispersion

plot(herring_negbi)
#slightly better than the quasipoisson model -> use negative binomial for the segmented analysis

mean(herring$SSB, na.rm = T)
seg_herring_negbi <- segmented::segmented(glm.nb(R_0 ~SSB, data = herring), seg.Z =  ~SSB, psi = mean(herring$SSB, na.rm = T))
summary(seg_herring_negbi)
summary(seg_herring_negbi)$psi

coef_herring_negbi <-c(seg_herring_negbi$fitted.values)
brpt_herring_negbi <- seg_herring_negbi$psi[2]
brpt_ste_herring_negbi <- seg_herring_negbi$psi[3]

#1.7. Strucchange ----
bpts <- strucchange :: breakpoints(R_0 ~ SSB, data = herring)


plot(bpts)
summary(bpts)


opt_bpts <- function(x) {
  #x = bpts_sum$RSS["BIC",]
  n <- length(x)
  lowest <- vector("logical", length = n-1)
  lowest[1] <- FALSE
  for (i in 2:n) {
    lowest[i] <- x[i] < x[i-1] & x[i] < x[i+1]
  }
  out <- as.integer(names(x)[lowest])
  return(out)
}
bpts_sum <- summary(bpts)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks #1
bpts2 <-strucchange :: breakpoints(bpts, breaks = opt_brks)
best_brk <- herring$SSB[bpts2$breakpoints]

best_brk #1202006

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(R_0 ~ SSB, data = herring, type = "p")
for (i in 1: opt_brks) {
  abline(v = haddock$SSB[ci_mod$confint[i,2]], col = "blue")
  abline(v = haddock$SSB[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = haddock$SSB[ci_mod$confint[i,3]], col = "red", lty = 3)
}


## fit null hypothesis model
fm0 <- lm(R_0 ~ SSB, data = herring)
# fit model with 1 breakpoint but formula different then in previous time series:



######cannot find "best_brk"
strucc_herring <- lm(R_0 ~ SSB*(SSB < best_brk) + SSB*(SSB > best_brk), data = herring)
fm1_coef <- coef(strucc_herring)

fit_strucc <- fitted(strucc_herring)

fit1 <- (fm1_coef[1] + fm1_coef[3]) + (fm1_coef[2] + fm1_coef[5])*herring$SSB[herring$SSB <= best_brk]
fit2 <- (fm1_coef[1] + fm1_coef[4]) + (fm1_coef[2])*herring$SSB[herring$SSB>= best_brk]

# add to previous plot
lines(herring$SSB, fitted(fm0), col = 3)
lines(herring$SSB[herring$SSB <= best_brk], fit1, col = "orange")
lines(herring$SSB[herring$SSB >= best_brk], fit2, col = "orange")

#1.8. Visualize and compare with RMSE ----
SRR_herring_models <- SRR_herring +
  geom_vline(aes(col = "seg. neg bi",xintercept = brpt_herring_negbi/1000), linetype = 2)+
  #geom_segment(y = 800, yend = 800, x = brpt_herring_negbi/1000000-brpt_ste_herring_negbi/1000000,
  #            xend = brpt_herring_negbi/1000000+brpt_ste_herring_negbi/1000000, col = "red")+
  geom_line(aes(y = coef_herring_negbi/10000000, col = "seg. neg bi"))+
  geom_vline(aes(col = "segmented", xintercept = brpt_herring/1000), linetype = 2)+
  #geom_segment(y = 20, yend = 20, x = brpt_herring/1000000-brpt_ste_herring/1000000,
  #            xend = brpt_herring/1000000+brpt_ste_herring/1000000, col = "red")+
  geom_line(aes(y = coef_herring/1000000, col = "segmented"))+
  geom_vline(aes(col = "segmented log", xintercept = exp(brpt_herring_log)/1000), linetype = 2)+
  #geom_segment(y = 0.5, yend = 0.5, x = exp(brpt_herring_log)/1000000-exp(brpt_ste_herring_log)/1000000,
  #            xend = exp(brpt_herring_log)/1000000+exp(brpt_ste_herring_log)/1000000, col = "red")+
  geom_line(aes(y = exp(coef_herring_log)/1000000, col = "segmented log"))+
  geom_line(data = ns_data, aes(ssb/1000,pR_ricker/1000000, col = "Ricker"), show.legend = TRUE)+ #Ricker
  geom_line(data=ns_data, aes(ssb/1000,pR_beverton/1000000, col = "Beverton-Holt"), show.legend = TRUE)+ #BH
  geom_line(data = ns_data, aes(ssb/1000, fitI/1000000, col = "Indipendence"), show.legend = T)+
  #geom_line(data = herring[herring$SSB_lag <= best_brk[1], ], aes(x = SSB_lag/1000, y = fit1/1000, col = "strucchange"), show.legend = TRUE)+
  #geom_line(data = herring[herring$SSB_lag > best_brk[1] & herring$SSB_lag<= best_brk[2], ], aes(x = SSB_lag/1000, y = fit2/100000, col = "strucchange"), show.legend = TRUE)+
  #geom_line(data = herring[herring$SSB_lag > best_brk[2], ], aes(SSB_lag/1000, fit3/1000, col = "strucchange"), show.legend = TRUE)+
  #geom_vline(aes(xintercept = best_brk[1]/1000, col = "strucchange"), linetype = 2)+
  #geom_vline(aes(xintercept = best_brk[2]/1000, col = "strucchange"), linetype = 2)+
  labs(col = "Model")

SRR_herring_models
#Note: must abline strucchange in different way (if it works)

rmse <- function(sim, obs) {
  sqrt(mean((obs-sim)^2))
}

comparison <- NULL
comparison <- AIC(m1, srR_beverton, srR_ricker, seg_herring, seg_herring_log, seg_herring_negbi
                  , strucc_herring
)
comparison[1, 3] <- rmse(sim = fitI, obs = herring$R_0)
comparison[2, 3] <- rmse(sim = ns_data$pR_beverton, obs = herring$R_0)
comparison[3, 3] <- rmse(sim = ns_data$pR_ricker, obs = herring$R_0)
comparison[4, 3] <- rmse(sim = seg_herring$fitted.values, obs = herring$R_0)
comparison[5, 3] <- rmse(sim = seg_herring_log$fitted.values, obs = herring$R_0)
comparison[6, 3] <- rmse(sim = seg_herring_negbi$fitted.values, obs = herring$R_0)
comparison[7, 3] <- rmse(sim = c(fit1, fit2, fit3), obs = herring$R_0)
comparison
