library(tidyverse)

# season <- lapply(2015:2018,function(s) nflscrapR::scrape_game_ids(season = s,weeks = 1:17)) %>% 
#   do.call(what = "rbind")
#write_csv(season,"data/season18_post.csv")
season <- rbind(nflscrapR::scrape_game_ids(season = 2018,weeks = 1:17),
                nflscrapR::scrape_game_ids(season = 2018,type = "post"))
season <- read_csv("data/season18_post.csv")

season <- season %>% 
  select(home_team,away_team,home_score,away_score,week,season) %>% 
  filter(home_team != "NPR") %>% 
  mutate(differential = home_score - away_score,
         post = ifelse(week == 18,1,0),
         superbowl = (home_team == "LA")*(away_team == "NE")) %>% 
  mutate_at(vars(ends_with("team")),function(v) case_when(v == "SD" ~ "LAC",
                                                          v == "STL" ~ "LA",
                                                          v == "JAC" ~ "JAX",
                                                          TRUE ~ v))

teams <- nflscrapR::nflteams$abbr %>% {. [.%in% c(season$home_team,season$away_team)]} %>% sort
teams %>% length

gamemat <- apply(season,1,function(g){
  teams_in_game <- rep(0,length(teams))
  home <- g["home_team"]
  teams_in_game[which(teams == g["home_team"])] <- 1
  teams_in_game[which(teams == g["away_team"])] <- -1
  teams_in_game
}) %>% t

gamemat <- gamemat %>% 
  (function(d){
    colnames(d) <- teams
    d
  }) %>% 
  as_tibble()

nonsuperbowl <- 1-season$superbowl 
lin.mod <- lm(season$differential ~ 0 + nonsuperbowl + . ,data = gamemat %>% select(-TEN)) 
std.coef <- c(lin.mod$coefficients,"TEN" = 0)[teams] %>% {.-mean(.)}
lin.mod %>% summary



#lme4::lmer(season$differential ~ 1 + (.|season$home_team),data = gamemat)

par(mfrow = c(2,2))
plot(lin.mod)
par(mfrow = c(1,1))

#each week, mu_wt \sim N(mu_t,tau_t)
jags.model <- function(){
  #data
  for(g in 1:ngames){
    xmean[g] <- beta0 + mu[hometeam[g]] - mu[awayteam[g]]
    Y[g] ~ dnorm(xmean[g],sigma_prec)
  }
  
  #teams
  for(t in 1:32){
    # for(s in 1:max(season)){
    #   beta[t] ~ dnorm(mu[t],tau[t])
    # }
    # for(s in 1:2){
    #   beta[t,s] = beta.star[t,s] - mean(beta.star[,s])
    #   beta.star[t,s] ~ dnorm(mu[t],tau[t])
    # }
    # for(w in 1:max(week)){
    #   beta[t,w] ~ dnorm(mu.star[t],tau[t])
    # }
    #tau_prec[t] ~ dgamma(v/2,tau0*v/2)
    #tau[t] = tau0 #1/sqrt(tau_prec[t])
    mu.star[t] ~ dnorm(mu0,phi)
    mu[t] = mu.star[t] - mean(mu.star)
  }
  
  #priors
  beta0 ~ dnorm(0,1/10000)
  mu0 = 0 #~ dnorm(0,1/100)
  tau0 ~ dgamma(1/100,1/100)
  v ~ dunif(0,100)
  sigma_prec ~ dgamma(1/100,1/100)
  sigma = 1/sqrt(sigma_prec)
  phi ~ dgamma(1/100,1/100)
}

jags.data <- list(Y=season$differential,
                  hometeam = season$home_team %>% as.factor,
                  awayteam = season$away_team %>% as.factor,
                  season = season$season %>% {. - min(.) + 1},
                  week = season$week,
                  superbowl = season$superbowl,
                  #gamemat = gamemat %>% select(-differential),
                  ngames = nrow(gamemat))

jags.output <- 
  R2jags::jags(data = jags.data,model.file = jags.model,
              parameters.to.save = c("beta0","mu","mu0","tau","tau0","sigma","xmean","v"),n.iter = 10000)
jags.output
saveRDS(jags.output,"output/jags.output.rds")

data.frame(team = teams,"bayes" = jags.output$BUGSoutput$mean$mu,"freq" = std.coef %>% as.vector())

ppc_sample <- rnorm(nrow(season),jags.output$BUGSoutput$mean$xmean,jags.output$BUGSoutput$mean$sigma)

data.frame(observed = season$differential,simulated = ppc_sample) %>% 
  reshape2::melt() %>% 
  ggplot(aes(fill = variable,x=value)) + 
  geom_density(alpha=.6)


#each season, mu_st \sim N(mu_t,tau_t)
jags.model.weekre <- function(){
  #data
  for(g in 1:ngames){
    Y[g] ~ dnorm(beta0 + inprod(gamemat[g,],beta[g,]),sigma)
    
    for(t in 1:32){
      beta[g,t] ~ dnorm(mu[t],tau[t])
    }
  }
  
  #teams
  for(t in 1:32){
    tau[t] ~ dgamma(tau0*1/100,1/100)
    mu[t] ~ dnorm(mu0,phi)
  }
  
  #priors
  beta0 ~ dnorm(0,1/100)
  mu0 = 0 #~ dnorm(0,1/100)
  tau0 ~ dgamma(1/100,1/100)
  sigma ~ dgamma(1/100,1/100)
  phi ~ dgamma(1/100,1/100)
}

#possible scores in integers 0:70
sum(0:100 %in% unique(as.matrix(expand.grid(0:50,0:50,0:50)) %*% matrix(c(2,3,7),ncol=1)))
