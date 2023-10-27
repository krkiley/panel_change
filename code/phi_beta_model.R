
### What is this file?
#This code uses the model outlined in Kiley and Vaisey (2020) to estimate
#the presence of persistent change in the GSS panels.

#### Load in a long version of the GSS panels.
load("~/Desktop/panel_change/data/gsslong.Rdata")
load("~/Desktop/panel_change/data/attitude_vars.Rdata")
load("~/Desktop/panel_change/data/good_vars.Rdata")

#Make a list of variables in the gss
gss_vars <- attitude_vars$var


#Function that creates a subset of the panel that is just a variable of 
#interest, taken as a character vector, and omits missing cases
cleanGSS <- function(var) {
  data <- gss.long %>%
    select(idnum, ds, minage, wtpannr123, wave, var) %>%
    mutate(wave = paste("w", wave, sep = "")) %>%
    spread(wave, var) %>%
    na.omit() %>% filter(!is.infinite(minage)) %>%
    mutate(var = var) %>%
    mutate(wtpannr123 = as.numeric(wtpannr123))
  return(data)
}

## Loop that estimates the non-linear model for all variables in the 
## list. Stores those in a list named "results"
results <- vector(mode = "list", length = length(gss_vars))
for (i in 1:length(gss_vars)) {
  
  var <- gss_vars[i]
  
  #extract focal variable
  all.data <-  cleanGSS(var) %>%
    mutate(y1 = w1, y2 = w2, y3 = w3) %>%
    select(-c(w1, w2, w3)) 

  #Unconstrained model
  nls1 <- nls(y3 ~ a + b*p*y2 + b*(1-p)*y1,
              data = all.data,
              start = list(a = 0, b = 1, p = 0),
              weights = wtpannr123)
  bic1 <- BIC(nls1)

  #Model that constrains phi to .5
  nls2 <- nls(y3 ~ a + b*.5*y2 + b*(.5)*y1,
              data = all.data,
              start = list(a = 0, b = 1),
              weights = wtpannr123)
  bic2 <- BIC(nls2)

  #store results in list
  results[[i]] <- data.frame(var = var, bic1 = bic1, bic2 = bic2,
                             p = coef(nls1)[3], 
                             b1 = coef(nls1)[2], b2 = coef(nls2)[2])
  
  #print to spot errors
  print(paste(var, i, sep = " "))
  
  #clean out before the next iteration
  rm(var, bic1, bic2, nls1, nls2)
  
}

#Create a data frame of the results that 
#calculates whether constraint is preferred
#by comparing the bic of the constrained and 
#unconstrained models
res.df <- bind_rows(results) %>%
  mutate(aum_pref = ifelse(bic1 < bic2, 1, 0),
         bicdiff = bic1 - bic2,
         or_aum = exp(-bicdiff/2),
         prob_aum = or_aum/(or_aum + 1)) 

#make a vector of variables that prefer the constraints
stable <- res.df %>% 
  filter(aum_pref == 0)
stable_vars <- stable$var

#Save these data sets
save(res.df, file = "~/Desktop/panel_change/correction/results_df.Rdata")

#select only items where AUM is preferred
aum_vars <- res.df %>% filter(aum_pref == 1)
aum_vars <- aum_vars$var

#Create a vector of age cutoffs
cutoffs <- seq(20, 45, by = 1)

#Create a vector to store the results at various cutoffs.
cutoff.results <- vector(mode = "list", length = length(aum_vars))
count <- 0
for (i in 1:length(aum_vars)) {
  
  var <- aum_vars[i]
  
  #extract focal variable
  all.data <-  cleanGSS(var) %>%
    mutate(y1 = w1, y2 = w2, y3 = w3) %>%
    select(-c(w1, w2, w3)) 
  
  #Estimate baseline model without age dummy
  base.nls <- nls(y3 ~ a + b*p*y2 + b*(1-p)*y1,
                 data = all.data,
                 start = list(a = 0, b = 1, p = 0),
                 weights = wtpannr123)
  full.bic <- BIC(base.nls)
  
  #Establish a cutoff
  for (j in 1:length(cutoffs)) {
    count <- count + 1
    
    cutoff <- cutoffs[j]
    
    #Create a dummy for people equal to or under that cutoff
    age.data <- all.data %>%
      mutate(young = ifelse(minage <= cutoff, 1, 0))
    
    age.nls <- nls(y3 ~ a + b*(p+d*young)*y2 + b*(1-(p+d*young))*y1,
                   data = age.data,
                   start = list(a = 0, b = 1, p = 0, d = 0),
                   weights = wtpannr123,
                   control = nls.control(maxiter = 5000, warnOnly = TRUE))
    p.est <- coef(age.nls)[3]
    d.est <- coef(age.nls)[4]
    age.bic <- BIC(age.nls)

    
    cutoff.results[[count]] <- data.frame(var = var, cutoff = cutoff, full.bic = full.bic, 
                                          agebic = age.bic,
                                          p.est = p.est, d.est = d.est)
    
    print(paste(var, cutoff, sep = " "))
    
  }
  
}


#Create a data set that indicates which items 
#are better off with the dummy variable
age_summary <- bind_rows(cutoff.results) %>%
  mutate(age.good = ifelse(agebic < full.bic, 1, 0)) %>%
  group_by(var) %>% summarise(pct.good = mean(age.good, na.rm = TRUE),
                              mean.effect = mean(d.est, na.rm = TRUE)) %>%
  arrange(desc(pct.good)) 

#For those items that prefer a dummy variable, estimate the appox phi
#above and below the age cutoff
age_vars <- age_summary$var[age_summary$pct.good >= .5]
age.results <- vector(mode = "list", length = length(age_vars))
for (i in 1:length(age_vars)) {
  
  #extract var from the list of variables
  var <- age_vars[i]
  
  #extract the data set of just the focal variable
  all <-  cleanGSS(var) %>%
    mutate(y1 = w1, y2 = w2, y3 = w3) %>%
    select(-c(w1, w2, w3)) 
  
  #estimate an unconstrained model to 
  #get an estimate of beta and alpha to fix in 
  #the next models
  all.nls <- nls(y3 ~ a + b*p*y2 + b*(1-p)*y1,
                   data = all,
                   start = list(a = 0, b = 1, p = 0),
                   weights = wtpannr123)
  #store those values
  set.a <- coef(all.nls)[1]
  set.b <- coef(all.nls)[2]
  
  #Create a data set of just people 30 and under
  young <-  cleanGSS(var) %>%
    mutate(y1 = w1, y2 = w2, y3 = w3) %>%
    select(-c(w1, w2, w3)) %>%
    filter(minage <= 30)
  
  #create a data set of people over 30
  old <-  cleanGSS(var) %>%
    mutate(y1 = w1, y2 = w2, y3 = w3) %>%
    select(-c(w1, w2, w3)) %>%
    filter(minage > 30)
  
  #estimate phi for each group, constraining alpha and beta to be
  #equal for both groups
  young.nls <- nls(y3 ~ set.a + set.b*p*y2 + set.b*(1-p)*y1,
                 data = young,
                 start = list(p = 0),
                 weights = wtpannr123)
  
  old.nls <- nls(y3 ~ set.a + set.b*p*y2 + set.b*(1-p)*y1,
                   data = old,
                   start = list(p = 0),
                   weights = wtpannr123)
  
  t.young <- tidy(young.nls) %>% mutate(age = "young", var = var)
  t.old <- tidy(old.nls) %>% mutate(age = "old", var = var)
  age.results[[i]] <- bind_rows(t.young, t.old)
  
  rm(var, all, all.nls, set.a, set.b, young, old, young.nls, old.nls, 
     t.young, t.old)
}

save(age.results, file = "~/Desktop/panel_change/correction/age_results.Rdata")

bind_rows(age.results) %>%
  mutate(ci.lower = estimate - 1.96 * std.error,
         ci.upper = estimate + 1.96 * std.error) %>%
  left_join(good_vars) %>%
  mutate(age = recode(age, "old"="Over 30", "young"="30 & under")) %>%
  ggplot(aes(x = reorder(var.name, estimate), y = estimate, color = age)) + 
  geom_hline(yintercept = .5, color = "black", linetype = 2) + 
  geom_pointrange(aes(ymin = ci.lower, ymax = ci.upper)) + 
  coord_flip() + 
  theme_minimal() + 
  scale_color_manual(values = c("black", "gray")) + 
  labs(x = "", y = "Phi estimate", color = "") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=9),
        legend.position = "bottom")




p.estimates <- vector(mode = "list", length = length(gss_vars))
for (i in 1:length(gss_vars)) {
  
  var <- gss_vars[i]
  
  all <-  cleanGSS(var) %>%
    mutate(y1 = w1, y2 = w2, y3 = w3) %>%
    select(-c(w1, w2, w3)) 
  
  nls1 <- nls(y3 ~ a + b*p*y2 + b*(1-p)*y1,
      data = all,
      start = list(a = 0, b = 1, p = 0),
      weights = wtpannr123)
  
  p.estimates[[i]] <- tnls1 <- tidy(nls1) %>% mutate(var = var)
}

clean.pestimates <- bind_rows(p.estimates) %>%
  filter(term == "p") %>%
  mutate(estimate = ifelse(var %in% stable_vars, .5, estimate),
         stable = ifelse(var %in% stable_vars, 1, 0)) %>%
  left_join(good_vars) %>%
  mutate(ci.lower = estimate - 1.96 * std.error,
         ci.upper = estimate + 1.96 * std.error) %>%
  mutate(ci.lower = ifelse(var %in% stable_vars, .5, ci.lower),
         ci.upper = ifelse(var %in% stable_vars, .5, ci.upper)) 

save(clean.pestimates, file = "~/Desktop/panel_change/correction/figure_data.Rdata")

plot.data <- clean.pestimates %>%
  filter(!is.na(small.cat)) %>%
  group_by(small.cat) %>%
  summarise(median = median(estimate), 
            q25 = quantile(estimate)[2],
            q75 = quantile(estimate)[4],
            min = quantile(estimate)[1],
            max = quantile(estimate)[5]) 

label.points <- clean.pestimates %>%
  filter(!is.na(small.cat)) %>%
  group_by(small.cat) %>%
  filter(estimate == max(estimate))  %>%
  ungroup() %>%
  select(small.cat, var.name)

plot.data <- left_join(plot.data, label.points)
save(plot.data, file = "~/Desktop/panel_change/correction/summary_plot_data.Rdata")




