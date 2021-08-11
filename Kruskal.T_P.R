
###----------------
###--- Kruskal Wallis Test between time points for both FMT and placebo groups for all variables
###----------------

##NOTE: Need to use %in% to filter more than one variable at a time
library(broom)

### FMT p-values 
FMT <- all %>% 
  filter(IMP == "FMT")

#1. FMT D0 - D7
F.0.7 <- FMT %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point %in% c("D0", "D7")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.FMT.D0_D7 = p.value,
         variable = key)
  
#2. FMT D0 - D30
F.0.30 <- FMT %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  subset(time_point %in% c("D0", "D30")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.FMT.D0_D30 = p.value,
         variable = key)

#3. FMT D0 - D90
F.0.90 <- FMT %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point %in% c("D0", "D90")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.FMT.D0_D90 = p.value,
         variable = key)

#4. FMT D7 - 30 
F.7.30 <- FMT %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point %in% c("D7", "D30")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.FMT.D7_D30 = p.value,
         variable = key)

#5. FMT D7 - D90 
F.7.90 <- FMT %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point %in% c("D7", "D90")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.FMT.D7_D90 = p.value,
         variable = key)

#6. FMT D30 - D90 
F.30.90 <- FMT %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point %in% c("D30", "D90")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.FMT.D30_D90 = p.value,
         variable = key)


###############-------------------------------------------------
###-- PLACEBO p-values 

p <- all %>% 
  filter(IMP == "placebo")

#1. FMT D0 - D7
p.0.7 <- p %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point %in% c("D0", "D7")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.p.D0_D7 = p.value,
         variable = key)

#2. FMT D0 - D30
p.0.30 <- p %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  subset(time_point %in% c("D0", "D30")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.p.D0_D30 = p.value,
         variable = key)

#3. FMT D0 - D90
p.0.90 <- p %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point %in% c("D0", "D90")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.p.D0_D90 = p.value,
         variable = key)

#4. FMT D7 - 30 
p.7.30 <- p %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point %in% c("D7", "D30")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.p.D7_D30 = p.value,
         variable = key)

#5. FMT D7 - D90 
p.7.90 <- p %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point %in% c("D7", "D90")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.p.D7_D90 = p.value,
         variable = key)

#6. FMT D30 - D90 
p.30.90 <- p %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point %in% c("D30", "D90")) %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$time_point))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(K.p.D30_D90 = p.value,
         variable = key)



###join those muthas

Kruskal.T_P <- F.0.7 %>% 
  full_join(F.0.30, "variable") %>% 
  full_join(F.0.90, "variable") %>% 
  full_join(F.7.30, "variable") %>%
  full_join(F.7.90, "variable") %>%
  full_join(F.30.90, "variable") %>%
  full_join(p.0.7, "variable") %>%
  full_join(p.0.30, "variable") %>%
  full_join(p.0.90, "variable") %>%
  full_join(p.7.30, "variable") %>%
  full_join(p.7.90, "variable") %>%
  full_join(p.30.90, "variable")

# Create a function to round numeric values
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

#Round numeric values to 6 DP
Kruskal.T_P <- round_df(Kruskal.T_P, 6)

rm(FMT)
rm(p)
rm(F.0.7)
rm(F.0.30)
rm(F.0.90)
rm(F.7.30)
rm(F.7.90)
rm(F.30.90)
rm(p.0.7)
rm(p.0.30)
rm(p.0.90)
rm(p.7.30)
rm(p.7.90)
rm(p.30.90)
