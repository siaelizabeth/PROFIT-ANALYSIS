
###----------------
###--- Kruskal Wallis Test between FMT and placebo group at all time points for all variables
###----------------


library(broom)
#Could POSSIBLY make this neater with split() and lapply() 

### ALL D0 p values 
kD0 <- all %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point == "D0") %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$IMP))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(D0 = p.value,
         variable = key)

### ALL D7 p values 
kD7 <- all %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point == "D7") %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$IMP))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(D7 = p.value,
         variable = key)

### ALL D30 p values 
kD30 <- all %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point == "D30") %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$IMP))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(D30 = p.value,
         variable = key)

### ALL D90 p values 
kD90 <- all %>%
  select(c(IMP, time_point, id), which(sapply(., is.numeric))) %>%
  filter(time_point == "D90") %>%
  gather(key, value, -IMP, -id, -time_point) %>%
  group_by(key) %>%
  do(tidy(kruskal.test(x = .$value, g = .$IMP))) %>%
  filter(p.value <= 0.05) %>%
  select(key, p.value) %>%
  rename(D90 = p.value,
         variable = key)

#Join the data frames
Kruskal.IMP <- kD0 %>% 
  full_join(kD7, "variable") %>% 
  full_join(kD30, "variable") %>% 
  full_join(kD90, "variable")

colnames(Kruskal.IMP)[1] <- "Variable"
#export as .csv to put in markdown

write.csv(Kruskal.IMP, "\\Kruskal.results.csv", row.names=FALSE)
#Kruskal.IMP <- read_csv("\\Kruskal.results.csv")


##Function to round values
# round all numeric variables
# x: data frame 
# digits: number of digits to round
round_df <- function(x, digits) {
  # select only numeric columns to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

#Round numeric values to 6 DP
Kruskal.IMP <- round_df(Kruskal.IMP, 6)


rm(kD0)
rm(kD7)
rm(kD30)
rm(kD90)
