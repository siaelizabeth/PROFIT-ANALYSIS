
### FULL, SIGNIFICANT WILCOXON/ MANN WHITNEY U TABLE COMPARING
### TIME POINTS FOR EACH IMP FOR EACH VARIABLE

#run all.base.data.R
##-----------------------------------------------------------------------

#Get a WIDE dataframe of ONLY numeric variables, IMP, and time_point
n <- all %>%
  select(c(IMP, time_point), which(sapply(., is.numeric)))

##-----------------------------------------------------------------------
#FMT all time_point
n.FMT <- n %>% 
  subset(IMP == "FMT") %>%
  select(-IMP) %>%
  dplyr:: relocate(time_point)

# Make separate for each time_point, then merge

#placebo all time_point
n.placebo <- n %>%
  subset(IMP == "placebo") %>%
  select(-IMP) %>%
  dplyr:: relocate(time_point)

# now apply the Mann-Whitney U test for all variables
variables <- colnames(n.FMT)[2:dim(n.FMT)[2]]

#-- 
#For() FMT D0-D7
#create empty lists
pvals<-{}
vars<-{}

#run for() loop to get wilcoxon p.values
for (i in variables) {
  xxx <- n.FMT %>% 
    select(c("time_point", i))
  x1 <- xxx %>%
    filter(time_point == "D0") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(time_point =="D7") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df 
n.FMT.0.7 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(W.FMT.D0_D7 = "pvalues")
n.FMT.0.7$Variable <- as.character(n.FMT.0.7$Variable)

##---
#For() placebo D0 - D7
#Create empty lists
pvals<-{}
vars<-{}

for (i in variables) {
  xxx <- n.placebo %>% 
    select(c("time_point", i))
  x1 <- xxx %>%
    filter(time_point == "D0") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(time_point =="D7") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df
n.placebo.0.7 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(W.P.D0_D7 = "pvalues")

n.placebo.0.7$Variable <- as.character(n.placebo.0.7$Variable)


##--

#For() FMT D7-D30
#create empty lists
pvals<-{}
vars<-{}

#run for() loop to get wilcoxon p.values
for (i in variables) {
  xxx <- n.FMT %>% 
    select(c("time_point", i))
  x1 <- xxx %>%
    filter(time_point == "D7") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(time_point =="D30") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df 
n.FMT.7.30 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(W.FMT.D7_D30 = "pvalues")
n.FMT.7.30$Variable <- as.character(n.FMT.7.30$Variable)

##---
#For() placebo D7 - D30
#Create empty lists
pvals<-{}
vars<-{}

for (i in variables) {
  xxx <- n.placebo %>% 
    select(c("time_point", i))
  x1 <- xxx %>%
    filter(time_point == "D7") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(time_point =="D30") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df
n.placebo.7.30 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(W.P.D7_D30 = "pvalues")

n.placebo.7.30$Variable <- as.character(n.placebo.7.30$Variable)

##---------------

#For() FMT D30-D90
#create empty lists
pvals<-{}
vars<-{}

#run for() loop to get wilcoxon p.values
for (i in variables) {
  xxx <- n.FMT %>% 
    select(c("time_point", i))
  x1 <- xxx %>%
    filter(time_point == "D30") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(time_point =="D90") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df 
n.FMT.30.90 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(W.FMT.D30_D90 = "pvalues")
n.FMT.30.90$Variable <- as.character(n.FMT.30.90$Variable)

##---
#For() placebo D30 - D90
#Create empty lists
pvals<-{}
vars<-{}

for (i in variables) {
  xxx <- n.placebo %>% 
    select(c("time_point", i))
  x1 <- xxx %>%
    filter(time_point == "D30") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(time_point =="D90") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df
n.placebo.30.90 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(W.P.D30_D90 = "pvalues")

n.placebo.30.90$Variable <- as.character(n.placebo.30.90$Variable)

#--
#FMT D0-D30
#create empty lists
pvals<-{}
vars<-{}

#run for() loop to get wilcoxon p.values
for (i in variables) {
  xxx <- n.FMT %>% 
    select(c("time_point", i))
  x1 <- xxx %>%
    filter(time_point == "D0") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(time_point =="D30") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df 
n.FMT.0.30 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(W.FMT.D0_D30 = "pvalues")
n.FMT.0.30$Variable <- as.character(n.FMT.0.30$Variable)

#----
#FMT D0-D90
#create empty lists
pvals<-{}
vars<-{}

#run for() loop to get wilcoxon p.values
for (i in variables) {
  xxx <- n.FMT %>% 
    select(c("time_point", i))
  x1 <- xxx %>%
    filter(time_point == "D0") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(time_point =="D90") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

n.FMT.0.90 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(W.FMT.D0_D90 = "pvalues")
n.FMT.0.90$Variable <- as.character(n.FMT.0.90$Variable)
#----
#create empty lists
pvals<-{}
vars<-{}

#run for() loop to get wilcoxon p.values
for (i in variables) {
  xxx <- n.FMT %>% 
    select(c("time_point", i))
  x1 <- xxx %>%
    filter(time_point == "D7") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(time_point =="D90") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df 
n.FMT.7.90 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(W.FMT.D7_D90 = "pvalues")
n.FMT.7.90$Variable <- as.character(n.FMT.7.90$Variable)

##---

#Join data.frame

wilcox.T_P <- n.placebo.0.7 %>% 
  inner_join(n.FMT.0.7, "Variable") %>%
  inner_join(n.FMT.0.30, "Variable") %>%
  inner_join(n.FMT.0.90, "Variable")%>%
  inner_join(n.FMT.7.30, "Variable") %>%
  inner_join(n.placebo.7.30, "Variable") %>%
  inner_join(n.FMT.7.90, "Variable") %>%
  inner_join(n.FMT.30.90, "Variable") %>%
  inner_join(n.placebo.30.90, "Variable")
  
##----------------------------------------------------
##-- SIGNIFICANT VALUES ONLY

wilcox.T_P.sig <- gather(wilcox.T_P, time.point, value, -Variable) %>%
  filter(value <= 0.05) %>%
  spread(time.point, value)

wilcox.T_P.sig <- wilcox.T_P.sig[, c(1, 3, 2, 4, 6, 7, 5, 8, 9)] 

#all.mann.u.t_p.sig <- all.mann.u.t_p.sig[-c(13, 15, 23, 31), ]

view(wilcox.T_P.sig)

rm(n.FMT)
rm(n.FMT.0.30)
rm(n.FMT.0.7)
rm(n.FMT.0.90)
rm(n.FMT.30.90)
rm(n.FMT.7.30)
rm(n.FMT.7.90)
rm(n.placebo)
rm(n.placebo.0.7)
rm(n.placebo.30.90)
rm(n.placebo.7.30)
rm(nc.summary.between.time.points)
rm(wc)
rm(xxx)
rm(n)
rm(x1)
rm(x2)
rm(vars)
rm(variables)
rm(pvals)
rm(i)
rm(wilcox.T_P)
