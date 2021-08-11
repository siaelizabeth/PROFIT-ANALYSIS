### FULL, SIGNIFICANT WILCOXON TABLE COMPARING IMPs (either placebo or FMT) AT EACH TIMEPOINT

# run all.base.data.R

##-----------------------------------------------------------------------
#-- Do Wilcoxon Rank Sum for entire dataframe

#Get a WIDE dataframe of ONLY numeric variables, IMP, and time_point
n <- all %>%
  select(c(IMP, time_point), which(sapply(., is.numeric)))

#For all time_points:
#D0
nWn.0 <- n %>%
  subset(time_point == "D0") %>%
  select(-time_point) %>%
  dplyr:: relocate(IMP)


#D7
nWn.7 <- n %>%
  subset(time_point == "D7") %>%
  select(-time_point) %>%
  dplyr:: relocate(IMP)

#D30
nWn.30 <- n %>%
  subset(time_point == "D30") %>%
  select(-time_point) %>%
  dplyr:: relocate(IMP)

#D90
nWn.90 <- n %>%
  subset(time_point == "D90") %>%
  select(-time_point) %>%
  dplyr:: relocate(IMP)
##--
# now apply the Mann-Whitney U test for all variables
variables <- colnames(n)[3:dim(n)[2]]

#-- 
#For() D0
#create empty lists
pvals<-{}
vars<-{}

#run for() loop to get wilcoxon p.values
for (i in variables) {
  xxx <- nWn.0 %>% 
    select(c("IMP", i))
  x1 <- xxx %>%
    filter(IMP == "FMT") %>% 
    dplyr::select(c(i)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(IMP =="placebo") %>% 
    dplyr::select(c(i)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df 
nW.df.0 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(Wilcox.p.value.D0 = "pvalues")
nW.df.0$Variable <- as.character(nW.df.0$Variable)

##---
#For() D7
#D7
nWn.7 <- n %>%
  subset(time_point == "D7") %>%
  select(-time_point) %>%
  dplyr:: relocate(IMP) 

variables <- colnames(nWn.7)[3:dim(n)[2]]
#Create empty lists
pvals<-{}
vars<-{}

for (i in variables) {
  xxx <- nWn.7 %>% 
    select(c("IMP", i))
  x1 <- xxx %>%
    filter(IMP == "FMT") %>% 
    dplyr::select(c(i)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(IMP =="placebo") %>% 
    dplyr::select(c(i)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df
nW.df.7 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(Wilcox.p.value.D7 = "pvalues")

nW.df.7$Variable <- as.character(nW.df.7$Variable)

##---
#For() D30
#D30
nWn.30 <- n %>%
  subset(time_point == "D30") %>%
  select(-time_point) %>%
  dplyr:: relocate(IMP) #%>%
  #subset(select = -c(cp.lactate_mmol.L_plasma))

variables <- colnames(nWn.30)[3:dim(n)[2]]
#Create empty lists
pvals<-{}
vars<-{}
for (i in variables) {
  xxx <- nWn.30 %>% 
    select(c("IMP", i))
  x1 <- xxx %>%
    filter(IMP == "FMT") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(IMP =="placebo") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df
nW.df.30 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(Wilcox.p.value.D30 = "pvalues")

nW.df.30$Variable <- as.character(nW.df.30$Variable)

##---
#For() D90
#D90
nWn.90 <- n %>%
  subset(time_point == "D90") %>%
  select(-time_point) %>%
  dplyr:: relocate(IMP) 

#Create empty lists
pvals<-{}
vars<-{}
for (i in variables) {
  xxx <- nWn.90 %>% 
    select(c("IMP", i))
  x1 <- xxx %>%
    filter(IMP == "FMT") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  x2 <- xxx %>% 
    filter(IMP =="placebo") %>% 
    dplyr::select(c(2)) %>% 
    na.omit() %>% 
    pull()
  wc <- wilcox.test(x1,x2)
  
  pvals <- c(pvals,round(wc$p.value,4) )
  vars <- c(vars,i)
}

#Make df
nW.df.90 <- data.frame(Variable=vars, pvalues=pvals) %>%
  dplyr::rename(Wilcox.p.value.D90 = "pvalues")

nW.df.90$Variable <- as.character(nW.df.90$Variable)

##--
#Join data.frame

wilcox.summary.report <- nW.df.0 %>% 
  full_join(nW.df.7, "Variable") %>% 
  full_join(nW.df.30, "Variable") %>% 
  full_join(nW.df.90, "Variable")

##-----------------------------------------------------------------------
#- Filter dataframe by significance

wilcox.IMP.sig <- gather(wilcox.summary.report, time.point, value, -Variable) %>%
  filter(value <= 0.05) %>%
  spread(time.point, value)
#change column order
wilcox.IMP.sig <- wilcox.IMP.sig[, c(1, 2, 4, 3, 5)]

view(wilcox.IMP.sig)


##-----------------------------------------------------------------------
rm(nW.df.7)
rm(nW.df.30)
rm(nW.df.90)
rm(nWn.0)
rm(nWn.7)
rm(nWn.30)
rm(nWn.90)
rm(wc)
rm(wilcox.summary.report)
rm(xxx)
rm(i)
rm(pvals)
rm(variables)
rm(vars)
rm(x1)
rm(x2)
rm(n)
rm(nW.df.0)
