#### Correlating significant cytokines 
library(dplyr)
require(ggpubr)
require(ggplot2)
require(viridis)
require(reshape2)
library(corrr)

#cytokine <- all %>%
  #select(time_point, id, IMP, starts_with("nc.IL"))

#I want to correlate cytokine data with ALL other data, running through a for() loop
#so that I can correlate each day and IMP seperately, and then paste the results into a dataframe

d0 <- all %>%
  subset(IMP == "FMT") %>%
  subset(time_point == "D0") %>%
  select_if(is.numeric) 
cor0 <- correlate(d0) %>%
  select(c(term, starts_with("nc.IL"))) #
colnames(cor0) <- paste("FMT.0.", colnames(cor0), sep = "")
cor0 <- cor0 %>%
  dplyr::rename(term = FMT.0.term)

d7 <- all %>%
  subset(IMP == "FMT") %>%
  subset(time_point == "D7") %>%
  select_if(is.numeric) 
cor7 <- correlate(d7) %>%
  select(c(term, starts_with("nc.IL"))) #
colnames(cor7) <- paste("FMT.07.", colnames(cor7), sep = "")
cor7 <- cor7 %>%
  dplyr::rename(term = FMT.07.term)

d30 <- all %>%
  subset(IMP == "FMT") %>%
  subset(time_point == "D30") %>%
  select_if(is.numeric) 

cor30 <- correlate(d30) %>%
  select(c(term, starts_with("nc.IL"))) #
colnames(cor30) <- paste("FMT.30.", colnames(cor30), sep = "")
cor30 <- cor30 %>%
  dplyr::rename(term = FMT.30.term)

d90 <- all %>%
  subset(IMP == "FMT") %>%
  subset(time_point == "D90") %>%
  select_if(is.numeric) 
cor90 <- correlate(d90) %>%
  select(c(term, starts_with("nc.IL"))) #
colnames(cor90) <- paste("FMT.90.", colnames(cor90), sep = "")
cor90 <- cor90 %>%
  dplyr::rename(term = FMT.90.term)

FMT.nc.cor <- cor0 %>% 
  inner_join(cor7, "term") %>%
  inner_join(cor30, "term") %>%
  inner_join(cor90, "term") %>%
  dplyr::rename(Variable = "term")

FMT.nc.cor <- gather(FMT.nc.cor, key, value, -Variable) %>%
  filter(value <= -0.5 | value >= 0.5) %>%
  spread(key, value)

#--

p0 <- all %>%
  subset(IMP == "placebo") %>%
  subset(time_point == "D0") %>%
  select_if(is.numeric) 
pcor0 <- correlate(p0) %>%
  select(c(term, starts_with("nc.IL"))) #
colnames(pcor0) <- paste("p.0.", colnames(pcor0), sep = "")
pcor0 <- pcor0 %>%
  dplyr::rename(term = p.0.term)

p7 <- all %>%
  subset(IMP == "placebo") %>%
  subset(time_point == "D7") %>%
  select_if(is.numeric) 
pcor7 <- correlate(p7) %>%
  select(c(term, starts_with("nc.IL"))) #
colnames(pcor7) <- paste("p.7.", colnames(pcor7), sep = "")
pcor7 <- pcor7 %>%
  dplyr::rename(term = p.7.term)

p30 <- all %>%
  subset(IMP == "placebo") %>%
  subset(time_point == "D30") %>%
  select_if(is.numeric) 
pcor30 <- correlate(p30) %>%
  select(c(term, starts_with("nc.IL"))) #
colnames(pcor30) <- paste("p.30.", colnames(pcor30), sep = "")
pcor30 <- pcor30 %>%
  dplyr::rename(term = p.30.term)

p90 <- all %>%
  subset(IMP == "placebo") %>%
  subset(time_point == "D90") %>%
  select_if(is.numeric) 
pcor90 <- correlate(p90) %>%
  select(c(term, starts_with("nc.IL"))) #
colnames(pcor90) <- paste("p.90.", colnames(pcor90), sep = "")
pcor90 <- pcor90 %>%
  dplyr::rename(term = p.90.term)

placebo.nc.cor <- pcor0 %>% 
  inner_join(pcor7, "term") %>%
  inner_join(pcor30, "term") %>%
  inner_join(pcor90, "term") %>%
  dplyr::rename(Variable = "term")

placebo.nc.cor <- gather(placebo.nc.cor, key, value, -Variable) %>%
  filter(value <= -0.5 | value >= 0.5) %>%
  spread(key, value)


nc.cor <- FMT.nc.cor %>% 
  left_join(placebo.nc.cor, "Variable") 
####------

rm(p30)
rm(p0)
rm(p7)
rm(p90)
rm(d7)
rm(d0)
rm(d30)
rm(d90)
rm(cor0)
rm(cor30)
rm(cor7)
rm(cor90)
rm(pcor0)
rm(pcor30)
rm(pcor7)
rm(pcor90)

rm(FMT.nc.cor)
rm(placebo.nc.cor)

###### CYTOKINES WITH OTHER SIGNIFICANT RESULTS

### RUN all.base.data
### RUN cytokine.correlation
### RUN merged_dataframes_wilcox
### RUN merged.wilcox.T_P

### Select significant correlations

### Plot significant correlations

library(dplyr)

##get names of dataframes to compare to find which have the SAME significant variables
sig.nc.names <- as.data.frame(nc.cor$Variable) %>%
  dplyr::rename(Variable = "nc.cor$Variable")
mann.u.names <- as.data.frame(all.mann.u.t_p.sig$Variable) %>%
  dplyr::rename(Variable = "all.mann.u.t_p.sig$Variable")

wilcox.names <- as.data.frame(sig.wilcox.between.IMP$Variable) %>%
  dplyr::rename(Variable = "sig.wilcox.between.IMP$Variable")

#inner join by "Variable" to find matching rownames
#significant between time points AND AMR correlations
sig.nc.mann.u <- dplyr::inner_join(sig.nc.names, mann.u.names, by = "Variable")

#significant between IMPs AND AMR correlations
sig.nc.IMP <- dplyr::inner_join(sig.nc.names, wilcox.names, by = "Variable")

#significant between time points AND between IMP
sig.mann.u.IMP <- dplyr::inner_join(mann.u.names, wilcox.names, by = "Variable")

#####---- FOR AMR DATA, these results show significant AMR correlations with variables 
## that are ALSO significant in the mann.u between time_point and the wilcox between IMP tests
nc.sig.variables <- dplyr::inner_join(sig.nc.mann.u, wilcox.names, by = "Variable")

view(nc.sig.variables)

## Subset sig results
nc.sig <- nc.sig.variables$Variable

#get significant nc.cor data
nc.sig.cor <- nc.cor[nc.cor$Variable %in% nc.sig, ]
#remove columns with only NA
nc.sig.cor <- nc.sig.cor[colSums(!is.na(nc.sig.cor)) > 0]
#transpose

#get this dataframe as excel spreadsheet:
#library(writexl)
#write_xlsx(nc.sig.cor, "\\sig.NC.corr.xlsx")

#get sig nc.cor results from Wilcox between time_point data
t.p <- all.mann.u.t_p.sig[all.mann.u.t_p.sig$Variable %in% nc.sig, ]

#get sig nc.cor results from Wilcox between IMP data
I.M.P <- sig.wilcox.between.IMP[sig.wilcox.between.IMP$Variable %in% nc.sig, ]


###------------------------------------------
rm(wilcox.names)
rm(sig.amr.IMP)
rm(sig.mann.u.IMP)
rm(sig.amr.mann.u)
rm(amr.sig)
rm(sig.amr.names)
rm(mann.u.names)
rm(combinations)

rm(amr)
rm(t.p)
rm(I.M.P)
rm(sig.all)


####----------------
##-- this in a heatmap

#nc.sig <- nc.sig.variables$Variable
#nc <- all %>%
  #select(c(id, time_point, IMP, nc.sig, nc.DLactate_faecal, nc.DLactate_plasma,
           #nc.IFNgamma, nc.TNF.alpha, nc.FABP2,
           #-nc.E_coli, -nc.E_faecalis, -nc.EC2.9.1.1)) %>%
  #filter(!grepl('DONOR', IMP)) %>%
  #filter(!grepl('No_FMT', IMP))

###----

### CORRELATING Cytokines with other, statistically significant data


library(reshape2)
library(ggplot2)

#Select cytokine data and make data long
cytokine <- all %>%
  select(c(id, IMP, time_point, nc.DLactate_faecal, nc.DLactate_plasma, u.Succinic_acid,
           u.Hippuric_acid, nc.faecal_ammonia, vanL, pc.IL.17A, 
           nc.IL.12, nc.IL.17A, nc.IL.17E, nc.IL.17F, nc.IL.21, nc.IL.22, nc.IL.23,
           nc.IL.10, nc.TNF.alpha, nc.IL.1beta,
           nc.E_coli, nc.E_faecalis))
           #starts_with("nc.IL")))
names(cytokine) <- str_replace_all(names(cytokine), c("nc.IL." = "faecal."))
names(cytokine) <- str_replace_all(names(cytokine), c("pc." = "plasma."))
names(cytokine) <- str_replace_all(names(cytokine), c("u." = "urine."))

### --- I want a correlation heatmap of IL-cytokines, succinic acid, hippuric acid, 
## -- vanD, vanL, D-lactate faecal, D-lactate plasma 


#make a grid of all IMP(Var1) and time_point(Var2)
combinations <- expand.grid(unique(cytokine$IMP), unique(cytokine$time_point))
#Create for() loop to run through grid, taking all row names, melting them, 
#subsetting the numeric data, and correlating them
for (row in rownames(combinations)){
  nam <- paste(combinations[row,'Var1'], combinations[row,'Var2'], sep = "")
  df <- correlate(
    select_if(
      subset(cytokine, (IMP == combinations[row,"Var1"] & time_point == combinations[row,"Var2"])),
      is.numeric),
    use='pairwise.complete.obs',
    method="spearman") %>%
    #select(c(term, starts_with("nc.IL"))) %>%
    select(c(term, c("nc.IL.12", "nc.IL.17A", "nc.IL.17E", "nc.IL.17F",
                     "nc.IL.21", "nc.IL.22", "nc.IL.10", "nc.TNF.alpha"))) %>%
    #select(c(term, c("faecal.IL.12", "faecal.IL.17A", "faecal.IL.17E", "faecal.IL.17F",
                    #"faecal.IL.21", "faecal.IL.22", "faecal.IL.10", "faecal.TNF.alpha"))) %>%
    gather(key, value, -term)
  
  #still within the for() loop, plot heatmaps 
  #removing for() loop means that only final result (FMTD90) will show
  p <- ggplot(data = df, aes(x=term, y=key, fill=value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#0C6291", high = "#A63446", mid = "#FBFEF9", midpoint = 0, limit = c(-1,1), space = "Lab", name = "Spearman\nCorrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 9, hjust = 1), axis.text.y = element_text(vjust = 1, hjust = 1, size = 9)) +
    coord_fixed() +
    geom_text(aes(term, key, label = round(value, 2)), color = "black", size = 2) +
    labs(x=element_blank(), y=element_blank(), title = nam, size = 8)
  #facet_grid(Var1~Var2)
  print(p)
  ggsave(filename = paste(nam,"cytokine.cor.pdf",sep = ""), device = 'pdf', scale = 1.5)
}


#####----------------------------------------------------------------------------
p.cytokine <- all %>%
  select(c(id, IMP, time_point, nc.DLactate_faecal, nc.DLactate_plasma, u.Succinic_acid,
           u.Hippuric_acid, nc.faecal_ammonia, vanL, pc.IL.17A, 
           nc.IL.12, nc.IL.17A, nc.IL.17E, nc.IL.17F, nc.IL.21, nc.IL.22, nc.IL.23,
           nc.IL.10, nc.TNF.alpha, nc.IL.1beta,
           nc.E_coli, nc.E_faecalis))
#starts_with("nc.IL")))
#names(cytokine) <- str_replace_all(names(cytokine), c("nc." = "faecal."))
names(cytokine) <- str_replace_all(names(cytokine), c("pc." = "plasma."))
names(cytokine) <- str_replace_all(names(cytokine), c("u." = "urine."))


#make a grid of all IMP(Var1) and time_point(Var2)
combinations <- expand.grid(unique(cytokine$IMP), unique(cytokine$time_point))
#Create for() loop to run through grid, taking all row names, melting them, 
#subsetting the numeric data, and correlating them
for (row in rownames(combinations)){
  nam <- paste(combinations[row,'Var1'], combinations[row,'Var2'], sep = "")
  df <- correlate(
    select_if(
      subset(cytokine, (IMP == combinations[row,"Var1"] & time_point == combinations[row,"Var2"])),
      is.numeric),
    use='pairwise.complete.obs',
    method="spearman") %>%
    #select(c(term, starts_with("nc.IL"))) %>%
    select(c(term, c("nc.IL.12", "nc.IL.17A", "nc.IL.17E", "nc.IL.17F",
                     "nc.IL.21", "nc.IL.22", "nc.IL.10", "nc.TNF.alpha"))) %>%
    #select(c(term, c("faecal.IL.12", "faecal.IL.17A", "faecal.IL.17E", "faecal.IL.17F",
    #"faecal.IL.21", "faecal.IL.22", "faecal.IL.10", "faecal.TNF.alpha"))) %>%
    gather(key, value, -term)
  
  #still within the for() loop, plot heatmaps 
  #removing for() loop means that only final result (FMTD90) will show
  p <- ggplot(data = df, aes(x=term, y=key, fill=value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#0C6291", high = "#A63446", mid = "#FBFEF9", midpoint = 0, limit = c(-1,1), space = "Lab", name = "Spearman\nCorrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 9, hjust = 1), axis.text.y = element_text(vjust = 1, hjust = 1, size = 9)) +
    coord_fixed() +
    geom_text(aes(term, key, label = round(value, 2)), color = "black", size = 2) +
    labs(x=element_blank(), y=element_blank(), title = nam, size = 8)
  #facet_grid(Var1~Var2)
  print(p)
  ggsave(filename = paste(nam,"cytokine.cor.pdf",sep = ""), device = 'pdf', scale = 1.5)
}