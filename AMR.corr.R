### ----------------------------------

##-- CORRELATING AMR DATA WITH SIGNIFICANT DATA
####------------ THIS WORKS. DON'T RUN WHOLE PAGE

# run all.base.data.R

library(dplyr)
require(ggpubr)
require(ggplot2)
require(viridis)
require(reshape2)
library(corrr)

##-------------------------------------------------------
## For a table of all significant correlations

d0 <- all %>%
  subset(IMP == "FMT") %>%
  subset(time_point == "D0") %>%
  select_if(is.numeric)

#correlate, select what we want to plot, gather, select only sig results
cor0 <- correlate(d0) %>%
  select(c(term, vanD, vanL)) %>%
  dplyr::rename(FMT.vanD.0 = "vanD") %>%
  dplyr::rename(FMT.vanL.0 = "vanL")

#--
d7 <- all %>%
  subset(IMP == "FMT") %>%
  subset(time_point == "D7") %>%
  select_if(is.numeric)

#correlate, select what we want to plot, gather, select only sig results
cor7 <- correlate(d7) %>%
  select(c(term, vanD, vanL)) %>%
  dplyr::rename(FMT.vanD.7 = "vanD") %>%
  dplyr::rename(FMT.vanL.7 = "vanL")
#-
d30 <- all %>%
  subset(IMP == "FMT") %>%
  subset(time_point == "D30") %>%
  select_if(is.numeric)

#correlate, select what we want to plot, gather, select only sig results
cor30 <- correlate(d30) %>%
  select(c(term, vanD, vanL)) %>%
  dplyr::rename(FMT.vanD.30 = "vanD") %>%
  dplyr::rename(FMT.vanL.30 = "vanL")
#-
d90 <- all %>%
  subset(IMP == "FMT") %>%
  subset(time_point == "D90") %>%
  select_if(is.numeric)

#correlate, select what we want to plot, gather, select only sig results
cor90 <- correlate(d90) %>%
  select(c(term, vanD, vanL)) %>%
  dplyr::rename(FMT.vanD.90 = "vanD") %>%
  dplyr::rename(FMT.vanL.90 = "vanL")

FMT.amr.cor <- cor0 %>% 
  inner_join(cor7, "term") %>%
  inner_join(cor30, "term") %>%
  inner_join(cor90, "term") %>%
  dplyr::rename(Variable = "term")

FMT.amr.cor <- gather(FMT.amr.cor, key, value, -Variable) %>%
  filter(value <= -0.5 | value >= 0.5) %>%
  spread(key, value)

FMT.amr.cor <- FMT.amr.cor[, c(1, 2, 3, 4, 6, 5, 7)]  
#-- 
#placebo AMR correlations
p0 <- all %>%
  subset(IMP == "placebo") %>%
  subset(time_point == "D0") %>%
  select_if(is.numeric)

#correlate, select what we want to plot, gather, select only sig results
pcor0 <- correlate(p0) %>%
  select(c(term, vanD, vanL)) %>%
  dplyr::rename(p.vanD.0 = "vanD") %>%
  dplyr::rename(p.vanL.0 = "vanL")

#--
p7 <- all %>%
  subset(IMP == "placebo") %>%
  subset(time_point == "D7") %>%
  select_if(is.numeric)

#correlate, select what we want to plot, gather, select only sig results
pcor7 <- correlate(p7) %>%
  select(c(term, vanD, vanL)) %>%
  dplyr::rename(p.vanD.7 = "vanD") %>%
  dplyr::rename(p.vanL.7 = "vanL")
#-
p30 <- all %>%
  subset(IMP == "placebo") %>%
  subset(time_point == "D30") %>%
  select_if(is.numeric)

#correlate, select what we want to plot, gather, select only sig results
pcor30 <- correlate(p30) %>%
  select(c(term, vanD, vanL)) %>%
  dplyr::rename(p.vanD.30 = "vanD") %>%
  dplyr::rename(p.vanL.30 = "vanL")
#-
p90 <- all %>%
  subset(IMP == "placebo") %>%
  subset(time_point == "D90") %>%
  select_if(is.numeric)

#correlate, select what we want to plot, gather, select only sig results
pcor90 <- correlate(p90) %>%
  select(c(term, vanD, vanL)) %>%
  dplyr::rename(p.vanD.90 = "vanD") %>%
  dplyr::rename(p.vanL.90 = "vanL")

placebo.amr.cor <- pcor0 %>%
  inner_join(pcor7, "term") %>%
  inner_join(pcor30, "term") %>%
  inner_join(pcor90, "term") %>%
  dplyr::rename(Variable = "term")

placebo.amr.cor <- gather(placebo.amr.cor, key, value, -Variable) %>%
  filter(value <= -0.5 | value >= 0.5) %>%
  spread(key, value)

placebo.amr.cor <- placebo.amr.cor[, c(1, 2, 3, 4, 6, 5, 7)]  
##--
#bind FMT.amr.cor and placebo.amr.cor together, showing all FMT and only relevant placebo results
amr.cor <- FMT.amr.cor %>% 
  left_join(placebo.amr.cor, "Variable") #%>%
  #dplyr::rename(Variable = "term")
#restructure col headings
amr.cor <- amr.cor[, c(1, 2, 3, 8, 9, 4, 5, 6, 7, 10, 11, 12, 13)] 


#strong correlations only for all
#high.sig <- gather(amr.cor, key, value, -Variable) %>%
  #filter(value <= -0.7 | value >= 0.7) %>%
  #spread(key, value)
#high.sig <- high.sig[, c(1, 2, 3, 8, 9, 4, 6, 5, 7, 10, 12, 11, 13)]  

#library(writexl)
#write_xlsx(amr.cor, "\\sig.AMR.corr.xlsx")

##---

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
rm(FMT.amr.cor)
rm(placebo.amr.cor)


#-----------------
#-----------------
# TO GET amr.cor FOR FURTHER WORK, ONLY RUN TO HERE #
#-----------------
#-----------------

#Plot only significant variables in a heatmap

vars <- amr.cor$Variable
amr <- all %>%
  select(id, time_point, IMP, vars, vanD, vanL) %>%
  filter(!grepl('DONOR', IMP)) %>%
  filter(!grepl('No_FMT', IMP))


#make a grid of all IMP(Var1) and time_point(Var2)
combinations <- expand.grid(unique(amr$IMP), unique(amr$time_point))
#Create for() loop to run through grid, taking all row names, melting them, 
#subsetting the numeric data, and correlating them
for (row in rownames(combinations)){
  nam <- paste(combinations[row,'Var1'], combinations[row,'Var2'], sep = "")
  df <- correlate(
    select_if(
      subset(amr, (IMP == combinations[row,"Var1"] & time_point == combinations[row,"Var2"])),
      is.numeric),
    use='pairwise.complete.obs',
    method="pearson") %>%
    select(c(term, vanD, vanL)) %>%
    gather(key, value, -term)
  
  #still within the for() loop, plot heatmaps 
  #removing for() loop means that only final result (FMTD90) will show
  p <- ggplot(data = df, aes(x=term, y=key, fill=value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#0C6291", high = "#A63446", mid = "#FBFEF9", midpoint = 0, limit = c(-1,1), space = "Lab", name = "Pearson\nCorrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 9, hjust = 1), axis.text.y = element_text(vjust = 1, hjust = 1, size = 9)) +
    coord_fixed() +
    geom_text(aes(term, key, label = round(value, 2)), color = "black", size = 1.5) +
    labs(x=element_blank(), y=element_blank(), title = nam, size = 8)
  #facet_grid(Var1~Var2)
  print(p)
  #ggsave(filename = paste(nam,"amr.cor.pdf",sep = ""), device = 'pdf', scale = 1.5)
}

##---------------