
library(ggpubr)
library(dplyr)
library(dlookr)
##----
##run all.base.data.R
#get normality of all, grouped by IMP and time.point

n.all <- all %>%
  group_by(IMP, time_point) %>%
  normality()

#NND = NOT NORMALLY DISTRIBUTED
#if data is <= 0.05 then reject null hypothesis, data does not follow normal distribution
nnd <- all %>%
  group_by(IMP, time_point) %>%
  normality() %>%
  filter(p_value < 0.05) %>%
  dplyr::rename(Shapiro.p.value = "p_value")

#nd = NORMALLY DISTRIBUTED
#if data is >= 0.05 then fail to reject null hypothesis, data probably does follow normal distribution
nd <- all %>%
  group_by(IMP, time_point) %>%
  normality() %>%
  filter(p_value > 0.05) %>%
  dplyr::rename(Shapiro.p.value = "p_value")

#####------------------------------------