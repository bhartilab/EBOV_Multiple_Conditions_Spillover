
setwd(".")

library(dplyr)
library(ggplot2)
library(ggpubr)
library(dendextend)
library(stringr)
library(IncDTW)

##3 ekata and olloba are shifted by a year, fix it! 
###################### TEMPERATURE
all<- read.csv("allspills_20kbuff_1990_2022_dtrndtemp_20250824.csv", header = T)

buf20<- subset(all, buffer == 20)

buf20<- buf20 %>%
  arrange(rolling_num)
buf20<- buf20 %>%
  arrange(newprd2)
buf20<- buf20 %>%
  arrange(Location)


spills<- split(buf20, ceiling(seq_along(buf20$spmean_rain) / 23 ))

r<- split(buf20$spmean_rain, ceiling(seq_along(buf20$spmean_rain) / 23))
t<- split(buf20$dtrnd_temp , ceiling(seq_along(buf20$dtrnd_temp) / 23))
#544 times series 



labs<- rep(c("Bik", "Boe", "Bou", "Eka", "Kik", "Lik","Lue", "May", "Mb1", "Mb2", "Mbz","Mek", "Mvo", "Nki", "Odz", "Ol1", "Ol2"), each=32)
yrs<-as.character(replicate(17,c("90_91","91_92","92_93","93_94","94_95","95_96","96_97","97_98","98_99","99_00","00_01","01_02","02_03","03_04","04_05","05_06","06_07","07_08", "08_09","09_10","10_11","11_12", "12_13","13_14","14_15","15_16","16_17","17_18", "18_19","19_20","20_21", "21_22")))

l<- as.data.frame(cbind(labs,yrs))
l$labs<- paste(l$labs, l$yrs, sep = "_")
labs<- l$labs

#######

overlap<-read.csv("alloverlaps_allpcts_buffoverlap_20250901.csv")

ovlap20<- subset(overlap, overlap_percent20 >= 49.5 ) #get buffers that overlap more than 20% and remove self matches
table(ovlap20$st_loc)

ovlap20$spillone<- ifelse(ovlap20$st_loc == "Mekouka", "Mek_93_94", 
                          ifelse(ovlap20$st_loc == "Kikwit", "Kik_94_95", 
                                 ifelse(ovlap20$st_loc == "Booue", "Bou_95_96", 
                                        ifelse(ovlap20$st_loc == "Mayibout", "May_95_96", 
                                               ifelse(ovlap20$st_loc == "Olloba01", "Ol1_01_02", 
                                                      ifelse(ovlap20$st_loc == "Olloba01" & ovlap20$near_loc == "Olloba02", "Ol1_02_03", 
                                                             ifelse(ovlap20$st_loc == "Olloba02"& ovlap20$near_loc == "Olloba01", "Ol2_01_02", 
                                                                    ifelse(ovlap20$st_loc == "Olloba02", "Ol2_02_03", 
                                                                           ifelse(ovlap20$st_loc == "Ekata", "Eka_01_02", 
                                                                                  ifelse(ovlap20$st_loc == "Mbandza", "Mbz_02_03", 
                                                                                         ifelse(ovlap20$st_loc == "Mvoula", "Mvo_02_03", 
                                                                                                ifelse(ovlap20$st_loc == "Odzala", "Odz_04_05", 
                                                                                                       ifelse(ovlap20$st_loc == "Luebo", "Lue_06_07", 
                                                                                                              ifelse(ovlap20$st_loc == "Boende", "Boe_13_14", 
                                                                                                                     ifelse(ovlap20$st_loc == "Likati", "Lik_16_17", 
                                                                                                                            ifelse(ovlap20$st_loc == "Bikoro", "Bik_17_18", 
                                                                                                                                   ifelse(ovlap20$st_loc == "Nkivu", "Nki_17_18", 
                                                                                                                                          ifelse(ovlap20$st_loc == "Mbandaka20" & ovlap20$near_loc == "Mbandaka22", "Mb1_19_20", 
                                                                                                                                                 ifelse(ovlap20$st_loc == "Mbandaka20", "Mb1_21_22", 
                                                                                                                                                        ifelse(ovlap20$st_loc == "Mbandaka22", "Mb2_19_20", 
                                                                                                                                                               ifelse(ovlap20$st_loc == "Mbandaka22", "Mb2_21_22", 
                                                                                                                                                                      NA )))))))))))))))))))))

ovlap20$overlapspill<- ifelse(ovlap20$near_loc == "Mekouka", "Mek_93_94", 
                              ifelse(ovlap20$near_loc == "Kikwit", "Kik_94_95", 
                                     ifelse(ovlap20$near_loc == "Booue", "Bou_95_96", 
                                            ifelse(ovlap20$near_loc == "Mayibout", "May_95_96", 
                                                   ifelse(ovlap20$near_loc == "Olloba01", "Ol1_01_02", 
                                                                        ifelse(ovlap20$near_loc == "Olloba02", "Ol2_02_03", 
                                                                               ifelse(ovlap20$near_loc == "Ekata", "Eka_01_02", 
                                                                                      ifelse(ovlap20$near_loc == "Mbandza", "Mbz_02_03", 
                                                                                             ifelse(ovlap20$near_loc == "Mvoula", "Mek_02_03", 
                                                                                                    ifelse(ovlap20$near_loc == "Odzala", "Odz_04_05", 
                                                                                                           ifelse(ovlap20$near_loc == "Luebo", "Lue_06_07", 
                                                                                                                  ifelse(ovlap20$near_loc == "Boende", "Boe_13_14", 
                                                                                                                         ifelse(ovlap20$near_loc == "Likati", "Lik_16_17", 
                                                                                                                                ifelse(ovlap20$near_loc == "Bikoro", "Bik_17_18", 
                                                                                                                                       ifelse(ovlap20$near_loc == "Nkivu", "Nki_17_18", 
                                                                                                                                              ifelse(ovlap20$near_loc == "Mbandaka20", "Mb1_19_20", 
                                                                                                                                                     ifelse(ovlap20$near_loc == "Mbandaka22", "Mb2_21_22", 
                                                                                                                                                            NA )))))))))))))))))

ovlap20$ovlsp<- substr(ovlap20$spillone, 1,3)
ovlap20$ovlyr<- substr(ovlap20$overlapspill, 5,9)
ovlap20$overlapspillyr<- paste(ovlap20$ovlsp,ovlap20$ovlyr, sep="_" )

laps<- unique(ovlap20$overlapspillyr)

#######################.  #######################.  #######################
############################### PRECIPITATION #######################
#######################  #######################. #######################

#run rain DTW in a loop 
rqdf.list<- list()
for (i in 1:(length(r))) {
  Q<- r[[i]]
  ss<- dtw_disvec(Q, r,  step_pattern = "symmetric2", normalize = TRUE,dist_method = "norm2", ws = NULL)
  ss$labels<- l$labs
  rqdf<- as.data.frame(cbind(ss$disvec, ss$labels))
  rqdf$V1<- as.numeric(rqdf$V1)
  rqdf.list<- append(rqdf.list, list(rqdf))
}


rqdf.list <- Map(cbind, rqdf.list, QTS = labs)
rqdf<- do.call("rbind", rqdf.list)

colnames(rqdf)[c(1,2)] = c("cost", "CTS")


##########  candidate and query spillover years
rqdf$Qspillyr<- ifelse(rqdf$QTS %in% laps, "S", "N")
rqdf$Cspillyr<- ifelse(rqdf$CTS %in% laps, "S", "N")

#define the spillover year comparison
rqdf$pair<- paste(rqdf$Qspillyr, rqdf$Cspillyr, sep= "-")
rqdf$pair<- factor(rqdf$pair, levels =  c("S-S", "S-N", "N-S", "N-N"))

#get the candidate and query spillover location
rqdf$Qspill<- substr(rqdf$QTS, 1,3)
rqdf$Cspill<- substr(rqdf$CTS, 1,3)

#identify within vs between location costs 
rqdf$loc<- ifelse(rqdf$Qspill == rqdf$Cspill, "Within", "Between")
rqdf$subp<- substr(rqdf$loc, 1,1)

#remove time series matching with themselves 
rqdf_nomatch<- subset(rqdf, cost > 0)
table(rqdf_nomatch$pair, rqdf_nomatch$Qspill)

######### within location only costs 
spill_non_finalq<- subset(rqdf_nomatch, loc == "Within" & pair != "N-S" )

spill_non_finalq$type<- paste(spill_non_finalq$Qspill, spill_non_finalq$pair, sep = "_")

##remove the spillover-spillover pairs
r<- subset(spill_non_finalq, pair != "S-S" )

#permutation test
n_perm <- 10000
perm_diffs <- numeric(n_perm)


results_list <- list() # To store results for each location
#run a loop of permutation tests, mixing up which costs are assigned S-N or N-N
diff_list<- list()
for (loc in 1:length(unique(r$Qspill))) {
  location<- unique(r$Qspill)[loc]
  spills<- subset(r, Qspill == location )
  observed_diff <- mean(spills$cost[spills$pair == "S-N"]) - mean(spills$cost[spills$pair == "N-N"])
  diff_list[[loc]] <- observed_diff
  
  for (i in 1:n_perm) {
    perm_group<- sample(spills$pair) # Shuffle group assignments
    perm_data <- data.frame(cost = spills$cost, group = perm_group)
    perm_diffs[i] <- mean(perm_data$cost[perm_data$group == "S-N"]) - mean(perm_data$cost[perm_data$group == "N-N"])
    # results_list[[loc]] <- perm_p_value
    
  }
  
  #store p values in a list 
  results_list[[loc]] <- sum(abs(perm_diffs) >= abs(observed_diff)) / n_perm
  
  print(paste("Observed difference:", round(observed_diff, 3)))
  
  #plot it to check 
  hist(perm_diffs, main = paste("Permutation Distribution of Mean Differences in Rain Costs", sep = "_"),
       xlab = "Difference in Means",  border = "white", xlim= c(-1,1))
  abline(v = observed_diff, col = "red", lwd = 2, lty = 2)
  
}

final_results_df <- do.call(rbind, results_list)
diff_df <- do.call(rbind, diff_list)

s<- as.data.frame(unique (r$Qspill))
s$pval_rain_costs<- final_results_df
s$rain_obs_diff<- diff_df
table(spill_non_finalq$type3)

#label the comparisons for boxplot
spill_non_finalq$type3<- factor(spill_non_finalq$type, levels = c("Mek_N-N", "Mek_S-N", "Mek_S-S", "Kik_N-N", "Kik_S-N", "Kik_S-S", "May_N-N", "May_S-N", "May_S-S", "Bou_N-N", "Bou_S-N", "Bou_S-S",  "Eka_N-N","Eka_S-N", "Eka_S-S",  "Ol1_N-N","Ol1_S-N", "Ol2_N-N", "Ol2_S-N","Ol1_S-S" ,"Ol2_S-S" ,"Mvo_N-N","Mvo_S-N", "Mvo_S-S",  "Mbz_N-N","Mbz_S-N", "Mbz_S-S",  "Odz_N-N", "Odz_S-N","Odz_S-S",    "Lue_N-N","Lue_S-N", "Lue_S-S",   "Boe_N-N","Boe_S-N","Boe_S-S",  "Lik_N-N","Lik_S-N", "Lik_S-S",  "Bik_N-N","Bik_S-N", "Bik_S-S", "Nki_N-N","Nki_S-N","Nki_S-S",  "Mb1_N-N" ,"Mb1_S-N",  "Mb2_N-N", "Mb2_S-N","Mb1_S-S","Mb2_S-S"  ))


p1<- spill_non_finalq %>%
  # filter(pair != "S-S") %>%
  ggplot() +
  geom_boxplot(aes( x = type3, y = cost, fill = pair), notch = F) +
  labs(y = "Within Loc. Rain Alignment Cost 20k Buff", x = "Query-Candidate Pair", color =  "Query Spillover", fill = "Query Spillover" )+
  #scale_fill_manual(values = c("#66C2A6", "#FC8E62", "#8EA0CB", "#E78AC3", "#A6D953" ,'green4', "#FFDA2E")) +
  scale_fill_manual(values = c("purple4", "#c77dff" ,"grey84")) +
  theme_minimal(base_size = 10) +
  # xlim(0,35)+
  #ylim(5,25)+
  #facet_wrap(~loc, nrow = 1) +
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text(angle = 55, hjust =1, vjust =1))
p1






 
#######################.  #######################.  #######################
############################### TEMPERATURE #######################
#######################  #######################. #######################
#run temperature DTW in a loop 
tqdf.list<- list()
for (i in 1:(length(t))) {
  Q<- t[[i]]
  ss<- dtw_disvec(Q, t,  step_pattern = "symmetric2", normalize = TRUE,dist_method = "norm2", ws = NULL)
  ss$labels<- l$labs
  tqdf<- as.data.frame(cbind(ss$disvec, ss$labels))
  tqdf$V1<- as.numeric(tqdf$V1)
  tqdf.list<- append(tqdf.list, list(tqdf))
}


tqdf.list <- Map(cbind, tqdf.list, QTS = labs)
tqdf<- do.call("rbind", tqdf.list)

colnames(tqdf)[c(1,2)] = c("cost", "CTS")
tqdf$Qspillyr<- ifelse(tqdf$QTS %in% laps, "S", "N")

table(tqdf$QTS)
tqdf$Cspillyr<- ifelse(tqdf$CTS %in% laps, "S", "N")

tqdf$pair<- paste(tqdf$Qspillyr, tqdf$Cspillyr, sep= "-")
tqdf$pair<- factor(tqdf$pair, levels =  c("S-S", "S-N", "N-S", "N-N"))

tqdf$Qspill<- substr(tqdf$QTS, 1,3)
tqdf$Cspill<- substr(tqdf$CTS, 1,3)

tqdf$loc<- ifelse(tqdf$Qspill == tqdf$Cspill, "Within", "Between")
tqdf$subp<- substr(tqdf$loc, 1,1)


tqdf_nomatch<- subset(tqdf, cost > 0)
table(tqdf_nomatch$pair, tqdf_nomatch$Qspill)


######### within location only costs 
spill_non_finalq<- subset(tqdf_nomatch, loc == "Within" & pair != "N-S" )
spill_non_finalq$type<- paste(spill_non_finalq$Qspill, spill_non_finalq$pair, sep = "_")

table(spill_non_finalq$type)

##clean more
t<- subset(spill_non_finalq, pair != "S-S" )
table(t$pair)

#permutation test
n_perm <- 10000
perm_diffs <- numeric(n_perm)


results_list <- list() # To store results for each location
diff_list<- list()
for (loc in 1:length(unique(t$Qspill))) {
  location<- unique(t$Qspill)[loc]
  spills<- subset(t, Qspill == location )
  observed_diff <- mean(spills$cost[spills$pair == "S-N"]) - mean(spills$cost[spills$pair == "N-N"])
  diff_list[[loc]] <- observed_diff
  for (i in 1:n_perm) {
    perm_group<- sample(spills$pair) # Shuffle group assignments
    perm_data <- data.frame(cost = spills$cost, group = perm_group)
    perm_diffs[i] <- mean(perm_data$cost[perm_data$group == "S-N"]) - mean(perm_data$cost[perm_data$group == "N-N"])
    # results_list[[loc]] <- perm_p_value
    
  }
  
  #store p values in a list 
  results_list[[loc]] <- sum(abs(perm_diffs) >= abs(observed_diff)) / n_perm
  
  print(paste("Observed difference:", round(observed_diff, 3)))
  
  #plot it to check 
  hist(perm_diffs, main = paste("Permutation Distribution of Mean Differences in Temp Costs", sep = "_"),
       xlab = "Difference in Means",  border = "white", xlim= c(-0.05,0.05))
  abline(v = observed_diff, col = "red", lwd = 2, lty = 2)
  
}

final_results_df <- do.call(rbind, results_list)
#s<- as.data.frame(unique (t$Qspill))
diff_df <- do.call(rbind, diff_list)

s$pval_temp_costs<- final_results_df
s$temp_obs_diff<- diff_df
colnames(s)<- c("Qspill", "pval_rain_costs", "rain_obs_diff", "pval_temp_costs", "temp_obs_diff")

spill_non_finalq$type3<- factor(spill_non_finalq$type, levels = c("Mek_N-N", "Mek_S-N", "Mek_S-S", "Kik_N-N", "Kik_S-N", "Kik_S-S", "May_N-N", "May_S-N", "May_S-S", "Bou_N-N", "Bou_S-N", "Bou_S-S",  "Eka_N-N","Eka_S-N", "Eka_S-S",  "Ol1_N-N","Ol1_S-N", "Ol2_N-N", "Ol2_S-N","Ol1_S-S" ,"Ol2_S-S" ,"Mvo_N-N","Mvo_S-N", "Mvo_S-S",  "Mbz_N-N","Mbz_S-N", "Mbz_S-S",  "Odz_N-N", "Odz_S-N","Odz_S-S",    "Lue_N-N","Lue_S-N", "Lue_S-S",   "Boe_N-N","Boe_S-N","Boe_S-S",  "Lik_N-N","Lik_S-N", "Lik_S-S",  "Bik_N-N","Bik_S-N", "Bik_S-S", "Nki_N-N","Nki_S-N","Nki_S-S",  "Mb1_N-N" ,"Mb1_S-N",  "Mb2_N-N", "Mb2_S-N","Mb1_S-S","Mb2_S-S"  ))




p1<- spill_non_finalq %>%
  # filter(pair != "S-S") %>%
  ggplot() +
  geom_boxplot(aes( x = type3, y = cost, fill = pair), notch = F) +
  #  geom_violin(aes( x = type, y = cost, fill = pair)) +
  labs(y = "Within Loc. Temp Alignment Cost 20k Buff", x = "Query-Candidate Pair", color =  "Query Spillover", fill = "Query Spillover" )+
  #scale_fill_manual(values = c("#66C2A6", "#FC8E62", "#8EA0CB", "#E78AC3", "#A6D953" ,'green4', "#FFDA2E")) +
  scale_fill_manual(values = c("purple4", "#c77dff" ,"grey84")) +
  theme_minimal(base_size = 10) +
  # xlim(0,35)+
  #ylim(5,25)+
  #facet_wrap(~loc, nrow = 1) +
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text(angle = 55, hjust =1, vjust =1))
#p<- p + stat_compare_means(comparisons = my_comparisons, hide.ns = TRUE, label = "p.signif", method = "wilcox.test") 
p1


ggsave("withinloc_temp_aligncost_20kbuff_boxplots_chrn_20250903.svg", plot = last_plot(), height=4, width = 10, units = "in", dpi = 200 )


#######################.  #######################.  #######################
############################### EVI #######################
#######################  #######################. #######################

all2<- subset(all, newprd2 != "1990_1991" &newprd2 != "1991_1992" & newprd2 != "1992_1993"& newprd2 != "1993_1994" & newprd2 != "1994_1995" & newprd2 != "1995_1996" & newprd2 != "1996_1997" & newprd2 != "1997_1998" & newprd2 != "1998_1999"& newprd2 != "1999_2000"   )


buf20_e<- subset(all2, buffer == "20")

#take out spillovers that happen outside of the EVI dataset 
buf20_e2<- subset(buf20_e, Location != "Mekouka94" & Location != 'Kikwit95'& Location != "Booue96" & Location != 'Mayibout96'  )

table(buf20_e2$Location, buf20_e2$mon_ab)

buf20_e2<- buf20_e2 %>%
  arrange(rolling_num)
buf20_e2<- buf20_e2 %>%
  arrange(newprd2)
buf20_e2<-buf20_e2%>%
  arrange(Location)


e<- split(buf20_e2$spmean_EVI, ceiling(seq_along(buf20_e2$spmean_EVI) / 23))

###
labs<- rep(c("Bik", "Boe", "Eka", "Lik","Lue",  "Mb1", "Mb2", "Mbz", "Mvo",  "Nki", "Odz", "Ol1", "Ol2"), each=22)

yrs<-as.character(replicate(13,c("00_01","01_02","02_03","03_04","04_05","05_06","06_07","07_08", "08_09","09_10","10_11","11_12", "12_13","13_14","14_15","15_16","16_17","17_18", "18_19","19_20","20_21","21_22")))

l<- as.data.frame(cbind(labs,yrs))
l$labs<- paste(l$labs, l$yrs, sep = "_")
labs<- l$labs

#run EVI DTW in a loop 
eqdf.list<- list()
for (i in 1:(length(e))) {
  Q<- e[[i]]
  ss<- dtw_disvec(Q, e,  step_pattern = "symmetric2", normalize = TRUE,dist_method = "norm2", ws = NULL)
  ss$labels<- l$labs
  eqdf<- as.data.frame(cbind(ss$disvec, ss$labels))
  eqdf$V1<- as.numeric(eqdf$V1)
  eqdf.list<- append(eqdf.list, list(eqdf))
}


eqdf.list <- Map(cbind, eqdf.list, QTS = labs)
eqdf<- do.call("rbind", eqdf.list)

colnames(eqdf)[c(1,2)] = c("cost", "CTS")
eqdf$Qspillyr<- ifelse(eqdf$QTS %in% laps, "S", "N")

table(eqdf$QTS)
eqdf$Cspillyr<- ifelse(eqdf$CTS %in% laps, "S", "N")




eqdf$pair<- paste(eqdf$Qspillyr, eqdf$Cspillyr, sep= "-")
eqdf$pair<- factor(eqdf$pair, levels =  c("S-S", "S-N", "N-S", "N-N"))

eqdf$Qspill<- substr(eqdf$QTS, 1,3)
eqdf$Cspill<- substr(eqdf$CTS, 1,3)

eqdf$loc<- ifelse(eqdf$Qspill == eqdf$Cspill, "Within", "Between")
eqdf$subp<- substr(eqdf$loc, 1,1)

eqdf_nomatch<- subset(eqdf, cost > 0)
table(eqdf_nomatch$pair, eqdf_nomatch$Qspill)


######### within location only costs 
spill_non_finalq<- subset(eqdf_nomatch, loc == "Within" & pair != "N-S" )

spill_non_finalq$type<- paste(spill_non_finalq$Qspill, spill_non_finalq$pair, sep = "_")

table(spill_non_finalq$type)

##clean more
e<- subset(spill_non_finalq, pair != "S-S" )
table(e$pair)

#permutation test
n_perm <- 10000
perm_diffs <- numeric(n_perm)


results_list <- list() # To store results for each location
diff_list<- list()
for (loc in 1:length(unique(e$Qspill))) {
  location<- unique(e$Qspill)[loc]
  spills<- subset(e, Qspill == location )
  observed_diff <- mean(spills$cost[spills$pair == "S-N"]) - mean(spills$cost[spills$pair == "N-N"])
  diff_list[[loc]] <- observed_diff
  for (i in 1:n_perm) {
    perm_group<- sample(spills$pair) # Shuffle group assignments
    perm_data <- data.frame(cost = spills$cost, group = perm_group)
    perm_diffs[i] <- mean(perm_data$cost[perm_data$group == "S-N"]) - mean(perm_data$cost[perm_data$group == "N-N"])
    # results_list[[loc]] <- perm_p_value
    
  }
  
  #store p values in a list 
  results_list[[loc]] <- sum(abs(perm_diffs) >= abs(observed_diff)) / n_perm
  
  print(paste("Observed difference:", round(observed_diff, 3)))
  
  #plot it to check 
  hist(perm_diffs, main = paste("Permutation Distribution of Mean Differences in EVI Costs", sep = "_"),
       xlab = "Difference in Means",  border = "white", xlim= c(-0.006,0.006))
  abline(v = observed_diff, col = "red", lwd = 2, lty = 2)
  
}

final_results_df <- do.call(rbind, results_list)
diff_df <- do.call(rbind, diff_list)

s2<- as.data.frame(unique (e$Qspill))
s2$pval_evi_costs<- final_results_df
s2$evi_obs_diff<- diff_df

colnames(s2)<- c("Qspill", "pval_evi_costs", "evi_obs_diff")



spill_non_finalq$type3<- factor(spill_non_finalq$type, levels = c("Mek_N-N", "Mek_S-N", "Mek_S-S", "Kik_N-N", "Kik_S-N", "Kik_S-S", "May_N-N", "May_S-N", "May_S-S", "Bou_N-N", "Bou_S-N", "Bou_S-S",  "Eka_N-N","Eka_S-N", "Eka_S-S",  "Ol1_N-N","Ol1_S-N", "Ol2_N-N", "Ol2_S-N","Ol1_S-S" ,"Ol2_S-S" ,"Mvo_N-N","Mvo_S-N", "Mvo_S-S",  "Mbz_N-N","Mbz_S-N", "Mbz_S-S",  "Odz_N-N", "Odz_S-N","Odz_S-S",    "Lue_N-N","Lue_S-N", "Lue_S-S",   "Boe_N-N","Boe_S-N","Boe_S-S",  "Lik_N-N","Lik_S-N", "Lik_S-S",  "Bik_N-N","Bik_S-N", "Bik_S-S", "Nki_N-N","Nki_S-N","Nki_S-S",  "Mb1_N-N" ,"Mb1_S-N",  "Mb2_N-N", "Mb2_S-N","Mb1_S-S","Mb2_S-S"  ))




p1<- spill_non_finalq %>%
  # filter(pair != "S-S") %>%
  ggplot() +
  geom_boxplot(aes( x = type3, y = cost, fill = pair), notch = F) +
  labs(y = "Within Loc. EVI Alignment Cost 20k Buff", x = "Query-Candidate Pair", color =  "Query Spillover", fill = "Query Spillover" )+
  scale_fill_manual(values = c("purple4", "#c77dff" ,"grey84")) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text(angle = 55, hjust =1, vjust =1))
p1


#######################.  #######################.  #######################
############################### MULTIVARIATE DTW #######################
#######################  #######################. #######################

e<- split(buf20_e2$spmean_EVI, ceiling(seq_along(buf20_e2$spmean_EVI) / 23))
r<- split(buf20_e2$spmean_rain, ceiling(seq_along(buf20_e2$spmean_rain) / 23))
t<- split(buf20_e2$dtrnd_temp, ceiling(seq_along(buf20_e2$dtrnd_temp) / 23))
env<- Map(cbind, r,t,e)

#run multivariate DTW in a loop 
mvqdf.list<- list()
for (i in 1:(length(env))) {
  Q<- env[[i]]
  ss<- dtw_disvec(Q, env,  step_pattern = "symmetric2", normalize = TRUE,dist_method = "norm2", ws = NULL)
  ss$labels<- l$labs
  mvqdf<- as.data.frame(cbind(ss$disvec, ss$labels))
  mvqdf$V1<- as.numeric(mvqdf$V1)
  mvqdf.list<- append(mvqdf.list, list(mvqdf))
}


mvqdf.list <- Map(cbind, mvqdf.list, QTS = labs)
mvqdf<- do.call("rbind", mvqdf.list)

colnames(mvqdf)[c(1,2)] = c("cost", "CTS")
mvqdf$Qspillyr<- ifelse(mvqdf$QTS %in% laps, "S", "N")

table(mvqdf$QTS)
mvqdf$Cspillyr<- ifelse(mvqdf$CTS %in% laps, "S", "N")



mvqdf$pair<- paste(mvqdf$Qspillyr, mvqdf$Cspillyr, sep= "-")
mvqdf$pair<- factor(mvqdf$pair, levels =  c("S-S", "S-N", "N-S", "N-N"))

mvqdf$Qspill<- substr(mvqdf$QTS, 1,3)
mvqdf$Cspill<- substr(mvqdf$CTS, 1,3)

mvqdf$loc<- ifelse(mvqdf$Qspill == mvqdf$Cspill, "Within", "Between")
mvqdf$subp<- substr(mvqdf$loc, 1,1)

mvqdf_nomatch<- subset(mvqdf, cost > 0)
table(mvqdf_nomatch$pair, mvqdf_nomatch$Qspill)


######### within location only costs 
spill_non_finalq<- subset(mvqdf_nomatch, loc == "Within" & pair != "N-S" )

spill_non_finalq$type<- paste(spill_non_finalq$Qspill, spill_non_finalq$pair, sep = "_")

table(spill_non_finalq$type)

##clean more
mv<- subset(spill_non_finalq, pair != "S-S" )
table(mv$pair)

#permutation test
n_perm <- 10000
perm_diffs <- numeric(n_perm)


results_list <- list() # To store results for each location
diff_list<- list()
for (loc in 1:length(unique(mv$Qspill))) {
  location<- unique(mv$Qspill)[loc]
  spills<- subset(mv, Qspill == location )
  observed_diff <- mean(spills$cost[spills$pair == "S-N"]) - mean(spills$cost[spills$pair == "N-N"])
  diff_list[[loc]] <- observed_diff
  for (i in 1:n_perm) {
    perm_group<- sample(spills$pair) # Shuffle group assignments
    perm_data <- data.frame(cost = spills$cost, group = perm_group)
    perm_diffs[i] <- mean(perm_data$cost[perm_data$group == "S-N"]) - mean(perm_data$cost[perm_data$group == "N-N"])
    # results_list[[loc]] <- perm_p_value
    
  }
  
  #store p values in a list 
  results_list[[loc]] <- sum(abs(perm_diffs) >= abs(observed_diff)) / n_perm
  
  print(paste("Observed difference:", round(observed_diff, 3)))
  
  #plot it to check 
  hist(perm_diffs, main = paste("Permutation Distribution of Mean Differences in MV Costs", sep = "_"),
       xlab = "Difference in Means",  border = "white", xlim= c(-2,2))
  abline(v = observed_diff, col = "red", lwd = 2, lty = 2)
  
}

final_results_df <- do.call(rbind, results_list)
diff_df <- do.call(rbind, diff_list)

#s2<- as.data.frame(unique (mv$Qspill))
s2$pval_MV_costs<- final_results_df
s2$MV_obs_diff<- diff_df

colnames(s2)<- c("Qspill","pval_evi_costs", "evi_obs_diff",  "pval_MV_costs", "MV_obs_diff")



spill_non_finalq$type3<- factor(spill_non_finalq$type, levels = c("Eka_N-N","Eka_S-N", "Eka_S-S",  "Ol1_N-N","Ol1_S-N", "Ol2_N-N", "Ol2_S-N","Ol1_S-S" ,"Ol2_S-S" ,"Mvo_N-N","Mvo_S-N", "Mvo_S-S",  "Mbz_N-N","Mbz_S-N", "Mbz_S-S",  "Odz_N-N", "Odz_S-N","Odz_S-S",    "Lue_N-N","Lue_S-N", "Lue_S-S",   "Boe_N-N","Boe_S-N","Boe_S-S",  "Lik_N-N","Lik_S-N", "Lik_S-S",  "Bik_N-N","Bik_S-N", "Bik_S-S", "Nki_N-N","Nki_S-N","Nki_S-S",  "Mb1_N-N" ,"Mb1_S-N",  "Mb2_N-N", "Mb2_S-N","Mb1_S-S","Mb2_S-S"  ))



p1<- spill_non_finalq %>%
  # filter(pair != "S-S") %>%
  ggplot() +
  geom_boxplot(aes( x = type3, y = cost, fill = pair), notch = F) +
  #  geom_violin(aes( x = type, y = cost, fill = pair)) +
  labs(y = "Within Loc. MV Alignment Cost 20k Buff", x = "Query-Candidate Pair", color =  "Query Spillover", fill = "Query Spillover" )+
  #scale_fill_manual(values = c("#66C2A6", "#FC8E62", "#8EA0CB", "#E78AC3", "#A6D953" ,'green4', "#FFDA2E")) +
  scale_fill_manual(values = c("purple4", "#c77dff" ,"grey84")) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text(angle = 55, hjust =1, vjust =1))
p1



################## mash it all together 

final_pvals<- merge(s, s2, by = "Qspill", all.x = T)

final_pvals$rain_sig<- ifelse(final_pvals$pval_rain_costs <= 0.001, "***",
                              ifelse(final_pvals$pval_rain_costs <= 0.01, "**", 
                                     ifelse(final_pvals$pval_rain_costs <= 0.05, "*", "NS")))

final_pvals$temp_sig<- ifelse(final_pvals$pval_temp_costs <= 0.001, "***",
                              ifelse(final_pvals$pval_temp_costs <= 0.01, "**", 
                                     ifelse(final_pvals$pval_temp_costs <= 0.05, "*", "NS")))

final_pvals$evi_sig<- ifelse(final_pvals$pval_evi_costs <= 0.001, "***",
                             ifelse(final_pvals$pval_evi_costs <= 0.01, "**", 
                                    ifelse(final_pvals$pval_evi_costs <= 0.05, "*", "NS")))
final_pvals$mv_sig<- ifelse(final_pvals$pval_MV_costs <= 0.001, "***",
                             ifelse(final_pvals$pval_MV_costs <= 0.01, "**", 
                                    ifelse(final_pvals$pval_MV_costs <= 0.05, "*", "NS")))


