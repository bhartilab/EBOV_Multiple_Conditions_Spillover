setwd(".")
library(plyr)
library(dplyr)
library(ggplot2)
library(bife)
library(tidyr)
library(pROC)

land<- read.csv("allspills_allbuffs_CCIland_20250828.csv", header = T)
forest<- subset(land, Value >= 50 & Value <= 90 | Value == 160 | Value == 170 )

forest_sum<- forest %>%
  group_by(Location, year, buffer) %>%
  dplyr::summarise(forest_sum_ha = sum(area_ha)) %>%
  as.data.frame()

l<- forest_sum %>%
  pivot_wider(names_from = buffer, values_from = forest_sum_ha)

colnames(l)=  c("Location" , "year", "forst_ha_buff100", "forst_ha_buff200", "forst_ha_buff20", "forst_ha_buff300", "forst_ha_buff50")


forest_sum <- l %>%
  arrange(Location, year) %>%
  group_by(Location) %>%
  dplyr::mutate(diff_ha_forst_20k_1yr= forst_ha_buff20 - dplyr::lag(forst_ha_buff20),
                diff_ha_forst_20k_2yr= forst_ha_buff20 - dplyr::lag(forst_ha_buff20, 2),
                diff_ha_forst_50k_1yr= forst_ha_buff50 - dplyr::lag(forst_ha_buff50),
                diff_ha_forst_50k_2yr= forst_ha_buff50 - dplyr::lag(forst_ha_buff50, 2),
                diff_ha_forst_100k_1yr= forst_ha_buff100 - dplyr::lag(forst_ha_buff100),
                diff_ha_forst_100k_2yr= forst_ha_buff100 - dplyr::lag(forst_ha_buff100, 2),
                diff_ha_forst_200k_1yr= forst_ha_buff200 - dplyr::lag(forst_ha_buff200),
                diff_ha_forst_200k_2yr= forst_ha_buff200 - dplyr::lag(forst_ha_buff200, 2),
                diff_ha_forst_300k_1yr= forst_ha_buff300 - dplyr::lag(forst_ha_buff300),
                diff_ha_forst_300k_2yr= forst_ha_buff300 - dplyr::lag(forst_ha_buff300, 2)) %>%
  as.data.frame()


forest_sum$yrfac<- as.factor(forest_sum$year)
table(forest_sum$Location, forest_sum$year)

forest_sum$spillprd<- paste(forest_sum$Location, forest_sum$year, sep = "_" )


#### find the overlapping spillover buffers that share the same characteristics 
all_overlap<-read.csv("alloverlaps_allpcts_buffoverlap_20250901.csv")

ovlap20<- subset(all_overlap, overlap_percent20 >= 49 ) #get buffers that overlap more than 300% and remove self matches
table(ovlap20$st_loc)

ovlap20$spillprd <- ifelse(ovlap20$st_loc == "Mekouka", "Mekouka_1993", 
                           ifelse(ovlap20$st_loc == "Kikwit", "Kikwit_1994", 
                                  ifelse(ovlap20$st_loc == "Booue", "Booue_1995", 
                                         ifelse(ovlap20$st_loc == "Mayibout", "Mayibout_1995", 
                                                ifelse(ovlap20$st_loc == "Olloba01" & ovlap20$near_loc == "Olloba02", "Olloba_2000", 
                                                       ifelse(ovlap20$st_loc == "Olloba02" & ovlap20$near_loc == "Olloba02", "Olloba_2001", 
                                                              ifelse(ovlap20$st_loc == "Olloba01", "Olloba_2000", 
                                                                     ifelse(ovlap20$st_loc == "Olloba02", "Olloba_2001", 
                                                                            ifelse(ovlap20$st_loc == "Ekata", "Ekata_2000", 
                                                                                   ifelse(ovlap20$st_loc == "Mbandza", "Mbandza_2002", 
                                                                                          ifelse(ovlap20$st_loc == "Mvoula", "Mvoula_2002", 
                                                                                                 ifelse(ovlap20$st_loc == "Odzala", "Odzala_2004", 
                                                                                                        ifelse(ovlap20$st_loc == "Luebo", "Luebo_2006", 
                                                                                                               ifelse(ovlap20$st_loc == "Boende", "Boende_2013", 
                                                                                                                      ifelse(ovlap20$st_loc == "Likati", "Likati_2016", 
                                                                                                                             ifelse(ovlap20$st_loc == "Bikoro", "Bikoro_2017", 
                                                                                                                                    ifelse(ovlap20$st_loc == "Nkivu", "Nkivu_2017", 
                                                                                                                                           ifelse(ovlap20$st_loc == "Mbandaka20" & ovlap20$near_loc == "Mbandaka20", "Mbandaka_2019", 
                                                                                                                                                  ifelse(ovlap20$st_loc == "Mbandaka20" & ovlap20$near_loc == "Mbandaka22", "Mbandaka_2021", 
                                                                                                                                                         ifelse(ovlap20$st_loc == "Mbandaka22" &  ovlap20$near_loc == "Mbandaka20", "Mbandaka_2019", 
                                                                                                                                                                ifelse(ovlap20$st_loc == "Mbandaka22"& ovlap20$near_loc == "Mbandaka22", "Mbandaka_2021",
                                                                                                                                                                       NA )))))))))))))))))))))



laps<- unique(ovlap20$spillprd)

forest_sum$spillyr<- ifelse(forest_sum$spillprd %in% laps, "S", "N")



########################## Example for forst loss in 20km buffer surrounding spillover locations
pdf <- plm::pdata.frame(forest_sum_cl, index=c("Location", "year"))
head(pdf)

predictor_vars <- c( "diff_ha_forst_20k_1yr","diff_ha_forst_20k_2yr")


target_var <- "spillyr"
fix_var<- "| Location"

# Get the number of predictor variables
n <- length(predictor_vars)
results20k_buff<- list()
results20k_sums <- list()
liklih_20k<- list()
formula_list<- list()
for (k in 1:n) {  # Iterate through combinations of variables
  combinations <- combn(predictor_vars, k)
  
  # Loop through each combination
  for (i in 1:ncol(combinations)) {
    # Create model formula
    current_vars <- combinations[, i]
    formula_str<- paste(target_var, "~", paste(current_vars, collapse = "+"), paste(fix_var))
    formula_nm <- as.formula(formula_str)
    formula_list<- c(formula_list, list(formula_nm))
    
    # Fit the model (example with lm)
    #model<- bife(as.formula(formula_str), data = pdf)
    model <- bife(formula_nm , data = pdf)
    liklih<- logLik(model)
    
    # Store the model or perform further analysis
    results20k_buff= c(results20k_buff,list(model))
    results20k_sums= c(results20k_sums,list(summary(model)))
    liklih_20k = c(liklih_20k,list(liklih))
  }
  
}


#predict full dataframe 
predicted_probabilities <- lapply(results20k_buff, function(model) predict(model))
roc_curves <- list()
roc_curve <- list()

for (i in seq_along(predicted_probabilities )) {
  roc_curves <- roc(response =results20k_buff[[1]][11]$data$spillyr, predictor = predicted_probabilities[[i]])
  roc_curve[[paste0("Model_", i)]] <- roc_curves
}

# Combine the ROC data for plotting with ggplot2
roc_data_list <- lapply(roc_curve, function(roc_obj) {
  data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities
  )
})

roc_data_df <- do.call(rbind, Map(cbind, model = names(roc_curve), roc_data_list))
roc_data_df$mod_num<- stringr::str_extract(roc_data_df$model, "[^_]+$")
roc_data_df$mod_num<-as.numeric(roc_data_df$mod_num)
range(roc_data_df$mod_num)


roc_data_df<- roc_data_df %>% 
  arrange(mod_num)

p20<- ggplot(roc_data_df, aes(x = FPR, y = TPR, color = as.factor(mod_num))) +
  geom_line() +
  geom_abline(linetype = "dashed", color = "gray") + # Reference line for random classifier
  labs(
    title = "ROC BIFE 20K Buffer",
    x = "False Positive Rate",
    y = "True Positive Rate",     color = "Model"
  ) +
  scale_color_viridis_d(option = "D")+
  theme_minimal(base_size =9)+
  theme(legend.position = 'bottom', axis.title = element_text(size = 8)) 
p20

auc_list20<-list()
for (i in seq_along(roc_curve)) {
  auc_value <- auc(roc_curve[[i]])
  message(paste("AUC for", names(roc_curve)[i], ":", round(auc_value, 4)))
  auc_list20<- c(auc_list20,list(auc_value))
}


# Inspect the predictions
preds<- unlist(predictions_lapply, recursive = F, use.names = T)
preds_df<- as.data.frame(do.call("rbind", predictions_lapply))

forms<- do.call("rbind", formula_list)
forms<-as.data.frame(forms)
preds_df$model<- forms$V3

predict_long <- pivot_longer(
  preds_df,
  cols = -model,
  names_to = "spills",             # New column for original column names
  values_to = "prediction"                       # New column for values
)

predict_long$prediction<- round(predict_long$prediction, digits = 4)

predict_long$prediction_yr<- rep(1994:2022, times = 36)
predict_long<- as.data.frame(predict_long)


predict_long$spillprd<- paste(predict_long$spills, predict_long$prediction_yr, sep = "_")
predict_long$spillyr<- ifelse(predict_long$spillprd %in% laps, "S", "N")
predict_long$model<- as.character(predict_long$model)

