library(data.table)
library(tidyverse)

df <- fread("Data/data_clean_mutated.csv", data.table = FALSE)
head(df)

### DATA VISUALIZATION (NO PROCESSING): NUMBER OF DISTINCT CPT NUMBERS BY PAYER CLASS
df %>% group_by(provider_id, name.y) %>% mutate(payerclass_count = n()) %>% 
  distinct(provider_id, name.y, .keep_all = TRUE) %>% ggplot(aes(x = payerclass_count)) + 
  geom_histogram(bins = 50) + facet_wrap(~name.y) + ylab("# Hospitals") + xlab("# CPT Codes")
###

## Provide CPT Labels for coarse categories
df <- df %>% mutate(code_categ = case_when(as.integer(cpt) %in% c(99202:99499) ~ "Evaluation & Management",
                                           as.integer(cpt) %in% c(100:1999) ~ "Anesthesia",
                                           as.integer(cpt) %in% c(10021:69990) ~ "Surgery",
                                           as.integer(cpt) %in% c(70010:79999) ~ "Radiology Procedures",
                                           as.integer(cpt) %in% c(80047:89398) ~ "Pathology and Laboratory Procedures",
                                           as.integer(cpt) %in% c(90281:99607) ~ "Medicine Services and Procedures") )

### DATA VISUALIZATION (NO PROCESSING): NUMBER OF DISTINCT CPT NUMBERS BY CATEGORY
df %>% filter(name.y == "Commercial") %>% group_by(provider_id, code_categ) %>% mutate(categ_count = n()) %>% 
  distinct(provider_id, code_categ, .keep_all = TRUE) %>% ggplot(aes(x = categ_count)) + geom_histogram(bins=25) + 
  facet_wrap(~code_categ) + ylab("# Hospitals") + xlab("# CPT Codes") + 
  ggtitle("Distribution of CPT codes by category (for Commercial)")
###




## State Adjustment
#For each CPT code in our sample, we calculate for each payer class (i.e., commercial insurance, self-pay, medicare, etc.) 
# the state median across all hospitals in our sample that are located in the same state.
# The reported hospital cpt rate in a given payer class is the ratio of the focal
# hospital's median rate divided by the state median.

df <- as.data.frame(df %>% group_by(name.y,cpt,state) %>% 
                      mutate(state_ins_median = h_ins_median/(median(h_ins_median)), 
                             state_ins_min = min(h_ins_median)), state_ins_max = max(h_ins_median))


# For each hospital, determine the variance in median prices across its reported shoppable 
# CPT codes against the respective variance for the same CPT codes of all 
# hospitals in the state - adjusted for payer class.

hospital.list <- df %>% distinct(provider_id,state) #retrieve provider_ids

state.list <- df %>% distinct(state) #retrieve distinct states
state.list <- state.list[,1]

df$hospital_basket_median <- NA
df$hospital_basket_iqr <- NA
df$hospital_basket_25 <- NA
df$hospital_basket_75 <- NA

df$hospital_basket_statematch_median <- NA
df$hospital_basket_statematch_iqr <- NA
df$hospital_basket_statematch_25 <- NA
df$hospital_basket_statematch_75 <- NA  


payer_types <- unique(df$name.y)

library(doParallel)
cores <- detectCores()
print(cores)
registerDoParallel(cores-1)
timestamp()

r <- foreach(p_i = payer_types, .combine = rbind) %dopar% {

for(s in state.list)
{
  h.list <- hospital.list %>% filter(state == s)
  for(h in h.list[,1])
  {
    retrieve.cpts <- df %>% filter(provider_id==h, name.y==p_i) %>% distinct(cpt)
    retrieve.cpts <- retrieve.cpts[,1]
    #calculate variance in State per selected CPT code:
    
    state.match <-  df %>% filter(cpt %in% retrieve.cpts, state==s, name.y== p_i) %>%  
      select(cpt,provider_id,  h_ins_median) %>% arrange(cpt) %>% group_by(cpt) %>% mutate(cpt.state.median = median(h_ins_median)) %>% 
      distinct(cpt, .keep_all = TRUE) %>%  ungroup() %>% mutate(match_state_basket_iqr = IQR(cpt.state.median), 
      match_state_basket_median = median(cpt.state.median), match_state_basket_25 = quantile(cpt.state.median, probs = 0.25), 
      match_state_basket_75 = quantile(cpt.state.median, probs = 0.75) )  %>% distinct(match_state_basket_iqr, match_state_basket_median, 
      match_state_basket_25, match_state_basket_75,.keep_all = T) %>% select(match_state_basket_iqr, match_state_basket_median, 
      match_state_basket_25, match_state_basket_75)
    
    
    
    
    # Calculate basket variance of focal hospital 'h'
    h_median <-  df %>% filter(provider_id==h, name.y== p_i) %>% mutate(h_med = median(h_ins_median) ) %>% pull(h_med )
    h_median <- h_median[1]
    h_iqr_value <- df %>% filter(provider_id==h, name.y== p_i) %>% mutate(  h_iqr = IQR(h_ins_median)) %>% pull(  h_iqr) 
    h_iqr_value <- h_iqr_value[1]     
    h_25_value <- df %>% filter(provider_id==h, name.y== p_i) %>% mutate(  h_25 = quantile(h_ins_median, probs = 0.25)) %>% pull(  h_25) 
    h_25_value <- h_25_value[1]
    h_75_value <- df %>% filter(provider_id==h, name.y== p_i) %>% mutate(  h_75 = quantile(h_ins_median, probs = 0.75)) %>% pull(  h_75) 
    h_75_value <- h_75_value[1]
    
    df$hospital_basket_median[df$provider_id==h & df$name.y== p_i] <- h_median
    df$hospital_basket_iqr[df$provider_id==h & df$name.y== p_i] <- h_iqr_value
    df$hospital_basket_25[df$provider_id==h & df$name.y== p_i] <- h_25_value
    df$hospital_basket_75[df$provider_id==h & df$name.y== p_i] <- h_75_value
    
    
    df$hospital_basket_statematch_median[df$provider_id==h & df$name.y== p_i] <- as.integer(state.match[1,2])
    df$hospital_basket_statematch_iqr[df$provider_id==h & df$name.y== p_i] <- as.integer(state.match[1,1])
    df$hospital_basket_statematch_25[df$provider_id==h & df$name.y== p_i] <- as.integer(state.match[1,3])
    df$hospital_basket_statematch_75[df$provider_id==h & df$name.y== p_i] <- as.integer(state.match[1,4])
    
    
  }
  
}
  df <- df %>% filter(name.y == p_i)
}




# In a next step, we aggregate the cpt median rate ratios per hospital into one hospital-level rate, 
# that is, it shows the median for all reported CPT codes in a hospital for a particular payer class 
# (i.e., commercial insurance, self-pay, etc.).

df.agg <- as.data.frame(r %>% group_by(provider_id, name.y) %>% mutate(h_aggregate_median = median(state_ins_median), 
          h_aggregate_mean = mean(state_ins_median), h_aggregate_min = min(state_ins_median), 
          h_aggregate_max = max(state_ins_median), h_aggregate_sd = sd(state_ins_median) ) ) 

df.agg <- df.agg %>% distinct(provider_id, name.y, .keep_all = TRUE)


#r %>% filter(cpt == 80048, provider_id == 4993, plan_id == 110814 )
write_csv(df.agg, "Data/data_set.csv")
