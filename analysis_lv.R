# File used to generate images and result tables 
# (the tables were combined manually to get the table in the paper).
# You can change the directory where the files are saved by changing `dir_path`.

library(tidyverse) # General R work
library(ggplot2) # Plotting
library(stringr) # Regex

source("functions.R")

# PARAMETERS ----
n2_to_n1 <- 1/9 # 10-fold CV
dir_path <- "images/analysis_lv/"

# ANALYSIS ----
for (data_id in c(271, 326, 332)) {
  diff <- readRDS(file = paste0("data_", data_id, "_diff.RDS"))
  
  data_code <- case_when(data_id == 271 ~ "E",
                         data_id == 326 ~ "L",
                         data_id == 332 ~ "P",
                         TRUE ~ "Incorrect code!")
  
  for (measure in c("AUC", "QS")) {
    diff %>% 
      filter(str_detect(name, pattern = "~")) %>% 
      mutate(subsampling = str_extract(name, pattern = "^.*(?=~)"),
             name = str_extract(name, pattern = "(?<=~).*$")) %>% 
      mutate(subsampling = case_when(subsampling == "control" ~ "kontrole",
                                     subsampling == "up" ~ "pār-izlase",
                                     subsampling == "smote" ~ "SMOTE",
                                     TRUE ~ "Incorrect subsampling name!")) %>% 
      filter(name == measure) %>% 
      rename(Metode = subsampling) %>% 
      ggplot(aes(y = value, x = Metode, fill = Metode)) +
      geom_boxplot() +
      facet_wrap(~ type) +
      theme_bw() +
      labs(y = measure) +
      scale_fill_manual(values=c("#999999", "#1f78b4", "#33a02c"), guide = FALSE)
    ggsave(filename = paste0(dir_path, paste("data", data_id, measure, "values.png", sep = "_")), 
           device = "png", width = 8, height = 4)
    
    diff %>% 
      filter(str_detect(name, pattern = "diff")) %>% 
      mutate(subsampling = str_extract(name, pattern = "(?<=diff_).*$"),
             name = str_extract(name, pattern = "^.*(?=_diff)")) %>% 
      mutate(subsampling = case_when(subsampling == "control" ~ "kontrole",
                                     subsampling == "up" ~ "pār-izlase",
                                     subsampling == "smote" ~ "SMOTE",
                                     TRUE ~ "Incorrect subsampling name!")) %>% 
      filter(name == measure) %>% 
      rename(Metode = subsampling) %>%
      ggplot(aes(y = value, x = Metode, fill = Metode)) +
      geom_boxplot() +
      facet_wrap(~ type) +
      theme_bw() +
      labs(y = paste0(measure, " starpība")) +
      scale_fill_manual(values=c("#1f78b4", "#33a02c"), guide = FALSE)
    ggsave(filename = paste0(dir_path, paste("data", data_id, measure, "diff.png", sep = "_")), 
           device = "png", width = 8, height = 4)
  }
  
  diff_table <- diff %>% 
    filter(str_detect(name, pattern = "diff")) %>% 
    group_by(name, type) %>% 
    summarize(norm_test = shapiro.test(value)$p.value,
              mean = mean(value),
              median = median(value),
              t_test_p_val = t.test(value)$p.value,
              adj_t_test_p_val = adjusted_t_test(value, n2_to_n1 = n2_to_n1)$p_value)
  save(diff_table, file = paste("data", data_id, "diff_table.rda", sep = "_"))
}