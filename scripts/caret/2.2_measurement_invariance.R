library(here)
library(tidyverse)
library(lavaan)
library(semTools)

load(here("output/data_cleaned.Rdata"))
keys = read.csv(here("data/superKey.csv"), header = TRUE, stringsAsFactors = F)
keys = keys %>%
  select("X", contains("SPI_135"))

#remove beginning of scale names
scale_names = tolower(gsub("SPI_135_27_5_", "", names(keys)[-1]))
#create factor variable (for ordering tables and such)
scale_names_f = factor(scale_names, levels = scale_names)

#identify items for each scale and store in a data frame
keys.df = keys %>%
  rename(item = X) %>%
  gather(key = "scale", value = "value", -item) %>%
  filter(value != 0) %>%
  group_by(scale) %>%
  nest() %>%
  ungroup() %>%
  mutate(scale = tolower(gsub("SPI_135_27_5_", "", scale)))%>%
  mutate(items = map(data, 1)) %>%
  mutate(scale_key = map(data, 2)) %>%
  select(-data)

group = "diabetes"

# create a dataframe where each row refers to a single scale. 
# columns include the name of the scale and a data frame [wherein items are reverse scored when appropriate]
mi_data = expand.grid(RID = data$RID, 
                      scale = scale_names, 
                      stringsAsFactors = F) %>%
  full_join(data) %>%
  group_by(scale) %>%
  nest() %>%
  full_join(keys.df, by = "scale") %>%
  #select just scale items and group variable (diabetes status)
  mutate(data = map2(.x = data, 
                     .y = items, .f = function(x,y) dplyr::select(x, y, "diabetes"))) %>%
  mutate(data = map2(.x = data, .y = scale_key, 
                     .f = function(x,y) mutate_at(x, .vars = which(y == -1), 
                                                  .funs = function(i) i*-1+7))) 

mi_models = mi_data %>%
  mutate(model = map(items, 
                     .f = function(x) paste0("trait =~ ", paste(x, collapse = " + ")))) %>%
  # two groups analysis
  mutate(model_output = map2(model, data, lavaan::cfa, missing = "FIML")) %>%
  # run measurement invariance 
  mutate(config_mi_output = map2(model, data, measEq.syntax, group = "diabetes", 
                                 return.fit = T, missing = "FIML")) %>%
  mutate(metric_mi_output = map2(model, data, measEq.syntax, group = "diabetes", 
                                 return.fit = T, missing = "FIML",
                                 group.equal = "loadings")) %>%
  mutate(scalar_mi_output = map2(model, data, measEq.syntax, group = "diabetes", 
                                 return.fit = T, missing = "FIML",
                                 group.equal = c("loadings", "intercepts"))) %>%
  mutate(strict_mi_output = map2(model, data, measEq.syntax, group = "diabetes", 
                                 return.fit = T, missing = "FIML",
                                 group.equal = c("loadings", "intercepts", "residuals"))) 

# compare fits
mi_compare = mi_models %>% 
  mutate(compare_config = map2(model_output, config_mi_output, compareFit)) %>%
  mutate(compare_metric = map2(config_mi_output, metric_mi_output, compareFit)) %>%
  mutate(compare_scalar = map2(metric_mi_output, scalar_mi_output, compareFit)) %>%
  mutate(compare_strict = map2(scalar_mi_output, strict_mi_output, compareFit)) %>%
  select(compare_config, compare_metric, compare_scalar, compare_strict)

#funct# Function to calculate Mcdonald ’s NCI
Mc <- function ( object , digits =3) {
  fit <- inspect ( object , "fit" ) #lavaan ’s default output
  chisq = unlist ( fit [ "chisq" ])# unlist ( fit [" chisq "]) # model Chi - square
  df <- unlist ( fit [ "df" ]) # model df
  n <- object@SampleStats@ntotal
  ncp <- max( chisq - df ,0) #non - centrality parameter
  d <- ncp /(n -1) # scaled non - centrality parameter
  Mc = exp (( d)* -.5) # McDonald ’s non - centrality index
  Mc
}

fitmeasures = mi_models %>%
  mutate(model1_gammahat = map(model_output, moreFitIndices, fit.measures = "gammaHat"),
         model_config_gammahat = map(config_mi_output, moreFitIndices, fit.measures = "gammaHat"),
         model_metric_gammahat = map(metric_mi_output, moreFitIndices, fit.measures = "gammaHat"),
         model_scalar_gammahat = map(scalar_mi_output, moreFitIndices, fit.measures = "gammaHat"),
         model_strict_gammahat = map(strict_mi_output, moreFitIndices, fit.measures = "gammaHat")) %>%
  mutate(model1_nci = map(model_output, Mc),
         model_config_nci = map(config_mi_output, Mc),
         model_metric_nci = map(metric_mi_output, Mc),
         model_scalar_nci = map(scalar_mi_output, Mc),
         model_strict_nci = map(strict_mi_output, Mc)) %>%
  mutate(config_fit = map(compare_config, "fit"),
         config_cfi = map(config_fit, "cfi"),
         config_nci = map2(model1_nci, model_config_nci, function(x,y) abs(x-y)),
         congig_gammahat = map2(model1_gammahat, model_config_gammahat, function(x,y) abs(x-y))) %>%
  mutate(metric_fit = map(compare_metric, "fit"),
         metric_cfi = map(metric_fit, "cfi"),
         metric_nci = map2(model_config_nci, model_metric_nci, function(x,y) abs(x-y)),
         congig_gammahat = map2(model_config_gammahat, model_metric_gammahat, function(x,y) abs(x-y))) %>%
  mutate(scalar_fit = map(compare_scalar, "fit"),
         scalar_cfi = map(scalar_fit, "cfi"),
         scalar_nci = map2(model_metric_nci, model_scalar_nci, function(x,y) abs(x-y)),
         congig_gammahat = map2(model_metric_gammahat, model_scalar_gammahat, function(x,y) abs(x-y))) %>%
  mutate(strict_fit = map(compare_strict, "fit"),
         strict_cfi = map(strict_fit, "cfi"),
         strict_nci = map2(model_scalar_nci, model_strict_nci, function(x,y) abs(x-y)),
         congig_gammahat = map2(model_scalar_gammahat, model_strict_gammahat, function(x,y) abs(x-y))) 
