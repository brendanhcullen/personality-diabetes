# what to expect if random?

# ---------------------
# set probabilities
# ---------------------

load(here("output/data_cleaned.Rdata"))

probs = table(data_scored$diabetes)
p_t1d = probs["t1d"]/sum(probs)
p_t2d = probs["t2d"]/sum(probs)
p_healthy = probs["healthy"]/sum(probs)

n = sum(probs)

num_resamples = 10000

#accuracy, sensitivity, specificity
acc = numeric(length = num_resamples)
sens_healthy = numeric(length = num_resamples)
spec_healthy = numeric(length = num_resamples)
sens_t1d = numeric(length = num_resamples)
spec_t1d = numeric(length = num_resamples)
sens_t2d = numeric(length = num_resamples)
spec_t2d = numeric(length = num_resamples)

real_data = data_scored$diabetes

for(i in 1:num_resamples){
random_pred = sample(x = unique(data_scored$diabetes), size = n, prob = c(p_healthy, p_t2d, p_t1d), replace = TRUE)
CM = confusionMatrix(real_data, random_pred)

acc[i] = CM$overall["Accuracy"]
sens_healthy[i] = CM$byClass["Class: healthy", "Sensitivity"]
spec_healthy[i] = CM$byClass["Class: healthy", "Specificity"]
sens_t1d[i] = CM$byClass["Class: t1d", "Sensitivity"]
spec_t1d[i] = CM$byClass["Class: t1d", "Specificity"]
sens_t2d[i] = CM$byClass["Class: t2d", "Sensitivity"]
spec_t2d[i] = CM$byClass["Class: t2d", "Specificity"]
}