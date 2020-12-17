# what to expect if random?

# ---------------------
# set probabilities
# ---------------------

data_scored = readRDS(here("output/machine_learning/training/train_data_pp.RDS"))

probs = table(data_scored$diabetes)
p_t1d = probs["type1"]/sum(probs)
p_t2d = probs["type2"]/sum(probs)
p_healthy = probs["none"]/sum(probs)

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
CM = confusionMatrix(as.factor(real_data), as.factor(random_pred))

acc[i] = CM$overall["Accuracy"]
sens_healthy[i] = CM$byClass["Class: none", "Sensitivity"]
spec_healthy[i] = CM$byClass["Class: none", "Specificity"]
sens_t1d[i] = CM$byClass["Class: type1", "Sensitivity"]
spec_t1d[i] = CM$byClass["Class: type1", "Specificity"]
sens_t2d[i] = CM$byClass["Class: type2", "Sensitivity"]
spec_t2d[i] = CM$byClass["Class: type2", "Specificity"]
}


hist(acc)
mean(acc)
median(acc)
