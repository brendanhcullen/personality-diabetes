
# extraversion items
extra_items <- tribble(
  ~item, ~reverse_scored,
  "q_1904", "no" ,
  "q_565", "yes" ,
  "q_1243", "no" ,
  "q_312", "yes" ,
  "q_1027", "yes", 
  "q_254", "no" ,
  "q_1244", "no" ,
  "q_1923", "yes", 
  "q_1416", "no" ,
  "q_803", "no" ,
  "q_4243", "no" ,
  "q_1296", "no" ,
  "q_901", "yes" ,
  "q_684", "yes"
)

neuro_items <- tribble(
  ~item, ~reverse_scored,
"q_578", "no" ,
"q_4252", "no" ,
"q_979", "no" ,
"q_811", "no" ,
"q_1989", "no" ,
"q_1683", "yes",
"q_4249", "no" ,
"q_793", "no" ,
"q_1585", "yes",
"q_1505", "no" ,
"q_797", "yes" ,
"q_176", "yes" ,
"q_808", "no" ,
"q_1840", "yes"
)

# get reverse scored items
extra_reverse <- extra_items %>% 
  filter(reverse_scored == "yes") %>% 
  pull(item)

# get reverse scored items
neuro_reverse <- neuro_items %>% 
  filter(reverse_scored == "yes") %>% 
  pull(item)

# pull out scored training data
# ***NEED TO RUN 0_tidymodels_template.R first***
juiced <- juice(rec_prep)

# calculate mean extraversion scores manually
spot_check_extra <- juiced %>% 
  select(RID, extra, extra_items$item) %>% 
  mutate_at(vars(matches(extra_reverse)), .funs = function(x) 7-x) %>% 
  mutate(extra_check = rowMeans(select(., -extra, -RID), na.rm = TRUE)) %>% 
  select(RID, extra, extra_check)

# find any rows where scores from scoreVeryFast don't match scores created manually
spot_check_extra %>% 
  filter(!round(extra, 1) == round(extra_check,1))


# calculate mean neuroticism scores manually
spot_check_neuro <- juiced %>% 
  select(RID, neuro, neuro_items$item) %>% 
  mutate_at(vars(matches(neuro_reverse)), .funs = function(x) 7-x) %>% 
  mutate(neuro_check = rowMeans(select(., -neuro, -RID), na.rm = TRUE)) %>% 
  select(RID, neuro, neuro_check)

# find any rows where scores from scoreVeryFast don't match scores created manually
spot_check_neuro %>% 
  filter(!round(neuro, 1) == round(neuro_check,1))
