library(tidyverse)
dir = "rfmix/"
setwd(dir)
# get names of Q output files
Qfiles <- list.files(pattern = ".Q")
# get number of Q output files -- most often will be 22
numfiles <- length(Qfiles)
# apply a purr::map function instead of a for loop
global_ancestry <- map_dfr(1:numfiles, function(x){
  read_delim(file = Qfiles[x], skip = 1, delim = "\t")
}) %>% # read in all Q files, and create one really long tbl
  rename("sample" = "#sample") %>% #get rid of the # which can cause downstream problems
  group_by(sample) %>% #group all samples with the same name together
  summarize_all(mean) %>% # get mean of all other columns (AMR, AFR, EAS, EUR, SAS)
  ungroup() %>% #ungroup
  filter(str_detect(string = sample, pattern = "^HG|^NA", negate = T)) %>% # RFMix spits out 1000G reference individuals too.  These can be identified by beginning with HG or NA.  They need to be removed.
  mutate(tmp.amr = case_when(AMR >= 0.015 ~ AMR,
                             AMR < 0.015 ~ 0.015),
         inferred_race = case_when(EAS > 0.2 | SAS > 0.2 ~ "ASI",
                                   EUR > 0.75 & tmp.amr < 0.1 ~ "EUR",
                                   tmp.amr > 0.9 ~ "AMR",
                                   AFR/tmp.amr > 10 ~ "AA",
                                   TRUE ~ "LAT")) %>% 
  select(-7)
write.csv(global_ancestry, file = "global_ancestry.csv", row.names = FALSE)
