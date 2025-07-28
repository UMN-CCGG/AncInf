library(tidyverse)

dir1 = "rfmix/"
setwd(dir1)

# get names of Q output files
Qfiles <- list.files(pattern = ".Q")
# get number of Q output files -- most often will be 22
numfiles <- length(Qfiles)

dir2 = "../input/"
setwd(dir2)
famfiles = list.files(pattern = ".fam")
fam = read.table(famfiles) %>% select(V2)

# apply a purr::map function instead of a for loop
dir3 = "../rfmix/"
setwd(dir3)

global_ancestry <- map_dfr(1:numfiles, function(x){
  read_delim(file = Qfiles[x], skip = 1, delim = "\t")
}) %>% # read in all Q files, and create one really long tbl
  rename("sample" = "#sample") %>% #get rid of the # which can cause downstream problems
  group_by(sample) %>% #group all samples with the same name together
  summarize_all(mean) %>% # get mean of all other columns (AMR, AFR, EAS, EUR, SAS)
  ungroup() %>% #ungroup
  inner_join(fam, by = c("sample" = "V2")) %>% 
  mutate(tmp.amr = case_when(AMR >= 0.015 ~ AMR,
                             AMR < 0.015 ~ 0.015),
         inferred_race = case_when(EAS > 0.2 | SAS > 0.2 ~ "ASI",
                                   EUR > 0.75 & tmp.amr < 0.1 ~ "EUR",
                                   tmp.amr > 0.9 ~ "AMR",
                                   AFR/tmp.amr > 10 ~ "AA",
                                   TRUE ~ "LAT")) %>% 
  select(-7)

write.csv(global_ancestry, file = "global_ancestry.csv", row.names = FALSE)
