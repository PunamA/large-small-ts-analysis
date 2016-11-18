##Data analysis
download.file("http://www.datacarpentry.org/semester-biology/data/houseelf-earlength-dna-data.csv",
              "houseelf-earlength-dna-data.csv")
#import the data
houseelf <- read.csv("houseelf-earlength-dna-data.csv")

library(dplyr)
library(stringr)
#function to categorise ear length of house elves
length_cat = function(earlength){
  ifelse(earlength > 10,"Large","Small")
}

#Function to calculate the GC content
get_GC_content=function(sequence){
  sequence=str_to_lower(sequence)
  Gs <- str_count(sequence, "g")
  Cs <- str_count(sequence, "c")
  gc_content <- (Gs + Cs) / str_length(sequence) * 100
  return(gc_content)
}

#To create a new table that has categorical earlength and the GC-content
granger_analysis <- houseelf %>%
  rowwise() %>%
  mutate(earlength_cat = length_cat(earlength)) %>% 
  mutate(GC_content = get_GC_content(dnaseq)) %>% 
  select(id,earlength_cat,GC_content) %>% 
  write.csv("granger_analysis.csv") 

#To get a summary of the mean GC content by earlength category
granger_summary <- read.csv("granger_analysis.csv")  %>% 
  group_by(earlength_cat) %>% 
  summarise(mean_GC_content = mean(GC_content))
granger_summary

#1)-- Set Up Git --

#2)