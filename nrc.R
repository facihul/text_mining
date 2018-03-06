library(tidytext)
library(dplyr)

#=======
##### 3 class data #####
class3 <- c("Amazon.txt","Comments_BBC.txt","Comments_Digg.txt","Comments_NYT.txt",
            "Comments_TED.txt","Comments_YTB.txt","Myspace.txt","Reviews_II.txt","RW.txt",
            "Tweets_DBT.txt","Tweets_RND_I.txt","Tweets_RND_II.txt","Tweets_RND_III.txt",
            "Tweets_RND_IV.txt","Tweets_SAN.txt","Tweets_Semeval.txt")

# stop words
data(stop_words)

# read data
for (i in 1:length(class3)) {
  filename <- class3[i]
  
  path <-
    paste0("/Volumes/Study/TUT/Phd/golden_standard/Sentences/",
           filename)
  data <-
    readLines(path)
  data_df <- data_frame(line = 1:length(data), text = data)
  
  tidy_data <- data_df %>%
    unnest_tokens(word, text)
  tidy_data <- tidy_data %>%
    anti_join(stop_words)
  
  nrc <- get_sentiments("nrc")
  
  # sentiment
  sentiment <- tidy_data %>%
    inner_join(get_sentiments("nrc")) %>%
    group_by(line) %>%
    ungroup()
  
  Ngsenti <- c("anger","negative","sadness","disgust","fear")  
  Nsenti <- c("trust","surprise")
  lsenti <- c("anticipation","joy","positive")
  
  #############  
 sentiment1 <- sentiment %>%
   group_by(line) %>%
   
   
     mutate (sentiment, senti = ifelse(sentiment %in% lsenti, 1 ,
                                       ifelse(sentiment %in% Ngsenti, -1 ,0 )))%>%
           
     count( senti)  %>%
     summarise(senti = sum(senti))
  
    
                   
  
  pred_NRC_Label <- rep(0, length(data))
  
  exist <- sentiment1[['line']]
  
  for (i in 1:length(exist)) {
    linenum <- exist[i]
    pred_NRC_Label[linenum] <- sentiment1[[i, 2]]
    
    if(pred_NRC_Label[linenum] > 0){
      pred_NRC_Label[linenum] <- 1
    }
    else if(pred_NRC_Label[linenum] < 0){
      pred_NRC_Label[linenum] <- -1
    }
    else pred_NRC_Label[linenum] <- 0
  }
  
  # pathTowrite <-
  #   paste0("/Volumes/Study/TUT/Phd/golden_standard/code/NRc/label_tidy/",
  #          filename)
  # 
  # write(pred_NRC_Label, file = pathTowrite, sep = "\n")
}
