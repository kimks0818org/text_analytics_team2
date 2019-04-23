
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  library(textreadr)
  team2 <- read_document(file="C:/Users/Ki/Desktop/Text Analytics/Revised_T2_3.txt")
  
  ######################################################
  ###STEP2: Putting the vector in a data frame##########
  ######################################################
  
  #install.packages("dplyr")
  library(dplyr)
  mydf <- data_frame(line=1:234, text=team2)
  print(mydf)
  
  ######################################################
  ######## Step3: tokenizing the mydf dataframe#########
  ######################################################
  library(tidyverse)
  library(tidytext)
  token_list <- mydf %>%
    unnest_tokens(word, text)
  #no punctutation, no upper case letters
  print(token_list)
  
  #######################################################
  ##########STEP4: token frequencies####################
  #######################################################
  
  frequencies_tokens <- mydf %>%
    unnest_tokens(word, text) %>%
    count(word, sort=TRUE)
  print(frequencies_tokens)
  
  #######################################################
  #########STEP5:### stop words #########################
  #######################################################
  
  #stop words are words that are commonly used in English 
  # e.g. is, I, are, you, me, the, of, etc.
  #we will use the anti_join(stop_words) to remove the stop words
  library(dplyr)
  library(stringr)
  library(tidytext)
  
  data(stop_words)
  frequencies_tokens_nostop <- mydf %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% #here's where we remove tokens
    count(word, sort=TRUE)
  
  
  print(frequencies_tokens_nostop)
  
  #Frequency
  output$wordfrequency <- renderPlot({
    
    library(ggplot2)
    freq_hist <- mydf %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(word, sort=TRUE) %>%
      mutate(word=reorder(word, n)) %>%
      top_n(25) %>%
      ggplot(aes(word, n))+
      geom_col()+
      xlab(NULL)+
      coord_flip()
    print(freq_hist)
    
    
  }) #closing Frequency renderPlot
  
  
  #total
  
  output$total <- renderPlot({
    
    library(reshape2)
    library(wordcloud)
    
    
    
    # Wordcloud for all the words in the document
    wordcloud(words = frequencies_tokens_nostop$word, freq = frequencies_tokens_nostop$n, min.freq = 1,
              max.words = 200,
              random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
  }) #closing total renderPlot
  
  #Q14
  output$Q14 <- renderPlot({
    
    doc14 <- read_document(file="C:/Users/Ki/Desktop/Text Analytics/14.txt")
    
    ######################################################
    ###STEP2: Putting the vector in a data frame##########
    ######################################################
    
    #install.packages("dplyr")
    
    df14 <- data_frame(line=1:27, text=doc14)
    df14
    
    ######################################################
    ######## Step3: tokenizing the mydf dataframe#########
    ######################################################
    token_list14 <- df14 %>%
      unnest_tokens(word, text)
    #no punctutation, no upper case letters
    print(token_list14)
    
    #######################################################
    ##########STEP4: token frequencies####################
    #######################################################
    
    frequencies_tokens14 <- df14 %>%
      unnest_tokens(word, text) %>%
      count(word, sort=TRUE)
    
    
    #######################################################
    #########STEP5:### stop words #########################
    #######################################################
    
    #stop words are words that are commonly used in English 
    # e.g. is, I, are, you, me, the, of, etc.
    #we will use the anti_join(stop_words) to remove the stop words
    
    data(stop_words)
    stop_words <- stop_words %>%
      filter(word != "new")
    frequencies_tokens_nostop14 <- df14 %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% #here's where we remove tokens
      count(word, sort=TRUE)
    
    
    
    # Wordcloud for all the words in the document
    wordcloud(words = frequencies_tokens_nostop14$word, freq = frequencies_tokens_nostop14$n, min.freq = 1,
              max.words = 200,
              random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
    
  }) #closing Q14 renderPlot
  
  #Q15
  output$Q15 <- renderPlot({
    
    doc15 <- read_document(file="C:/Users/Ki/Desktop/Text Analytics/15.txt")
    
    ######################################################
    ###STEP2: Putting the vector in a data frame##########
    ######################################################
    
    #install.packages("dplyr")
    
    df15 <- data_frame(line=1:27, text=doc15)
    df15
    
    ######################################################
    ######## Step3: tokenizing the mydf dataframe#########
    ######################################################
    token_list15 <- df15 %>%
      unnest_tokens(word, text)
    #no punctutation, no upper case letters
    print(token_list15)
    
    #######################################################
    ##########STEP4: token frequencies####################
    #######################################################
    
    frequencies_tokens15 <- df15 %>%
      unnest_tokens(word, text) %>%
      count(word, sort=TRUE)
    
    
    #######################################################
    #########STEP5:### stop words #########################
    #######################################################
    
    #stop words are words that are commonly used in English 
    # e.g. is, I, are, you, me, the, of, etc.
    #we will use the anti_join(stop_words) to remove the stop words
    
    data(stop_words)
    stop_words <- stop_words %>%
      filter(word != "new")
    frequencies_tokens_nostop15 <- df15 %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% #here's where we remove tokens
      count(word, sort=TRUE)
    
    
    
    # Wordcloud for all the words in the document
    wordcloud(words = frequencies_tokens_nostop15$word, freq = frequencies_tokens_nostop15$n, min.freq = 1,
              max.words = 200,
              random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
    
  }) #closing Q15 renderPlot
  
  #Q04
  output$Q04 <- renderPlot({
  
    ##### Building a wordcount for 04 ######
    
    doc04 <- read_document(file="C:/Users/Ki/Desktop/Text Analytics/04.txt")
    
    ######################################################
    ###STEP2: Putting the vector in a data frame##########
    ######################################################
    
    #install.packages("dplyr")
    
    df04 <- data_frame(line=1:20, text=doc04)
    df04
    
    ######################################################
    ######## Step3: tokenizing the mydf dataframe#########
    ######################################################
    token_list04 <- df04 %>%
      unnest_tokens(word, text)
    #no punctutation, no upper case letters
    print(token_list04)
    
    #######################################################
    ##########STEP4: token frequencies####################
    #######################################################
    
    frequencies_tokens04 <- df04 %>%
      unnest_tokens(word, text) %>%
      count(word, sort=TRUE)
    
    
    #######################################################
    #########STEP5:### stop words #########################
    #######################################################
    
    #stop words are words that are commonly used in English 
    # e.g. is, I, are, you, me, the, of, etc.
    #we will use the anti_join(stop_words) to remove the stop words
    
    data(stop_words)
    stop_words <- stop_words %>%
      filter(word != "new")
    frequencies_tokens_nostop04 <- df04 %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% #here's where we remove tokens
      count(word, sort=TRUE)
    
    
    
    # Wordcloud for all the words in the document
    wordcloud(words = frequencies_tokens_nostop04$word, freq = frequencies_tokens_nostop04$n, min.freq = 1,
              max.words = 200,
              random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
    
      
  }) #closing Q04 renderPlot
  
  #Q05
  output$Q05 <- renderPlot({
    doc05 <- read_document(file="C:/Users/Ki/Desktop/Text Analytics/05.txt")
    
    ######################################################
    ###STEP2: Putting the vector in a data frame##########
    ######################################################
    
    #install.packages("dplyr")
    
    df05 <- data_frame(line=1:20, text=doc05)
    df05
    
    ######################################################
    ######## Step3: tokenizing the mydf dataframe#########
    ######################################################
    token_list05 <- df05 %>%
      unnest_tokens(word, text)
    #no punctutation, no upper case letters
    print(token_list05)
    
    #######################################################
    ##########STEP4: token frequencies####################
    #######################################################
    
    frequencies_tokens05 <- df05 %>%
      unnest_tokens(word, text) %>%
      count(word, sort=TRUE)
    
    
    #######################################################
    #########STEP5:### stop words #########################
    #######################################################
    
    #stop words are words that are commonly used in English 
    # e.g. is, I, are, you, me, the, of, etc.
    #we will use the anti_join(stop_words) to remove the stop words
    
    data(stop_words)
    stop_words <- stop_words %>%
      filter(word != "new")
    frequencies_tokens_nostop05 <- df05 %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% #here's where we remove tokens
      count(word, sort=TRUE)
    
    
    
    # Wordcloud for all the words in the document
    wordcloud(words = frequencies_tokens_nostop05$word, freq = frequencies_tokens_nostop05$n, min.freq = 1,
              max.words = 200,
              random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
  }) #closing Q05 renderPlot
  
  
  #correlation      
  output$correlation <- renderPlot({  
  #pairwise
    
    ######################################################
    ########Step1: building a small text object###########
    ######################################################
    
    library(textreadr)
    team2_2 <- read.csv(file="C:/Users/Ki/Desktop/Text Analytics/clean.csv")
    
    ######################################################
    ###STEP2: Putting the vector in a data frame##########
    ######################################################
    
    #install.packages("dplyr")
    library(dplyr)
    mydf2 <- data_frame(line=1:234, text=team2_2)
    print(mydf2)
    
    ######################################################
    ######## Step3: tokenizing the mydf dataframe#########
    ######################################################
    #install.packages("tidytext")
    #install.packages("tidyverse")
    #install.packages("SnowballC")
    library(tidyverse)
    library(tidytext)
    token_list2 <- mydf2 %>%
      unnest_tokens(word, text)
    #no punctutation, no upper case letters
    print(token_list2)
    
    #######################################################
    ##########STEP4: token frequencies####################
    #######################################################
    
    frequencies_tokens2 <- mydf2 %>%
      unnest_tokens(word, text) %>%
      count(word, sort=TRUE)
    print(frequencies_tokens2)
    
    #######################################################
    #########STEP5:### stop words #########################
    #######################################################
    
    #stop words are words that are commonly used in English 
    # e.g. is, I, are, you, me, the, of, etc.
    #we will use the anti_join(stop_words) to remove the stop words
    library(dplyr)
    library(stringr)
    library(tidytext)
    
    data(stop_words)
    frequencies_tokens_nostop2 <- mydf2 %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% #here's where we remove tokens
      count(word, sort=TRUE)
    
    
    print(frequencies_tokens_nostop2)
    
    
    #install.packages("widyr")
    library(widyr)
    # count words co-occuring within sections
    word_pairs <- frequencies_tokens_nostop2 %>%
      pairwise_count(word, n, sort = TRUE)
    
    word_pairs
    
    word_pairs %>%
      filter(item1 == "degree")
    
    word_cors <- frequencies_tokens_nostop %>%
      group_by(word) %>%
      #filter(n() >= 20) %>%
      pairwise_cor(word, n, sort = TRUE)
    
    word_cors %>%
      filter(item1 %in% c("mba", "degree", "international", "business")) %>%
      group_by(item1) %>%
      top_n(6) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      ggplot(aes(item2, correlation)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ item1, scales = "free") +
      coord_flip()
    
    #install.packages("igraph")
    #install.packages("ggraph")
    library(igraph)
    library(ggraph)
    set.seed(1224)
    word_cors %>%
      filter(correlation > .15) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void()    
    
  }) #closing the correlation renderPlot
  
})
