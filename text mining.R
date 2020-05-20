```{r}
install.packages("dplyr")
install.packages("tidytext")
install.packages("tm")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("igraph")
install.packages("ggraph")
install.packages("wordcloud")
install.packages("widyr")
install.packages("tidyquant")

```


```{r}
##  Attach File, Each column is a question, each row is a different response)
## data is a placeholder here, replace with dataset name
attach(data)

## The numbers in the [] are the Row,Column. Leave the (Row) empty if you are using all the responses in the sheet

text <- data[,2:7]

# This code collapses all of the rows so that all of the text is in on long line. 
text <- paste(text, collapse = " ")
View(text)


library(dplyr)

# This code separates each word out and puts 1 word in each line. 
text_df <- tibble(line = 1, text = text)
View(text_df)

library(tidytext)
library(tm)


tidy_books <- text_df %>%
  unnest_tokens(word, text)


# This code removes stop words for english. It would probably work to remove english as well if you changed spanish 
# to english. 
custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("en"),
                                          lexicon = "custom"))


tidy_books <- tidy_books %>%
  anti_join(custom_stop_words)  %>%
  count(word, sort = TRUE) 

library(ggplot2)

# This creates bigrams (2 words that occure in sequence). From this we can create a web of words that occur together)
bigrams <- tidy_books %>%
  anti_join(custom_stop_words) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams %>%
  count(bigram, sort = TRUE)

library(tidyr)

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# This code filters out the stop words (filler words). This refernces custom_stop_words from above!
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

library(igraph)

#This preps for the bigram word web
bigram_graph <- bigram_counts %>%
  filter(n > 250) %>%
  graph_from_data_frame()

View(bg) 
bg <- bigram_graph
bc <- bigram_counts
#This creates a word cloud
library(wordcloud)

###old code
dev.new(width = 1000, height = 1000, unit = "px")
tidy_books %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, min.freq=3,scale=c(4,.5),random.order = FALSE, rot.per=.5, vfont=c("sans serif","plain")))
###new code
set.seed(1234)
wordcloud(words = word12, freq = bc$n, min.freq = 50,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(5, "Dark2"))
###end new 
library(ggraph)
library(tidyquant)


ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)



a <- grid::arrow(type = "closed", length = unit(.1, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "turquoise", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  labs(
    title  = "Why did you choose Homie?"
    #xlab = "",
    #ylab = "",
  ) +
  theme_tq()
####################################################################################################



```