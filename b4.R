#Load required packages
library(tidytext)
library(janeaustenr)
library(tidyverse)

#Choose book
full.book <- janeaustenr::prideprejudice

#Prepare text
book <- full.book
book <- paste(book, collapse = " ") #remove ""
book <- str_replace_all(book, pattern = "\\\\", replacement = "") # Remove slashes
book <- str_replace_all(book, pattern = "\\n", replacement = "") #Remove new line
book <- str_replace_all(book, pattern = "[:punct:]", replacement = "") #Remove punctuation
book <- tolower(book) #words that are in both capital and lowercase will not be double counted

book_df <- tibble(Text = book) #Make tibble for 

book_words <- book_df %>% 
    unnest_tokens(output = word, input = Text) 

book_words  <- book_words  %>%
    anti_join(stop_words) # Remove stop words 

# Word Counts:
  
book_wordcounts <- book_words  %>% count(word, sort = TRUE)
use <- slice(book_wordcounts,(1:10))
use

ggplot(data = use, aes(y=n, x=word)) +
  geom_col(colour="black",fill="darkslategray4") +
  xlab("word") +
  ylab("times used") +
  ggtitle("Most Used Words - Pride and Prejudice")

set.seed(13)
word1 <- sample(stringr::words, 20)
body <- str_sub(word1, -2, -1)
end <- str_c(str_sub(word1, 1,2), "hat")
str_c(body,end) 
body
word1
