##goodreads

library(httr)
library(tidyverse)
library(stringr)
library(xml2)
library(viridis)
library(knitr)
devtools::install_github("famguy/rgoodreads")

opts_chunk$set(cache = TRUE, warning = FALSE, message = FALSE)

API_KEY <- 'Owwa1MwZG91gzRxKdDxDRg'
SECRET <- 'C9sA9naNzdu0tZXNX6CGDkun7WwGUQjvqUIx8sKck'
USER_ID <- '77968644'

Sys.setenv(GOODREADS_KEY = "Owwa1MwZG91gzRxKdDxDRg")

#my_books <- read_csv()

devtools::install_github("Tazinho/snakecase")
library(snakecase)
orig_colnames <- colnames(my_books)  # get original colnames
new_colnames <- to_any_case(orig_colnames, case = "snake")
colnames(my_books) <- new_colnames  # assign new colnames
colnames(my_books)  # check to make sure it worked

keeper_cols <- c("book_id", "title", "author", "my_rating", "number_of_pages", 
                 "original_publication_year", "date_read", "date_added", "bookshelves")

books_myvars <- my_books %>%
  select(one_of(keeper_cols))

read_books <- books_myvars %>%
  filter(!is.na(date_read))

library(lubridate)

read_books <- read_books %>%
  mutate(date_read = ymd(date_read)) %>%
  mutate(month_read = month(date_read)) %>%
  mutate(year_read = year(date_read)) %>%
  arrange(desc(date_read))


##books read per year
read_books %>%
  ggplot(aes(year_read)) +
  geom_bar(stat = "count", fill="#69b3a2", color="#e9ecef") +
  labs(title = "Books read per year", 
       caption = "source: Goodreads API")+
  theme_minimal()

#bad
read_books %>%
  filter(number_of_pages >= 1) %>%
  ggplot(aes(month_read)) +
  geom_bar(stat = "number_of_pages",fill="#69b3a2", color="#e9ecef") +
  labs(title = "Number of Pages read per year", 
       caption = "source: Goodreads API")

year_pages <- read_books %>%
  group_by(year_read) %>%
  summarize(number_of_pages = sum(number_of_pages, na.rm = TRUE))
#use
myplot<- ggplot(year_pages, aes(x=year_read, y=number_of_pages)) +
  geom_col(fill="#69b3a2")+
  labs(title = "Number of Pages read per year", 
       caption = "source: Goodreads API")+
  theme_minimal()

print(myplot)

myplot + theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

##pages/year
myplot + theme_minimal()

ggplot(read_books, aes(y=read_books$number_of_pages, x=read_books$year_read)) +
  geom_bar(color=read_books$year_read)

read_books %>% 
  ggplot(mapping = aes(x = language, y = speakers)) +
  geom_col() 

##boxplot
read_books$my_rating <- as.factor(read_books$my_rating)
box <- ggplot(read_books, aes(x=my_rating, y=number_of_pages)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2, notch=FALSE)
box + stat_summary(fun.y=mean, geom="point", shape=23, size=4)
box + geom_jitter(shape=16, position=position_jitter(0.2))
box

box+labs(title="Number of Pages Distributed by Rating")+theme_minimal()

box+labs(title="Number of Pages Distributed by Rating"
         caption = "source: Goodreads API")
##density
dens<-read_books %>%
  filter( original_publication_year>1940 ) %>%
  ggplot( aes(x=original_publication_year)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Books read/Original Publication Year")+
  theme_minimal()
dens
?geom_density

dens2<-read_books %>%
  filter( number_of_pages>1 ) %>%
  ggplot( aes(x=number_of_pages)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Books read/Number of Pages")+
  theme_minimal()
dens2
?geom_density

dens <- ggplot(read_books, aes(x=original_publication_year)) + 
  geom_density()
dens
