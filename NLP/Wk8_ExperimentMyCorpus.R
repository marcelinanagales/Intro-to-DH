library(dplyr)
library(NLP)
library(openNLP)
library(RWeka)
library(rJava)
library(magrittr)

# easy function for extracting entities from an Annotated Plain Text Document (we will use this in the next code chunk)
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotation(doc) 
  # a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

# a function that annotates text and then creates a AnnotatedPlainTextDocument
annotate_entities <- function(doc, annotation_pipeline) {
  # because I was getting errors when running this function (in a future code chunk)
  annotations <- annotate(doc, annotation_pipeline)
  AnnotatedPlainTextDocument(doc, annotations)
}

# gets the rds
detective_corpus <- readRDS("~/Downloads/Intro-to-DH/Week1_Final/detective.rds")

# gives a new column newtext that has a 0 when the title name changes
detective_corpus %$%
{title == lag(title, n = 1)} %>%
  as.numeric() %>%
  {.} -> detective_corpus$newtext

# getting all unique titles
titles <- detective_corpus %>% pull(title)
distinct_title <- unique(titles)

# the starting indices for each individual books
# using the newtext column (n/a is first book) (0 are new book mentions)
book_indices <- which(detective_corpus$newtext == 0 | is.na(detective_corpus$newtext))

# append last book end index to book_indices
book_indices <- append(book_indices, (nrow(detective_corpus)+1))

# initialize empty list
litlist = list()

for (i in 1:length(distinct_title))
{
  litlist[i] <-  trimws(paste0(detective_corpus$text[book_indices[i] : book_indices [i+1]], collapse = " "))
}

# formatting litlist to be annotated and create AnnotatedPlainTextDocument
litlist <- litlist %>% lapply(as.String)
names(litlist) <- basename(distinct_title)

# creates a list of the different annotators: word, sentence, person, location 
itinerants_pipeline <- list(
  Maxent_Sent_Token_Annotator(),
  Maxent_Word_Token_Annotator(),
  Maxent_Entity_Annotator(kind = "person"),
  Maxent_Entity_Annotator(kind = "location")
)

# this is suppose to take the longest because its going through 4 annotators for eachtext (of which there are 3)
# annotates all texts in litlist and applies all annotators from itinerants_pipeline using annotate_entities()
texts_annotated <- litlist %>%
  lapply(annotate_entities, itinerants_pipeline)

# selects all places mentioned in text_annotated 
places <- texts_annotated %>%
  lapply(entities, kind = "location")

# selects all people mentioned in text_annotated 
people <- texts_annotated %>%
  lapply(entities, kind = "person")

# lists the number of places per text file 
print("Number of Places per Text:")
print(places %>%
  sapply(length))

# lists the number of unique places per text file
print("Number of Unique Places per Text:")
print(places %>%
  lapply(unique) %>%
  sapply(length))

# lists the number of people per text mentioned
print("Number of People Mentioned per Text:")
print(people %>%
  sapply(length))

# lists the number of unique people per test mentioned
print("Number of Unique People Mentioned per Text:")
print(people %>%
  lapply(unique) %>%
  sapply(length))