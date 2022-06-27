################################################################
# Helper functions for cleaning text
################################################################

if(!require('stringr')) {
  install.packages("stringr")
  library(stringr)
}

if (!require("textutils")) {
  install.packages("textutils")
  library(textutils)
}

################################################################
# Cleans text of undesired characters. 
# - For word frequency analysis, all non-alphanumeric are removed.
# - Otherwise most common punctuation is allowed.
################################################################
clean_text <- function(text, for_freq=FALSE, html_decode=TRUE) {
  if (isTRUE(html_decode)) {
    text <- HTMLdecode(text)
  }
  text <- str_replace_all(text, "[\\s]+", " ")
  text <- str_replace_all(text, "http\\S+", "")
  text <- str_replace_all(text, "’", "'")
  if (isTRUE(for_freq)) {
    text <- tolower(text)
    text <- str_replace_all(text, "_", "-")
    text <- str_replace_all(text, "[^a-z1-9 ']", "")
  } else {
    text <- str_replace_all(text, "[^a-zA-Z1-9 `~!@#$%^&*()-_=+\\[\\];:'\",./?’]", "")
  }
  text <- str_replace_all(text, " +", " ")
  text <- trimws(text)
}


