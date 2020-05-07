# -------------------------------------------------- 
# Script Name : dependancies.R
# Purpose: Functions that converts word numbers into digits
#											
# --------------------------------------------------



word_to_number <- function(x){
  # Remove punctuation and 'and'
  x <- tolower(gsub("([[:punct:]]| and )", " ", x))
  # separate into distinct words
  x <- trimws(unlist(strsplit(x, "\\s+")))
  
  # verify that all words are found in the reference vectors.
  if (!(all(x %in% names(c(word_to_number_reference, magnitude_reference)))))
    stop("Text found that is not compatible with conversion. Check your spelling?")
  
  # translate words to the numeric reference
  num <- c(word_to_number_reference, magnitude_reference)[x]
  
  # Identify positions with a magnitude indicator
  magnitude_at <- 
    which(names(num) %in% 
            c("quadrillion", "trillion", "billion",
              "million", "thousand"))
  
  # Create an indexing vector for each magnitude class of the number
  magnitude_index <- 
    cut(seq_along(num), 
        breaks = unique(c(0, magnitude_at, length(num))))
  
  # Make a list with each magnitude
  num_component <- 
    lapply(unique(magnitude_index),
           FUN = function(i) num[magnitude_index == i])
  
  # Transate each component
  num_component <- 
    vapply(num_component,
           FUN = word_to_number_translate_hundred,
           FUN.VALUE = numeric(1))
  
  # Add the components together
  num <- sum(num_component)
  
  if (is.na(num))
    warning(sprintf("Unable to translate %s", x))
  
  num
}

word_to_number_translate_hundred <- function(n){
  # set a magnitude multiplier for thousands and greater
  if (tail(names(n), 1) %in% names(magnitude_reference)){
    magnitude <- tail(n, 1)
    n <- head(n, -1)
  } else {
    magnitude <- 1
  }
  
  # if hundred appears anywhere but the second position or of the
  # value preceding hundred is greater than 9, handle with care
  # (for instance, 1200)
  if ( ("hundred" %in% names(n) && which(names(n) == "hundred") != 2) ||
       ("hundred" %in% names(n) && n[1] > 1) )
  {
    which_hundred <- which(names(n) == "hundred")
    (sum(n[seq_along(n) < which_hundred]) * 100 + 
        sum(n[seq_along(n) > which_hundred])) * magnitude
  } else {
    op <- rep("+", length(n) - 1)
    op[names(n)[-1] == "hundred"] <- "*"
    op <- c(op, "")
    eval(parse(text = paste(paste(n, op), collapse = " "))) * magnitude
  }
}



word_to_number_reference <- 
  c("zero" = 0,
    "one" = 1,
    "two" = 2,
    "three" = 3,
    "four" = 4,
    "five" = 5,
    "six" = 6,
    "seven" = 7,
    "eight" = 8,
    "nine" = 9,
    "ten" = 10,
    "eleven" = 11,
    "twelve" = 12,
    "thirteen" = 13,
    "fourteen" = 14,
    "fifteen" = 15,
    "sixteen" = 16,
    "seventeen" = 17,
    "eighteen" = 18,
    "nineteen" = 19,
    "twenty" = 20,
    "thirty" = 30,
    "forty" = 40,
    "fifty" = 50,
    "sixty" = 60,
    "seventy" = 70,
    "eighty" = 80,
    "ninety" = 90,
    "hundred" = 100)

magnitude_reference <- 
  c("thousand" = 1000,
    "million" =  1e6,
    "billion" =  1e9,
    "trillion" = 1e12,
    "quadrillion" = 1e15)