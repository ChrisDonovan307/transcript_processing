#' Smush Text
#' 2024-08-14


# smush_text --------------------------------------------------------------


#' Smush lines of text together so that when a single speaker has several lines
#' in a row, it will consolidate them into a single line.

smush_text <- function(df,
                       text_col = 'text',
                       min_length = 6) {
  
  # First consolidate all rows missing a name
  i <- 2
  while (i <= nrow(df)) {
    if (str_detect(df[[text_col]][i], ':', negate = TRUE)) {
      df[[text_col]][i - 1] <- paste(df[[text_col]][i - 1], df[[text_col]][i])
      df <- df[-i, ]
    } else {
      i <- i + 1
    }
  }
  
  # Now consolidate all consecutive rows with the same name
  i <- 2
  while (i <= nrow(df)) {
    if (str_split_i(df[[text_col]][i], ':', 1) == str_split_i(df[[text_col]][i - 1], ':', 1)) {
      df[[text_col]][i] <- str_split_i(df[[text_col]][i], ': ', 2)
      df[[text_col]][i - 1] <- paste(df[[text_col]][i - 1], df[[text_col]][i])
      df <- df[-i, ]
    } else {
      i <- i + 1
    }
  }
  
  # Then remove any lines with less than certain length IF between lines from 
  # the same speaker
  i <- 2
  while(i <= nrow(df)) {
    if (str_length(str_split_i(df[[text_col]][i], ': ', 2)) <= min_length &&
        str_split_i(df[[text_col]][i - 1], ':', 1) == str_split_i(df[[text_col]][i + 1], ':', 1)) {
      df <- df[-i, ]
    } else {
      i <- i + 1
    }
  }
  
  # Now we have to smush again
  i <- 2
  while (i <= nrow(df)) {
    if (str_split_i(df[[text_col]][i], ':', 1) == str_split_i(df[[text_col]][i - 1], ':', 1)) {
      df[[text_col]][i] <- str_split_i(df[[text_col]][i], ': ', 2)
      df[[text_col]][i - 1] <- paste(df[[text_col]][i - 1], df[[text_col]][i])
      df <- df[-i, ]
    } else {
      i <- i + 1
    }
  }
  
  return(df)
}

