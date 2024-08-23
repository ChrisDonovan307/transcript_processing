#' clean_zoom_transcript
#' 2024-08-19

#' This was the original one without timestamps (raw_transcript). Corresponds
#' to walkthrough file



# Packages ----------------------------------------------------------------


pacman::p_load(
  dplyr,
  officer,
  purrr,
  stringr
)



# Function ----------------------------------------------------------------


clean_zoom_transcript <- function(raw_docx_file,
                                  clean_docx_file,
                                  min_str_length = 6,
                                  font_family = 'Aptos',
                                  font_size = 12,
                                  line_spacing = 1) {
  
  
  # Front Matter -----
  
  # Check inputs
  stopifnot(
    '** The raw_docx_file argument should be a character string **' = 
      is.character(raw_docx_file),
    '** The clean_docx_file argument should be a character string **' = 
      is.character(clean_docx_file),
    '** The min_str_length argument should be an integer **' =
      is.numeric(min_str_length),
    '** The font_family argument should be a character string ("Aptos" or "Times") **' =
      is.character(font_family),
    '** The font_size argument should be an integer **' = 
      is.numeric(font_size),
    '** The line_spacing argument should be an integer **' = 
      is.numeric(line_spacing)
    )
  
  # Read in file with officer and get a summary object
  dat <- read_docx(raw_docx_file) %>% 
    docx_summary()
  
  # Check that no text line has more than one colon
  checks <- map(dat$text, ~ str_count(.x, ':'))
  if (any(checks > 1)) {
    warning(
      '** More than one colon in',
      {{ raw_docx_file }},
      ' paragraph(s) ',
      paste(which(checks > 1), collapse = ', '),
      '. Check for errors! **',
      call. = FALSE
    )
    cat(
      '\nProblem paragraphs in ',
      {{ raw_docx_file }},
      ':',
      paste0('\n', dat$text[which(checks > 1)] %>% str_sub(end = 80)),
      '\n\n',
      sep = ''
    )
  } 

  # Pull out first few lines of front matter and save for later
  front_matter <- dat$text[c(1:3)]
  
  
  # Cleaning -----
  
  # Replace first colon with filler
  dat$text <- str_replace(dat$text, ':', 'first_colon')
  dat$name <- str_split_i(dat$text, 'first_colon', 1)
  dat$text <- str_split_i(dat$text, 'first_colon ', 2)

  dat <- dat %>%

    # Remove avatars (style_name == NA)
    filter(!is.na(style_name))
  
    # Fix first line (still has series of numbers for avatar)
    # mutate(text = str_remove_all(text, "[^a-zA-Z\\s:\\.]"))
  
  # Smush -----
  
  browser() # [] here, but this isn't quite right. Some names are fuckes up
  
  # First consolidate all rows missing a name
  i <- 2
  while (i <= nrow(dat)) {
    if (str_detect(dat[['text']][i], ':', negate = TRUE)) {
      dat[['text']][i - 1] <- paste(dat[['text']][i - 1], dat[['text']][i])
      dat <- dat[-i, ]
    } else {
      i <- i + 1
    }
  }
  
  # Now consolidate all consecutive rows with the same name
  i <- 2
  while (i <= nrow(dat)) {
    if (str_split_i(dat[['text']][i], ':', 1) == str_split_i(dat[['text']][i - 1], ':', 1)) {
      dat[['text']][i] <- str_split_i(dat[['text']][i], ': ', 2)
      dat[['text']][i - 1] <- paste(dat[['text']][i - 1], dat[['text']][i])
      dat <- dat[-i, ]
    } else {
      i <- i + 1
    }
  }
  
  # Then remove any lines with less than certain length IF between lines from 
  # the same speaker
  i <- 2
  tryCatch({
    while(i < nrow(dat)) {
      if (str_length(str_split_i(dat[['text']][i], ': ', 2)) <= min_str_length &&
          str_split_i(dat[['text']][i - 1], ':', 1) == str_split_i(dat[['text']][i + 1], ':', 1)) {
        dat <- dat[-i, ]
      } else {
        i <- i + 1
      }
    }
  },
  error = function(e) {
    message(cat(
      'Error in line ',
      i,
      ' (After consolidating lines): ',
      dat$text[i],
      sep = ''
    ))
  })
  
  # Now we have to smush again (remove all consecutive rows with the same name)
  i <- 2
  while (i <= nrow(dat)) {
    if (str_split_i(dat[['text']][i], ':', 1) == str_split_i(dat[['text']][i - 1], ':', 1)) {
      dat[['text']][i] <- str_split_i(dat[['text']][i], ': ', 2)
      dat[['text']][i - 1] <- paste(dat[['text']][i - 1], dat[['text']][i])
      dat <- dat[-i, ]
    } else {
      i <- i + 1
    }
  }
  
  # Write to docx -----
  
  # Set fonts to be used later - one bold, one normal
  bold_font <- fp_text(
    font.size = font_size,
    font.family = font_family,
    bold = TRUE
  )
  
  normal_font <- fp_text(
    font.size = font_size,
    font.family = font_family,
  )
  
  # Start a new docx file
  doc <- read_docx()

  # Write each name and line to docx file with formatting
  for (line in dat$text) {
    split <- str_split_1(line, ': ')
    paragraph <- fpar(
      ftext(split[[1]], prop = bold_font),
      ftext(': ', prop = bold_font),
      ftext(split[[2]], prop = normal_font),
      fp_p = fp_par(line_spacing = line_spacing)
    )
    doc <- doc %>%
      body_add_fpar(paragraph) %>%
      body_add_par('')
  }
  
  # Save docx file to specified path
  print(doc, target = clean_docx_file)
}
  
