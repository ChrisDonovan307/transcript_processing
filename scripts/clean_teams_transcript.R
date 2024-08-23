#' clean_transcript
#' 2024-08-14

#' This is the new one with the pictures/numbers to get rid of. Correponds to
#' the fiddle file.


# Packages ----------------------------------------------------------------


pacman::p_load(
  dplyr,
  officer,
  purrr,
  stringr
)



# Function ----------------------------------------------------------------


clean_teams_transcript <- function(raw_docx_file,
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
  # checks <- map(dat$text, ~ str_count(.x, ':'))
  # if (any(checks > 1)) {
  #   warning(
  #     '** More than one colon in',
  #     {{ raw_docx_file }},
  #     ' paragraph(s) ',
  #     paste(which(checks > 1), collapse = ', '),
  #     '. Check for errors! **',
  #     call. = FALSE
  #   )
  #   cat(
  #     '\nProblem paragraphs in ',
  #     {{ raw_docx_file }},
  #     ':',
  #     paste0('\n', dat$text[which(checks > 1)] %>% str_sub(end = 80)),
  #     '\n\n',
  #     sep = ''
  #   )
  # } 

  # Pull out first few lines of front matter and save for later
  front_matter <- dat$text[1:2]
  
  
  # Cleaning -----
  
  # Get rid of front matter
  corpus <- dat[5:nrow(dat) - 1, ]
  
  # Remove NA rows
  corpus <- corpus %>% 
    filter(text != '')
  
  # Split text into columns for name, time, and text
  preliminary <- str_split(corpus$text, '\\s{3}')
  clean_name <- map_chr(preliminary, ~ .x[[1]][1])
  
  # Find first colon
  first_colon_pos <- map(preliminary, ~ str_locate(.x[[2]], ':')[1])
  
  # Replace colon with filler 
  preliminary_replaced <- map(preliminary, 
                              ~ str_replace(.x[[2]], ':', 'first_colon'))
  
  # Now split on filler, and pull out time (clean it) and text
  secondary <- str_split(preliminary_replaced, '(?<=first_colon[0-9]{2})')
  clean_time <- map_chr(secondary, ~ str_replace(.x[[1]], 'first_colon', ':'))
  clean_text <- map_chr(secondary, ~ .x[[2]])
  
  # Put them into corpus DF
  corpus <- corpus %>%
    mutate(
      name = clean_name,
      time = clean_time, 
      text = clean_text
    )
  
  # Remove NA rows and remove numbers from name
  corpus <- corpus %>% 
    filter(text != '') %>% 
    mutate(name = str_remove_all(name, '[^a-zA-Z\\s\\,]'))
  
  # Consolidate all consecutive rows with the same name
  i <- 2
  df <- corpus
  while (i <= nrow(df)) {
    if (df$name[i] == df$name[i - 1]) {
      df$text[i - 1] <- paste(df$text[i - 1], df$text[i])
      df <- df[-i, ]
    } else {
      i <- i + 1
    }
  }
  
  # Remove interjections
  i <- 2
  while(i < nrow(df)) {
    if (str_length(df$text[i]) < min_str_length && df$name[i + 1] == df$name[i - 1]) {
      df <- df[-i, ]
    } else {
      i <- i + 1
    }
  }
  
  # Consolidate rows with same name (again)
  i <- 2
  df <- corpus
  while (i <= nrow(df)) {
    if (df$name[i] == df$name[i - 1]) {
      df$text[i - 1] <- paste(df$text[i - 1], df$text[i])
      df <- df[-i, ]
    } else {
      i <- i + 1
    }
  }
  
  corpus <- df
  
  
  # Write to docx -----
  
  bold_font <- fp_text(
    font.size = font_size,
    font.family = font_family,
    bold = TRUE
  )
  
  normal_font <- fp_text(
    font.size = font_size,
    font.family = font_family
  )
  
  # Start a new docx file
  doc <- read_docx() %>% 
    body_add_fpar(fpar(ftext(front_matter[2], prop = bold_font))) %>%
    body_add_par('')
  
  # Write each name and line to docx file with formatting
  for (i in seq_along(corpus$text)) {
    paragraph <- fpar(
      ftext(corpus$name[i], prop = bold_font),
      ftext(' ['),
      ftext(corpus$time[i], prop = normal_font),
      ftext('] '),
      ftext(corpus$text[i], prop = normal_font),
      fp_p = fp_par(line_spacing = line_spacing)
    )
    doc <- doc %>%
      body_add_fpar(paragraph) %>%
      body_add_par('')
  }
  
  # Save to docx
  print(doc, target = clean_docx_file)
  
}