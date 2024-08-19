#' Walkthrough
#' 2024-08-14



# Load Packages and Data --------------------------------------------------


pacman::p_load(
  dplyr,
  officer,
  purrr,
  stringr
)



# Use Function ------------------------------------------------------------


# This section shows how to run the function to clean a transcript in one step.

# First we load the function
source('scripts/clean_transcript.R')

# Now use it to clean the transcript. There are only two required arguments.
clean_transcript(
  raw_docx_file = 'raw_docx/14. 1 - John Ibitoye Transcript.docx',
  clean_docx_file = 'clean_docx/cleaned_transcript.docx'
)

#' But if you want more control over the format, you can specify a few things.
#' The min_str_length specifies the smallest string acceptable in a line from 
#' person A when interjecting between two lines from person B. The default is
#' 6. You can also choose the font, font size, and line spacing. The defaults
#' are 'Aptos', 12, and 1.
clean_transcript(
  raw_docx_file = 'docx_files/raw_transcript.docx',
  clean_docx_file = 'docx_files/cleaned_transcript_2.docx',
  min_str_length = 6,
  font_family = 'Calibri',
  font_size = 11,
  line_spacing = 2
)



# Explore  ----------------------------------------------------------------


#' Now let's walk through the process manually to see how it works. This section
#' just explores the docx file in R. 

# First we load our file with officer
raw <- read_docx('raw_docx/raw_transcript.docx')

# Then we get a summary object which is what we will work with
dat <- docx_summary(raw)

# Explore the docx summary
str(dat)
#' doc index is line number
#' style_name is either default_paragraph (for legit line) or NA (blank line)
#' text starts with number (avatar) before name, then colon before text

# We can also explore the comments if we want
comments <- docx_comments(raw)
comments
str(comments, strict.width = 'cut')
# Pretty neat!



# Clean -------------------------------------------------------------------


#' Now we start cleaning. First, let's load another function that will help us. 
#' We will use it a bit later.
source('scripts/smush_text.R')

# Remove the avatars. They show up as style_name == NA.
dat <- filter(dat, !is.na(style_name))
str(dat)
# This fixes everything but the first line. It still has weird numbers.

#' For first line, let's remove everything that isn't a letter, colon, space, or
#' period.
dat <- mutate(dat, text = str_remove_all(text, "[^a-zA-Z\\s:\\.]"))
str(dat)
head(dat$text)
# Now we have cleanish lines separated by blanks

#' Smush multiple lines of text with the same speaker together into one line. 
#' This is a bit of a messy process, so I pulled it out and made it into this
#' smush_text function. You can see if by using `CTRL + Left Click` on the 
#' function, or putting the cursor on it and pressing `F2`.
dat <- smush_text(dat)
str(dat)
dat$text
# This is clean!



# Write to docx -----------------------------------------------------------


#' Before we save to a doc file, set some fonts to be used later - one bold, 
#' one normal
bold_font <- fp_text(
  font.size = 12,
  font.family = "Aptos",
  bold = TRUE
)

normal_font <- fp_text(
  font.size = 12,
  font.family = "Aptos"
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
    fp_p = fp_par(line_spacing = 1)
  )
  doc <- doc %>%
    body_add_fpar(paragraph) %>%
    body_add_par('')
}

# Save to docx
print(doc, target = "docx_files/cleaned_transcript_manual.docx")