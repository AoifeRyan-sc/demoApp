#' Wrangle Columns Appropriate for demoApp Workflow
#'
#' @param data Raw data frame object the user wishes to wrangle and clean
#' @param text_var The original Text/Message column present within the raw data as a string
#'
#' @return The raw data with columns cleaned to snake_case, arranged and cut down to 9 necessary variables for the demoApp workflow
#' @export
#'
#' @examples 
#' data_cosmetic <- demoapp_wrangle_cols(data_cosmetic,
#' text_var = "Text")
#' data_cosmetic <- demoapp_wrangle_cols(data_cosmetic,
#' text_var = "Message")

demoapp_wrangle_cols <- function(data,
                                 text_var) {
  
  # ensure that data is of class dataframe and text_var is character
  stopifnot(is.data.frame(data) && is.character(text_var))
  
  # initial column cleaning of names and removal of NA cols and NA date
  data <- data %>% 
    janitor::clean_names() %>%
    LimpiaR::limpiar_na_cols(0.1) %>%
    dplyr::mutate(
      text_copy = !!rlang::sym(snakecase::to_snake_case(text_var)),
      date = lubridate::as_date(lubridate::dmy_hms(date))
    ) %>% 
    tidyr::drop_na(date)
  
  # sort conditional renaming of columns for columns exported from radarly
  if ("message" %in% names(data)) {
    data <- data %>% dplyr::rename(text = message)
  }
  if ("tone" %in% names(data)) {
    data <- data %>% dplyr::rename(sentiment = tone)
  }
  if ("radarly_id" %in% names(data)) {
    data <- data %>% dplyr::rename(universal_message_id = radarly_id)
  }
  if ("screen_name" %in% names(data)) {
    data <- data %>% dplyr::rename(sender_screen_name = screen_name)
  }
  
  # finally, selectively choose which columns to keep
  data <- data %>% 
    dplyr::select(date, text, text_copy, sentiment, permalink, sender_screen_name, platform, post_type, universal_message_id)
  
  return(data)
}

#' Perform Spam Removal Steps on Data and Cleaning of text_copy Variable for Embedding/Processing
#'
#' @param data The previously wrangled data frame object derived from `demoapp_wrangle_cols()` output
#' @param text_var The previously wrangled "text" column
#' @param text_copy A copy of "text", which will be processed and used for calculating embeddings
#' @param platform_var The column displaying information on social network/platform distribution to filter by
#' @param spams_n_gram The selected value for the n_gram argument of `LimpiaR::limpiar_spam_grams()` which is used within the data cleaning workflow, the default is `spams_n_gram = 9`.
#' @param spams_min_freq The selected value for the min_freq argument of `LimpiaR::limpiar_spam_grams()` which is used within the data cleaning workflow, the default is `spams_min_freq = 4`.
#'
#' @return The previously wrangled data frame object with a cleaned `text_copy` variable ready for embedding and with spam, unhelpful data and duplicated messages removed.
#' @export
#'
#' @examples
#' cleaned_data <- demoapp_data_cleaning(data,
#' text_var = "text",
#' text_copy = "text_copy",
#' platform_var = "platform",
#' spams_n_gram = 9,
#' spams_min_freq = 4)

demoapp_data_cleaning <- function(data,
                                  text_var = "text",
                                  text_copy = "text_copy",
                                  platform_var = "platform",
                                  spams_n_gram = 9,
                                  spams_min_freq = 4) {
  
  # ensure data is of class data.frame or similar and text columns are character
  stopifnot(is.data.frame(data) && is.character(text_var) && is.character(text_copy) && is.character(platform_var))
  
  # convert appropriate column names to symbols
  platform_sym <- dplyr::sym(platform_var)
  text_sym <- dplyr::sym(text_var)
  text_copy_sym <- dplyr::sym(text_copy)
  
  # filter for chosen sources
  data <- data %>% 
    dplyr::filter(!!platform_sym %in% c("X", "Instagram"))
  
  # define regex patterns
  hashtags_regex <- "(?<=#)[:graph:]*(?![:graph:])|(?<=#)[:graph:]*$|#" # hashtags
  mentions_regex <- "(?<=@)[:graph:]*(?![:graph:])|(?<=@)[:graph:]*$|@" # mentions
  emojis_regex <- "[^\x01-\x7F]" # emojis 
  non_english_regex <- "[^[:ascii:]]" # non-English and special characters/symbols
  
  # remove; hashtags, mentions and emojis
  data <- data %>%
    dplyr::mutate(!!text_copy_sym := !!text_copy_sym %>%
                    stringr::str_remove_all(pattern = hashtags_regex) %>% 
                    stringr::str_remove_all(pattern = mentions_regex) %>%
                    stringr::str_remove_all(pattern = emojis_regex))
  # remove URLs, non-English characters and clean unwanted spaces
  data <- data %>% 
    LimpiaR::limpiar_url(text_var = !!text_copy_sym) %>% 
    dplyr::mutate(!!text_copy_sym := !!text_copy_sym %>%
                    stringr::str_remove_all(pattern = non_english_regex)) %>% 
    LimpiaR::limpiar_spaces(text_var = !!text_copy_sym)
  
  # remove absolute duplicates + any super long/short posts
  data <- data %>% 
    LimpiaR::limpiar_duplicates(text_var = !!text_copy_sym) %>% 
    dplyr::filter(stringr::str_count(!!text_copy_sym) > 10 & stringr::str_count(!!text_copy_sym) < 2500)
  
  # count mentions and hashtags in each post and remove posts with more than 4 of both
  data <- data %>%
    dplyr::mutate(
      no_users_mentioned = stringr::str_count(!!text_sym, mentions_regex),
      no_hashtags_posted = stringr::str_count(!!text_sym, hashtags_regex)
    ) %>% 
    dplyr::filter(no_hashtags_posted <= 4, no_users_mentioned <= 4)
  
  # finally, run spam grams and remove spam
  spams <- data %>% 
    LimpiaR::limpiar_spam_grams(text_var = !!text_copy_sym, n_gram = spams_n_gram, min_freq = spams_min_freq)
  # remove unwanted columns created during cleaning
  data <- spams[[2]] %>% dplyr::select(-c(document, no_users_mentioned, no_hashtags_posted))
  
  return(data)
}