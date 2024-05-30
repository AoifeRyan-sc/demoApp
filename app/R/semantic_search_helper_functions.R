
# Utils -------------------------------------------------------------------

highlight_sentences <- function(text, sentences) {
  sentences <- unique(sentences)
  
  for (sentence in sentences) {
    highlighted_sentence <- paste0("<b>", sentence, "</b>")
    
    text <- stringr::str_replace_all(text, stringr::fixed(sentence), highlighted_sentence)
  }
  
  return(text)
}

insert_line_breaks <- function(text, n = 10) {
  
  words <- strsplit(text, " ")[[1]]
  
  paste(sapply(seq(1, length(words), n), 
               function(i) { paste(words[i:min(i + n - 1, length(words))], collapse = " ") }), 
        collapse = "<br>")
  
}

insert_line_breaks_test <- function(text, n = 10) {
  words <- unlist(strsplit(text, " "))
  num_words <- length(words)
  seq_indices <- seq(1, num_words, by = n)
  
  # Pre-allocate result vector
  result <- character(length(seq_indices))
  
  # Use vapply for better performance
  result <- vapply(seq_indices, function(i) {
    paste(words[i:min(i + n - 1, num_words)], collapse = " ")
  }, FUN.VALUE = character(1))
  
  paste(result, collapse = "<br>")
}

process_sentences_old <- function(doc_id, example_sentences) {
  if (nrow(doc_id) == 0){
    return(NULL)
  } else {
    doc_id %>%
      dplyr::select(-text_with_breaks) %>%
      dplyr::group_by(universal_message_id) %>% # Change to appropriate document column
      dplyr::mutate(
        # sentences = sentences,
        text_copy = dplyr::first(text_clean) # Change to appropriate text column
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(test_text = purrr::map2_chr(text_copy, sentences, highlight_sentences)) %>%
      dplyr::distinct(universal_message_id, .keep_all = TRUE) %>% # Change to appropriate document column
      dplyr::right_join(example_sentences) %>%
      dplyr::mutate(highlighted = dplyr::case_when(is.na(cosine_sim) ~ FALSE,
                                                   T ~ TRUE)) %>% # Change to appropriate document column
      dplyr::distinct(universal_message_id, .keep_all = TRUE) %>%
      dplyr::mutate(test_text = dplyr::case_when(
        is.na(cosine_sim) ~ text_copy,
        TRUE ~ test_text
      )) %>% 
      dplyr::mutate(text_with_breaks = sapply(test_text, insert_line_breaks))
  }
}


process_sentences <- function(doc_id, example_sentences) {
  if (nrow(doc_id) == 0){
    return(NULL)
  } else {
    doc_id %>%
      dplyr::select(-text_with_breaks) %>%
      dplyr::group_by(universal_message_id) %>% # Change to appropriate document column
      dplyr::mutate(text_copy = dplyr::first(text_clean)) %>%
      dplyr::ungroup() %>%
      # dplyr::mutate(test_text = purrr::map2_chr(text_copy, sentences, highlight_sentences)) %>%
      dplyr::mutate(test_text = purrr::map2_chr(text, sentences, highlight_sentences)) %>%
      dplyr::distinct(universal_message_id, .keep_all = TRUE) %>% 
      # dplyr::mutate(test_text = dplyr::case_when(
      #   is.na(cosine_sim) ~ text_copy,
      #   TRUE ~ test_text
      # )) %>% 
      dplyr::mutate(text_with_breaks = lapply(test_text, insert_line_breaks))
  }
}

# Embed Query ------------------------------------------------------------

embed_query <- function(query, embedding_model) {
  
  api_token <- Sys.getenv("HUGGINGFACE_API_KEY")
  base_hf_st_url <- "https://api-inference.huggingface.co/pipeline/feature-extraction/sentence-transformers/"
  endpoint_hf_st <- embedding_model
  
  # create the request with retry mechanism
  response <- httr2::request(base_hf_st_url) %>%
    httr2::req_url_path_append(endpoint_hf_st) %>% 
    httr2::req_headers(
      "Authorization" = paste("Bearer", api_token)
    ) %>%
    httr2::req_body_json(query) %>%
    httr2::req_retry(max_tries = 10, # select a maximum of 10 retries
                     backoff = ~ 2, # constant 2s delay after failure
                     is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500, 503)) %>% # specify common transient errors
    httr2::req_perform()
  
  # if response status isn't 200(OK), stop function
  if (httr2::resp_status(response) != 200) {
    # if transient error met, print message
    if (httr2::resp_status(response) %in% c(429, 500, 503)) {
      message("Max retries reached and timed out, please try again or check server-side connection.")
    }
    stop("Request failed with status: ", httr2::resp_status(response))
  }
  
  # return cosine similarity matrix
  data <- httr2::resp_body_json(response) %>% 
    unlist() %>% 
    as.matrix()
  return(data)
}

# Cosine similarity calculation -------------------------------------------

cosine_calculation_threshold_sentence <- function(reference_statement,
                                                  cosine_sim_threshold = 0.5,
                                                  embedding_model,
                                                  sentence_matrix,
                                                  df) {
  ref_sentence <- reference_statement
  
  reference_vector <- embed_query(query = ref_sentence, embedding_model = embedding_model)
  
  sentence_dot_products <- sentence_matrix %*% reference_vector
  
  sentence_norm_matrix <- sqrt(rowSums(sentence_matrix ^ 2))
  
  reference_norm <- sqrt(sum(reference_vector ^ 2))
  
  sentence_cosine_sims <- sentence_dot_products / (sentence_norm_matrix * reference_norm)
  
  current_sentence_candidates <- df %>%
    dplyr::mutate(cosine_sim = as.numeric(sentence_cosine_sims)) %>%
    dplyr::relocate(cosine_sim) %>%
    dplyr::filter(cosine_sim > cosine_sim_threshold) %>%
    dplyr::arrange(desc(cosine_sim))
  
  return(current_sentence_candidates)
  
}


keyword_search <- function(df, semantic_sim, search_term){
  df %>%
    dplyr::anti_join(semantic_sim, by = "universal_message_id") %>%
    dplyr::filter(grepl(search_term, text_clean, ignore.case = TRUE)) %>%
    dplyr::mutate(text_with_breaks = purrr::map2_chr(text, search_term, highlight_sentences),
                  text_with_breaks = lapply(text_with_breaks, insert_line_breaks))
}