searchUi <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::textInput(NS(id, "search_term"), "Enter search term:", placeholder = "face"),
    htmltools::tags$style(HTML("
    .irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
      visibility: hidden !important;
    }"
                               )),
    htmltools::div(
      style = "position: relative; margin-top: 20px; width: 200px;",
      shiny::sliderInput(ns("semantic_sim_threshold"), 
                  label = NULL,
                  min = 0, max = 1, value = 0.5, ticks = FALSE),
      htmltools::div( # slider title
        style = "position: absolute; top: -15px; left: 20%; transform: translateX(-30%);",
        "Term Similarity"
      ),
      htmltools::div( # slider lower bound label
        style = "position: absolute; top: 45px; left: 0; transform: translateX(-20%); font-family: Cinzel-Regular; src: fonts/Cinzel-Regular.ttf;",
        "Low"
      ),
      htmltools::div( #  slider halfway label
        style = "position: absolute; top: 45px; left: 50%; transform: translateX(-50%); font-family: Cinzel-Regular; src: fonts/Cinzel-Regular.ttf;",
        "Medium"
      ),
      htmltools::div( # slider upper bound label
        style = "position: absolute; top: 45px; left: 100%; transform: translateX(-70%); font-family: Cinzel-Regular; src: fonts/Cinzel-Regular.ttf;",
        "High"
        )
      ),
  htmltools::tags$div(style = "margin-top: 10px; margin-bottom: 10px;"),   # break before action button
  shiny::actionButton(NS(id, "update_plot"), "Update Plot")
  )
}

searchServer <- function(id, r) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # cosmetic_sentences <- readr::read_rds("~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/cleaned_data/for_app/cosmetic_sentences.rds")
    # 
    # cosmetic_sentences_embeddings <- readr::read_rds("~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/cleaned_data/for_app/cosmetic_sentences_embeddings.rds") %>%
      # as.matrix()
    
    # COMMENTING THIS OUT TO SPEED UP ITERATIVE DEVELOPMENT - NEED TO UNCOMMENT ----
    # file_paths <- c("for_app/cosmetic_sentences_embeddings.rds",
    #                 "for_app/cosmetic_sentences.rds",
    #                 "for_app/automotive_sentences_embeddings.rds",
    #                 "for_app/automotive_sentences.rds",
    #                 "for_app/food_beverage_sentences_embeddings.rds",
    #                 "for_app/food_beverage_sentences.rds")
    # 
    # for (file_path in file_paths) {
    #   
    #   file <- googledrive::drive_get(file_path)
    #   temp_file <- tempfile(fileext = ".rds")
    #   googledrive::drive_download(file, path = temp_file, overwrite = TRUE)
    #   
    #   category <- sub("for_app/(.*)\\.rds", "\\1", file_path)
    #   
    #   data <- readRDS(temp_file)
    #   assign(category, data)
    # }
    # ----
    
    
    observeEvent(input$update_plot, {
      
      shiny::validate(
        shiny::need(grepl("^[a-zA-Z0-9 ]*$", input$search_term), "Invalid characters detected! Please use only alphanumeric characters and spaces.")
        )
      
      keyword_search <- r$df() %>%
        dplyr::filter(grepl(input$search_term, text_clean, ignore.case = TRUE))
        # dplyr::filter(grepl("face", text_clean, ignore.case = TRUE))
      
      sentence_embeddings <- switch(r$input_dataset,
                                "Beauty & Cosmetics" = cosmetic_sentences_embeddings,
                                "Automotive" = automotive_sentences_embeddings,
                                "Food & Beverages" = food_data_sentences_embeddings
      )
      
      sentence_df <- switch(r$input_dataset,
                            "Beauty & Cosmetics" = cosmetic_sentences,
                            "Automotive" = automotive_sentence,
                            "Food & Beverages" = food_data_sentences
      )
      
      semantic_similarity_output <- cosine_calculation_threshold_sentence(
        reference_statement = input$search_term,
        cosine_sim_threshold = input$semantic_sim_threshold,
        # reference_statement = "face",
        # cosine_sim_threshold = 0.0,
        embedding_model = "multi-qa-mpnet-base-cos-v1",
        # sentence_matrix = multi_qa_matrix_sentences,
        # df = example_sentences
        sentence_matrix = sentence_embeddings,
        df = sentence_df
      ) %>% 
        process_sentences(example_sentences)
      
      r$highlight_df <- shiny::reactive({
        
        if(!is.null(semantic_similarity_output)){
          semantic_similarity_output %>%
            dplyr::filter(highlighted == TRUE) %>%
            dplyr::bind_rows(keyword_search) %>%
            dplyr::distinct(universal_message_id, .keep_all = TRUE) 
        } else{
          keyword_search
        }
        
        })
      
      print(nrow(r$highlight_df()))
      print(class(semantic_similarity_output))
      r$grey_df <- shiny::reactive({
        r$df() %>%
          dplyr::anti_join(r$highlight_df(), by = "universal_message_id")
      })
      
      print(nrow(r$grey_df()))
      

    })
    
    
    })
  }



