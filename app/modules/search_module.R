searchUi <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::textInput(ns("search_term"), "Enter search term:", placeholder = "face") %>%
      bslib::popover(ns("search_input_popover"),
                     "This searches the dataset for 
                     both the specific word(s) entered 
                     and phrases that have a similar meaning
                     to the word(s) entered. You can enter a 
                     single word or an idea that you would 
                     like to search for."),
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
      ) %>%
        bslib::popover(ns("slider_popover"), 
        "This adjusts how similar 
                       you would like the output 
                       to be to the input search term. 
                       Setting the slider all the way to 
                       the left sets the similarity to 0 
                       and so you are only searching for 
                       the exact word(s) entered"),
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
  shiny::fluidRow(
    shiny::column(
      width = 6,
      shiny::actionButton(ns("update_plot"), "Update Plot") 
    ),
    shiny::column(
      width = 6,
      shiny::actionButton(ns("reset_plot"), "Reset Plot") 
    )
  )
  )
}

searchServer <- function(id, r) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
    
    # r$calculating_plot <- shiny::reactiveVal(FALSE)
    observeEvent(input$update_plot, {
      # r$calculating_plot <- TRUE
      shiny::validate(
        shiny::need(
          input$search_term != "",
          "Enter a search term before updating plot"
        )
      )
      shiny::validate(
        shiny::need(grepl("^[a-zA-Z0-9 ]*$", input$search_term), 
                    "Invalid characters detected! Please use only alphanumeric characters and spaces.")
        )
      
      sentence_embeddings <- switch(r$input_dataset,
                                "Beauty & Cosmetics" = cosmetic_sentences_embeddings,
                                "Automotive" = automotive_sentences_embeddings,
                                "Food & Beverages" = food_data_sentences_embeddings
      ) %>% as.matrix()
      
      sentence_df <- switch(r$input_dataset,
                            "Beauty & Cosmetics" = cosmetic_sentences,
                            "Automotive" = automotive_sentences,
                            "Food & Beverages" = food_data_sentences
      )
      semantic_similarity_output <- cosine_calculation_threshold_sentence(
        reference_statement = input$search_term,
        cosine_sim_threshold = input$semantic_sim_threshold,
        # reference_statement = "face",
        # cosine_sim_threshold = 0.5,
        embedding_model = "multi-qa-mpnet-base-cos-v1",
        sentence_matrix = sentence_embeddings,
        df = sentence_df
        # sentence_matrix = as.matrix(cosmetic_sentences_embeddings),
        # df = cosmetic_sentences
        ) %>% 
        # process_sentences(cosmetic_sentences)
        process_sentences(sentence_df)
        
       keyword_search_output <- keyword_search(
         df = r$df(),
         # df = cosmetic_data,
         semantic_sim = semantic_similarity_output, 
         search_term = input$search_term
         # search_term = "face"
       )
     
      r$highlight_df <- shiny::reactive({
        
        if(!is.null(semantic_similarity_output)){
          
          semantic_similarity_output %>%
            dplyr::bind_rows(keyword_search_output) 
           
        } else{
          keyword_search_output
        }
        
        })
      
      r$grey_df <- shiny::reactive({
        r$df() %>%
          dplyr::anti_join(r$highlight_df(), 
                           by = "universal_message_id")

      })

    })
    
    # observeEvent(input$reset_plot, {
    #   r$calculating_plot <- FALSE
    #   r$grey_df <- NULL
    #   r$highlight_df <- NULL
    # })
    
    
    })
  }



