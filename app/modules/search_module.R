searchUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    htmltools::div(
      div(
          style = "position: relative;",
          shiny::textInput(ns("search_term"), "Enter search term:", placeholder = "face"), 
        div(
          style = "position: absolute; top: 0; right: 5px; transform: translateX(-20%);",
          bsicons::bs_icon("question-circle-fill")
        ) %>%
          bslib::popover("This searches the dataset for 
                     both the specific word(s) entered
                     and phrases that have a similar meaning
                     to the word(s) entered. You can enter a
                     single word or an idea that you would
                     like to search for.")
      ) 
    ),
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
      htmltools::div(
        style = "position: absolute; top: -15px; right: -10px; transform: translateX(-20%);",
        bsicons::bs_icon("question-circle-fill")
      ) %>%
        bslib::popover("This adjusts how similar 
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
    
    # r$calculating_plot <- shiny::reactiveVal(FALSE)
    observeEvent(input$update_plot, {
      # the shiny::need error messages aren't displaying so also using showNotification
      if (input$search_term == "") {
        shiny::showNotification("Enter a search term before updating plot", type = "error")
        return(NULL)
      }
      
      if (!grepl("^[a-zA-Z0-9 ]*$", input$search_term)){
        shiny::showNotification("Invalid characters detected! Please use only alphanumeric characters and spaces.", type = "error")
      }
      
      if (input$search_term == "") {
        shiny::showNotification("Error: Please enter some text.", type = "error")
        return(NULL)
      }
      shiny::validate(
        shiny::need(
          input$search_term != "",
          message = FALSE
        )
      )
      shiny::validate(
        shiny::need(grepl("^[a-zA-Z0-9 ]*$", input$search_term), 
                    message = FALSE)
        )
      semantic_similarity_output <- cosine_calculation_threshold_sentence(
        reference_statement = input$search_term,
        cosine_sim_threshold = input$semantic_sim_threshold,
        # reference_statement = "face",
        # cosine_sim_threshold = 0.5,
        embedding_model = "multi-qa-mpnet-base-cos-v1",
        sentence_matrix = r$sentence_embeddings(),
        df = r$sentence_df()
        # sentence_matrix = as.matrix(cosmetic_sentences_embeddings),
        # df = cosmetic_sentences
        ) %>% 
        # process_sentences(cosmetic_sentences)
        process_sentences(r$sentence_df())
        
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
    
    observeEvent(input$reset_plot, {
      # r$calculating_plot <- FALSE
      r$grey_df <- NULL
      r$highlight_df <- NULL
    })
    
    
    })
  }



