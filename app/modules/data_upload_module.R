dataUploadUi <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::br(),
    shiny::tags$head(
      # Ensure Font Awesome is included
      shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css")
    ),
    shinyWidgets::pickerInput(
      inputId = ns("dataset"),
      label = "Dataset",
      choices =  c("Beauty & Cosmetics",
                   "Automotive",
                   "Food & Beverages"),
      options = list(`icon-base` = "fa"),
      choicesOpt = list(
        icon = c("fa-spa",
                 "fa-car",
                 "fa-burger")
      ),
    )
  )
}

dataUploadServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- shiny::eventReactive(input$dataset, {
      file_path <- switch(input$dataset,
                           "Beauty & Cosmetics" = "for_app/cosmetic_df.rds",
                           "Automotive" = "for_app/automotive_df.rds",
                           "Food & Beverages" = "for_app/food_beverage_df.rds")

        file <- googledrive::drive_get(file_path)
        temp_file <- tempfile(fileext = ".rds")
        googledrive::drive_download(file, path = temp_file, overwrite = TRUE)

        category <- sub("for_app/([a-z]+)_.*", "\\1", file_path)
        
        readRDS(temp_file)
    })
    
    sentence_df <- shiny::reactive({
      
      file_path <- switch(input$dataset,
                          "Beauty & Cosmetics" = "for_app/cosmetic_sentences.rds",
                          "Automotive" = "for_app/automotive_sentences.rds",
                          "Food & Beverages" = "for_app/food_beverage_sentences.rds")
      
      file <- googledrive::drive_get(file_path)
      temp_file <- tempfile(fileext = ".rds")
      googledrive::drive_download(file, path = temp_file, overwrite = TRUE)
      
      df <- readRDS(temp_file)
      
      if ("text_copy" %in% colnames(df)){
        df %>% dplyr::rename(text_clean = text_copy)
      }
      
      df
      
    })

    sentence_embeddings <- shiny::reactive({
      file_path <- switch(input$dataset,
                          "Beauty & Cosmetics" = "for_app/cosmetic_sentences_embeddings.rds",
                          "Automotive" = "for_app/automotive_sentences_embeddings.rds",
                          "Food & Beverages" = "for_app/food_beverage_sentences_embeddings.rds"
      ) 
      
      file <- googledrive::drive_get(file_path)
      temp_file <- tempfile(fileext = ".rds")
      googledrive::drive_download(file, path = temp_file, overwrite = TRUE)
      
      readRDS(temp_file) %>% as.matrix()
    }) 
    
    observeEvent(input$dataset, { 
      sentence_embeddings()
      sentence_df()
    }) # lazy reactivity means reactives need to be called to run
    
    
    shiny::observeEvent(df(), {
      r$df <- df
      r$sentence_df <- sentence_df
      r$sentence_embeddings <- sentence_embeddings
      r$input_dataset <- input$dataset
      })
  
    
  })
}