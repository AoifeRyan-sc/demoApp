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
    
    # COMMENTING THIS OUT TO SPEED UP ITERATIVE DEVELOPMENT - NEED TO UNCOMMENT ----
    # file_paths <- c("for_app/cosmetic_df.rds", 
    #                 "for_app/automotive_df.rds", 
    #                 "for_app/food_beverage_df.rds")
    # 
    # for (file_path in file_paths) {
    # 
    #   file <- googledrive::drive_get(file_path)
    #   temp_file <- tempfile(fileext = ".rds")
    #   googledrive::drive_download(file, path = temp_file, overwrite = TRUE)
    # 
    #   category <- sub("for_app/([a-z]+)_.*", "\\1", file_path)
    #   
    #   data <- readRDS(temp_file)
    #   assign(paste0(category, "_data"), data)
    # }
    
    # ----

    df <- shiny::reactive({
      test <- switch(input$dataset,
             # "Beauty & Cosmetics" = cosmetic_data
             "Beauty & Cosmetics" = cosmetic_data_test,
             "Automotive" = automotive_data,
             "Food & Beverages" = food_data
             )
    })
    
    
    shiny::observeEvent(df(), {
      r$df <- df
      r$input_dataset <- input$datset
      })

    
  })
}