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
    
    file_paths <- c("for_app/cosmetic_df.rds", 
                    "for_app/automotive_df.rds", 
                    "for_app/food_beverage_df.rds")
    
    for (file_path in file_paths) {
      file <- googledrive::drive_get(file_path)
      temp_file <- tempfile(fileext = ".rds")
      googledrive::drive_download(file, path = temp_file, overwrite = TRUE)

      category <- sub("for_app/([a-z]+)_.*", "\\1", file_path)
      
      data <- readRDS(temp_file)
      assign(paste0(category, "_data"), data)
    }
    
    df <- shiny::reactive({
      switch(input$dataset,
             # "Beauty & Cosmetics" = "cosmetic",
             # "Automotive" = "automotive",
             # "Food & Beverages" = "technology"
             "Beauty & Cosmetics" = cosmetic_data,
             "Automotive" = automotive_data,
             "Food & Beverages" = food_data
             )
    })
    
    
    # df <- reactive({
      # data <- readr::read_rds(here::here("~/Google Drive/My Drive/Share_Clients/data_science_project_work/hackafun/data/cleaned_data/for_app/cosmetic_df.rds"))
      # 
      # file <- googledrive::drive_get(paste0("for_app/", category(), "_df.rds"))
      # temp_file <- tempfile(fileext = ".rds")
      # googledrive::drive_download(file, path = temp_file, overwrite = TRUE)
      # readr::read_rds(temp_file) %>%
      #   dplyr::mutate(text_with_breaks = sapply(text, insert_line_breaks))
      
      

      # ----
    # })
    
    shiny::observeEvent(df(), {
      r$df <- df
      })

    
  })
}