embed_text_ui <- function(id){
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinycssloaders::withSpinner(
      shiny::uiOutput(ns("embeddedTexts"))
    )
  )
}

embed_text_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_texts <- shiny::reactive({
      req(shiny::isTruthy(r$selected_range))
      
      # print(r$selected_range)
      
      permalinks <- r$df() %>%
        dplyr::filter(universal_message_id %in% r$selected_range) %>%
        dplyr::pull(permalink)
      
      # permalinks <- reverse_link_click_html(permalinks)

      return(permalinks)
    })
    
    permalink_embeds <- reactive({
      if(!length(selected_texts()) > 0){
        validate("Select some data first!")
      }
      
      # take a sample to speed up rendering
      if(length(selected_texts()) > 15){
        sample_selected <- selected_texts()[sample(length(selected_texts()), 15)]
      } else {
        sample_selected <- selected_texts()
      }
      
      embedded_posts <- lapply(sample_selected, embed_switch)
      Filter(Negate(is.null), embedded_posts)
      
    })
    
  

    output$embeddedTexts <- renderUI({
      do.call(bslib::layout_column_wrap, c(width = 1/3, permalink_embeds()))
    })

  })
}