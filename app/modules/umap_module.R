#' UMAP ui function
#'
#' @param id param for shiny identification
#'
#' @noRd
umapUi <- function(id){
  ns <- shiny::NS(id)
    htmltools::div(
      style = "position: relative",
      shinycssloaders::withSpinner(
        # shiny::uiOutput(ns("display_plot"))
        plotly::plotlyOutput(ns("umap_plot"))
      ),
      htmltools::div(
      style = "position: absolute; top: 10px; left: 10px",
      shinyWidgets::prettySwitch(
      inputId = ns("plot_selection"),
      label = "Cluster View"
      )
      )
    )
}

#' UMAP server function
#'
#' @param id param for shiny identification
#'
#' @noRd
umapServer <- function(id, r){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # output$display_plot <- shiny::renderUI({
    #   if (input$plot_selection == FALSE){
    #     # print(r$calculating_plot)
    #     # shiny::req(r$calculating_plot == TRUE, r$input_dataset)
    #     # print("req satisfied")
    #     plotly::plotlyOutput(ns("umap_plot"))
    #   } else {
    #     # shiny::imageOutput(ns("raster_plot"))
    #     plotly::plotlyOuptu
    #   }
    # })
    
    observeEvent(input$plot_selection, {
      if (input$plot_selection == FALSE){
        r$cluster_type <- "kmeans"
      } else {
        r$cluster_type <- "hdbscan"
      }
    })
    
    output$umap_plot <- plotly::renderPlotly({
      if(is.null(r$highlight_df)){
        createUmap(df = r$df(), highlight_df = NULL, grey_df = NULL, cluster_type = r$cluster_type)
      } else{
        highlight_points <- r$highlight_df()
        grey_points = r$grey_df()
        
        createUmap(df = r$df(), highlight_df = highlight_points, grey_df = grey_points, cluster_type = r$cluster_type)
      }
    })
    
   
    
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "umap_plot"), {
      # r$selected_range <- plotly::event_data("plotly_selected", source = "umap_plot")$customdata
      r$selected_range <- plotly::event_data("plotly_selected", source = "umap_plot")$key

    })

    
  })
}