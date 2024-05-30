ui <- bslib::page_fillable(
  tags$style(HTML("g.hovertext > path {opacity: .8;}")),
  
  theme = bslib::bs_theme(
    bootswatch = "sandstone",
    heading_font = bslib::font_face(family = "Cinzel-SemiBold",
                                    src = "fonts/Cinzel-SemiBold.ttf"),
    base_font = bslib::font_face(family = "Cinzel-Regular",
                                 src = "fonts/Cinzel-Regular.ttf")
  ),

      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          dataUploadUi("data_upload_panel"),
          searchUi("semantic_search_panel"),
          open = TRUE),
        bslib::navset_card_tab(
          bslib::nav_panel(
            "Data Landscape",
            height = "900px",
            umapUi("umap_panel"),
            fillable = TRUE,
            fill = TRUE
          ),
          bslib::nav_panel(
            "Selected Posts",
            height = "900px",
            embed_text_ui("embed_text_panel"),
            fillable = TRUE,
            fill = TRUE
          ) 
        )
    )
    
  )

