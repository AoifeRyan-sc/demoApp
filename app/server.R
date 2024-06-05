options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

server <- function(input, output, session) {
  # bslib::bs_themer()
  r <- shiny::reactiveValues(
    highlight_df = NULL
  )

  dataUploadServer("data_upload_panel", r)

  umapServer("umap_panel", r)

  embed_text_server("embed_text_panel", r)
  searchServer("semantic_search_panel", r)
}

# server <- function(input, output, session) {
# 
# 
# }
# 
