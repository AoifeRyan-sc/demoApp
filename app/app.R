# this is for local use of an application - not for deployment. 
# when deploying with the multiple file set up (ui.R, server.R)
# you need to ensure your scripts etc. are all sourced - good place for this
# is global.R which is automatically sourced.

library(shiny)
library(here)
library(magrittr)

# Temporary to allow for live reloads in an RStudio background job or app running in a terminal:
options(shiny.autoreload=TRUE)
options(shiny.port = 7775)
options(shiny.host = "127.0.0.1")

# Source Business logic/helper functions
source(here("app/R/umap_helper_functions.R"))
source(here("app/R/semantic_search_helper_functions.R"))
source(here("app/R/html_embed.R"))# Source modules

# source(here("app/www/x_logo.png"))

source(here("app/modules/umap_module.R"))
source(here("app/modules/data_upload_module.R"))
source(here("app/modules/search_module.R"))
source(here("app/modules/embed_texts_module.R"))

# Source App Files
source(here("app/ui.R"))
source(here("app/server.R"))

# profvis::profvis({
    # shiny::runApp(here::here("app"))
# })
# Run an app the IDE way if in an interactive session, else run it the terminal/deployment way:
if(interactive()){
  shinyApp(ui, server)
} else {
  shiny::runApp(here::here("app"))
}
