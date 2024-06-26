adjust_colour_lighter <- function(colour_hex, og_val) {
  
  rgb_vals <- col2rgb(colour_hex)
  
  rgb_new <- rgb_vals * og_val + 255 * (1 - og_val)
  
  rgb_new <- pmin(rgb_new, 255)
  
  new_colour_hex <- rgb(rgb_new[1,] / 255, rgb_new[2,] / 255, rgb_new[3,] / 255)
  return(new_colour_hex)
}

adjust_colour_darker <- function(colour_hex, og_val) { # LEGACY 
  
  
  rgb_vals <- col2rgb(colour_hex)
  
  rgb_new <- rgb_vals * og_val
  
  rgb_new <- pmax(rgb_new, 0)
  
  new_colour_hex <- rgb(rgb_new[1,] / 255, rgb_new[2,] / 255, rgb_new[3,] / 255, 
                        maxColorValue = 5)
  return(new_colour_hex)
}

umapColourCreate <- function(df){
  
  topics <- unique(df$kmeans_topic_title) # number of colours
  colours <- viridis::viridis( # colour shades
    n = length(topics), 
    begin = 0, end = 0.92, 
    option = "D", direction = 1
  )
  
  colour_lighter <- adjust_colour_lighter(
    colours, og_val = 0.6 # lighter to make labels more readable
  )
  names(colour_lighter) <- topics # name colours
  names(colours) <- topics
  
  return(list(colour_lighter, colours))
}

umapClusterLookup <- function(df){
  
  cluster_lookup <- df %>%
    dplyr::group_by(kmeans_topic_title) %>%
    dplyr::summarise(
      label = dplyr::first(kmeans_topic_title),
      centroid_x = mean(V1),
      centroid_y = mean(V2)
    )
  return(cluster_lookup)
}


umapCreateHoverText <- function(df, colours){
  hover_text <- paste0(
    "<span style='display: inline-block; background-color: grey; padding: 10px; border-radius: 10px;width: 200px; text-align: center;'>",
    "<i>", "\"", df$text_with_breaks, "\"", "</i> - @", df$sender_screen_name, "<br><br>",
    "<b><span style='color:", 
    # colour_darker[kmeans_topic_title],
    colours[df$kmeans_topic_title],
    ";'>", df$kmeans_topic_title, "</span></b>",
    "</span>")
  
  return(hover_text)
}

createUmap <- function(df){
  df %>%
    plotly::plot_ly(x = ~V1, y = ~V2,
                    width = 900, height = 1100,
                    type = "scattergl",
                    mode = "markers")
}

#' UMAP Ui Server Function
#'
#' @param id parameter for shiny identification
#' @param df reactive dataframe containing docs and embedding info 
#' @param cluster_var reactive list of groups vorresponding to docs in df that is the colour var in the umap
#'
#' @noRd
#' 
createUmapsacve <- function(df, highlight_df = NULL, grey_df = NULL){
  
  colour_list <- umapColourCreate(df) # create umap colours
  colour_lighter <- colour_list[[1]]
  colours <- colour_list[[2]]
  print(colours)
  cluster_lookup <- umapClusterLookup(df) # create cluster label lookup
  
  if(is.null(highlight_df)){
    plot_df <- df 
    size = 4
  } else{
    plot_df <- highlight_df
    size = 10
  }
  
  plot_df <- plot_df %>%
    dplyr::mutate(hover_text = umapCreateHoverText(plot_df, colours)
                  # paste0(
                  #   "<span style='display: inline-block; background-color: grey; padding: 10px; border-radius: 10px;width: 200px; text-align: center;'>",
                  #   "<i>", "\"", text_with_breaks, "\"", "</i> - @", sender_screen_name, "<br><br>",
                  #   "<b><span style='color:", 
                  #   # colour_darker[kmeans_topic_title],
                  #   colours[kmeans_topic_title],
                  #   ";'>", kmeans_topic_title, "</span></b>",
                  #   "</span>")
    )
  
  print(plot_df$hover_text[1])
  
  
  p <- plotly::plot_ly(
    width = 900, height = 1100,
    source = "umap_plot"
  ) %>% 
    plotly::add_trace(data = plot_df,
                      x = ~V1, y = ~V2,
                      key = ~universal_message_id,
                      type = "scattergl",
                      mode = "markers",
                      text = ~hover_text,
                      hoverinfo = "text",
                      hoverlabel = list(bgcolor = 'rgba(255,255,255,0.75)',
                                        font = list(family = "Cinzel-Regular")
                      ),
                      showlegend = TRUE,
                      marker = list(opacity = 0.6, size = size, color = ~colour_lighter[kmeans_topic_title])
    )
  
  if (!is.null(grey_df)){
    p <- p %>%
      plotly::add_trace(data = grey_points,
                        x = ~V1, y = ~V2,
                        name = 'No search similarity',
                        type = "scattergl",
                        mode = "markers",
                        key = ~universal_message_id,
                        showlegend = TRUE,
                        marker = list(opacity = 0.5, size = 4, color = "#cccccc"),
                        hoverinfo = "skip")
    
  }
  
  
  p <- p %>%
    plotly::layout(dragmode = "lasso",
                   xaxis = list(showgrid = FALSE, showline = FALSE, zeroline = FALSE, showticklabels = FALSE, visile = FALSE, title = ""),
                   yaxis = list(showgrid = FALSE, showline = FALSE, zeroline = FALSE,  showticklabels = FALSE, visile = FALSE, title = ""),
                   legend = list(itemsizing = "constant",  orientation = "h",  xanchor = "center", x = 0.5,
                                 font = list(family = "Cinzel-Regular", size = 12)
                   )
    ) %>%
    plotly::config(
      scrollZoom = TRUE,
      displaylogo = FALSE,
      edits = list(shapePosition = TRUE, annotation = TRUE)
    ) %>% 
    plotly::add_annotations( # white shadow on cluster labels
      x = cluster_lookup$centroid_x,  # slight offset for the shadow
      y = cluster_lookup$centroid_y,  # slight offset for the shadow
      text = cluster_lookup$label,
      showarrow = FALSE,
      opacity = 1,
      xshift = 1, yshift = -1, # Adjust shadow position
      font = list(size = 18, family = "Cinzel", color = "white")
    )
  
  # cluster labelling: front of shadow ----
  for (i in 1:nrow(cluster_lookup)) {
    
    p <- p %>% plotly::add_annotations(
      x = cluster_lookup$centroid_x[i],
      y = cluster_lookup$centroid_y[i],
      text = cluster_lookup$label[i],
      showarrow = FALSE,
      font = list(size = 18, family = "Cinzel", color = "#000080")
    )
  }
  # ----
  
  return(p)
  
  
}

createUmapSaveLegaacy <- function(df, grey_df = NULL){
  
  # colour functions ----
  topics <- unique(r$df()$kmeans_topic_title)
  colours <- viridis::viridis(n = length(topics), begin = 0, end = 0.92, option = "D", direction = 1)
  colour_lighter <- adjust_colour_lighter(colours, og_val = 0.6)
  names(colour_lighter) <- unique(topics)
  colour_darker <- adjust_colour_darker(colours, og_val = 1)
  names(colour_darker) <- unique(topics)
  # ----
  
  # cluster labelling and colouring ----
  
  cluster_lookup <- r$df() %>%
    dplyr::group_by(kmeans_topic_title) %>%
    dplyr::summarise(
      # topic_number = topic,
      label = dplyr::first(kmeans_topic_title),
      centroid_x = mean(V1),
      centroid_y = mean(V2)) 
  
  # ----
  
  # plot ----
  
  if(is.null(r$highlight_df)){
    
    p <- r$df() %>%
      dplyr::mutate(
        hover_text = 
          paste0(
            "<span style='display: inline-block; background-color: grey; padding: 10px; border-radius: 10px;width: 200px; text-align: center;'>",
            "<i>", "\"", text_with_breaks, "\"", "</i> - @", sender_screen_name, "<br><br>",
            "<b><span style='color:", 
            colour_darker[kmeans_topic_title],
            ";'>", kmeans_topic_title, "</span></b>",
            "</span>")
      ) %>%
      plotly::plot_ly(x = ~V1,
                      y = ~V2,
                      width = 900, height = 700,
                      color = ~kmeans_topic_title,
                      colors = colour_lighter,
                      key = ~universal_message_id,
                      customdata = ~sender_screen_name,
                      type = "scattergl",
                      mode = "markers",
                      text = ~hover_text,
                      hoverinfo = "text",
                      hoverlabel = list(
                        bgcolor = 'rgba(255,255,255,0.75)',
                        font = list(
                          family = "Cinzel-Regular"
                        )
                      ),
                      showlegend = TRUE,
                      marker = list(opacity = 0.6, size = 4),
                      source = "umap_plot"
      )
  } else {
    grey_points <- r$grey_df()
    
    p <- plotly::plot_ly(width = 900, height = 700,
                         colors = colour_lighter,
                         source = "umap_plot"
    )
    
    if(nrow(r$highlight_df()) != 0){
      
      highlight_points <- r$highlight_df() %>%
        dplyr::mutate(
          hover_text = 
            paste0(
              "<span style='display: inline-block; background-color: grey; padding: 10px; border-radius: 10px;width: 200px; text-align: center;'>",
              "<i>", "\"", text_with_breaks, "\"", "</i> - @", sender_screen_name, "<br><br>",
              "<b><span style='color:", colour_darker[kmeans_topic_title],
              ";'>", kmeans_topic_title, "</span></b>",
              "</span>"))
      
      print(head(highlight_points %>% dplyr::select(hover_text)))
      
      p <-  p %>%
        plotly::add_trace(data = highlight_points,
                          x = ~V1, y = ~V2,
                          type = "scattergl",
                          mode = "markers",
                          key = ~universal_message_id,
                          color = ~kmeans_topic_title,
                          showlegend = TRUE,
                          marker = list(opacity = 0.6, size = 10),
                          hoverinfo ="text",
                          # text = ~hover_text,
                          text = ~text_with_breaks,
                          hoverinfo = "text",
                          hoverlabel = list(
                            bgcolor = 'rgba(255,255,255,0.75)',
                            font = list(
                              family = "Cinzel-Regular"
                            )
                          )
        )
      
      
    } 
    
    p <- p %>%
      plotly::add_trace(data = grey_points,
                        x = ~V1, y = ~V2,
                        type = "scattergl",
                        mode = "markers",
                        key = ~universal_message_id,
                        # showlegend = FALSE,
                        showlegend = TRUE,
                        marker = list(opacity = 0.5, size = 4, color = "#cccccc"),
                        hoverinfo = "skip")
    
  }
  
  p <- p %>%
    plotly::layout(dragmode = "lasso",
                   xaxis = list(showgrid = FALSE, showline = FALSE, zeroline = FALSE, showticklabels = FALSE, visile = FALSE, title = ""),
                   yaxis = list(showgrid = FALSE, showline = FALSE, zeroline = FALSE,  showticklabels = FALSE, visile = FALSE, title = ""),
                   legend = list(itemsizing = "constant",  orientation = "h",  xanchor = "center", x = 0.5,
                                 font = list(family = "Cinzel-Regular", size = 12)
                   )
    ) %>%
    plotly::config(
      scrollZoom = TRUE,
      displaylogo = FALSE,
      edits = list(shapePosition = TRUE, annotation = TRUE)
    )
  
  
  # cluster labelling ----
  for (i in 1:nrow(cluster_lookup)) {
    
    formatted_label <- sprintf("<b>%s</b>", cluster_lookup$label[i])
    
    # Adding the shadow
    p <- p %>% plotly::add_annotations(
      x = cluster_lookup$centroid_x[i] ,  # slight offset for the shadow
      y = cluster_lookup$centroid_y[i] ,  # slight offset for the shadow
      text = cluster_lookup$label[i],
      showarrow = FALSE,
      opacity = 1,
      xshift = 2, yshift = -2, # Adjust shadow position
      font = list(size = 22, family = "Cinzel", color = "white")
    )
    
    # Adding the main annotation
    p <- p %>% plotly::add_annotations(
      x = cluster_lookup$centroid_x[i],
      y = cluster_lookup$centroid_y[i],
      text = cluster_lookup$label[i],
      showarrow = FALSE,
      font = list(size = 22, family = "Cinzel", color = colour_darker[cluster_lookup$label[i]])
    )
  }
  # ----
  
  return(p)
  
  
}
