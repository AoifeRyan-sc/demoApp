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
  
  # topics <- unique(df$kmeans_topic_title) # number of colours
  topics <- unique(df$topic_title) # number of colours
  colours <- viridis::viridis( # colour shades
    n = length(topics),
    begin = 0.3, end = 0.92,
    option = "B", direction = 1
  )
  # colours <- hcl.colors(n = length(topics)
                        # palette = "Blue-Red 3"
                        # )
  
  
  names(colours) <- topics
  
  colour_lighter <- adjust_colour_lighter(colours, og_val = 0.7)
  colour_darker <- adjust_colour_darker(colours, og_val = 5)
  
  names(colour_lighter) <- topics # name colours
  names(colour_darker) <- topics
  names(colours) <- topics
  
  return(list(colour_lighter, colour_darker, colours))
}

umapClusterLookup <- function(df, colours){
  
  col_df <- data.frame(
    # kmeans_topic_title = names(colours),
    topic_title = names(colours),
    colour_map = unname(colours),
    row.names = NULL
  )
  
  cluster_lookup <- df %>%
    # dplyr::group_by(kmeans_topic_title) %>%
    dplyr::group_by(topic_title) %>%
    dplyr::summarise(
      # label = dplyr::first(kmeans_topic_title),
      label = dplyr::first(topic_title),
      centroid_x = mean(V1),
      centroid_y = mean(V2),
    ) %>%
    # dplyr::left_join(col_df, by = "kmeans_topic_title")
    dplyr::left_join(col_df, by = "topic_title")
  
  return(cluster_lookup)
}


umapCreateHoverText <- function(df, colours){
  
  col_df <- data.frame(
    # kmeans_topic_title = names(colours),
    topic_title = names(colours),
    colour_map = unname(colours),
    row.names = NULL
  )
  
  df <- df %>%
    # dplyr::left_join(col_df, by = "kmeans_topic_title") 
    dplyr::left_join(col_df, by = "topic_title") 
  
  hover_text <- paste0(
    "<span style='display: inline-block; background-color: grey; padding: 10px; border-radius: 10px;width: 200px; text-align: center;'>",
    "<i>", "\"", df$text_with_breaks, "\"", "</i> - @", df$sender_screen_name, "<br><br>",
    "<b><span style='color:", 
    df$colour_map,
    # ";'>", df$kmeans_topic_title, "</span></b>",
    ";'>", df$topic_title, "</span></b>",
    "</span>")
  
  return(hover_text)
}

createUmapLayout <- function(p){
  p <- p %>%
    plotly::layout(dragmode = "lasso",
                   showlegend = TRUE,
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
  
  return(p)
}

createClusterLabels <- function(p, cluster_lookup){
  
cluster_lookup$formatted_text <- sprintf("<b>%s</b>", cluster_lookup$label)
shadow_positions <- rbind(expand.grid(rep(list(c(1, -1)), 2)), 
                          c(1,0), c(-1,0), c(0,1), c(0,-1))
                          # expand.grid(rep(list(c(1, 0)), 2)))

for (i in 1:6){
  p <- p %>%
    plotly::add_annotations( # white shadow on cluster labels
      x = cluster_lookup$centroid_x,  # slight offset for the shadow
      y = cluster_lookup$centroid_y,  # slight offset for the shadow
      # text = cluster_lookup$label,
      text = cluster_lookup$formatted_text,
      showarrow = FALSE,
      opacity = 1,
      # xshift = 1, yshift = -1, # Adjust shadow position
      xshift = shadow_positions$Var1[i], yshift = shadow_positions$Var2[i],
      font = list(size = 16, family = "Cinzel", color = "white")
    )
}
  

  for (i in 1:nrow(cluster_lookup)) { # text infront of shadow
  
    p <- p %>% plotly::add_annotations(
      x = cluster_lookup$centroid_x[i],
      y = cluster_lookup$centroid_y[i],
      text = cluster_lookup$formatted_text[i],
      showarrow = FALSE,
      opacity = 1,
      font = list(size = 16, family = "Cinzel",
                  color = "#141414"
                  # color = cluster_lookup$colour_map[i]
      )
    )
  }
 
 return(p)
}

createUmap <- function(df, highlight_df = NULL, grey_df = NULL, cluster_type){
  
  if (cluster_type == "kmeans"){
    df <- df %>% dplyr::mutate(topic_title = kmeans_topic_title)
    if (!is.null(highlight_df)){
      highlight_df <- highlight_df %>% dplyr::mutate(topic_title = kmeans_topic_title)
    }
    
    grey_highlight <- NULL
  } else {
    topic_count <- df %>%
      dplyr::group_by(hdb_topic_title) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::arrange(desc(n)) %>%
      head(10)
    
    grey_highlight <- NULL
    
    if (!is.null(highlight_df)){
      # highlight_df <- highlight_df %>%
      #   dplyr::mutate(topic_title = hdb_topic_title,
      #                 topic_title = dplyr::case_when(
      #                   topic_title %in% topic_count$hdb_topic_title ~
      #                     topic_title,
      #                   TRUE ~ ""
      #                 ))
      
      grey_highlight <- highlight_df %>%
        dplyr::mutate(topic_title = hdb_topic_title) %>%
        dplyr::filter(!topic_title %in% topic_count$hdb_topic_title)
      
      highlight_df <- highlight_df %>%
        dplyr::mutate(topic_title = hdb_topic_title) %>%
        dplyr::filter(topic_title %in% topic_count$hdb_topic_title)
      
      # grey_df <- grey_df %>%
      #   dplyr::mutate(topic_title = hdb_topic_title) %>%
      #   dplyr::bind_rows(small_topics, .id = "id")
      grey_df <- grey_df %>% dplyr::mutate(topic_title = hdb_topic_title)
        

    }
    
    grey_df <- df %>%
      dplyr::mutate(topic_title = hdb_topic_title) %>%
      dplyr::filter(!topic_title %in% topic_count$hdb_topic_title)
    print(nrow(grey_df))
    
    df <- df %>% 
      dplyr::mutate(topic_title = hdb_topic_title) %>%
      dplyr::filter(topic_title %in% topic_count$hdb_topic_title)
    print(nrow(df))
      # dplyr::anti_join(grey)
      # dplyr::mutate(topic_title = hdb_topic_title,
      #               topic_title = dplyr::case_when(
      #                 topic_title %in% topic_count$hdb_topic_title ~
      #                   topic_title,
      #                 TRUE ~ ""
                    # )) 
  }
  
  
  colour_list <- umapColourCreate(df) # create umap colours
  colour_lighter <- colour_list[[1]]
  colour_darker <- colour_list[[2]]
  colours <- colour_list[[3]]
  
  cluster_lookup <- umapClusterLookup(df, 
                                      # colour_darker
                                      colour_lighter
                                      ) # create cluster label lookup
  
  if(is.null(highlight_df)){
    plot_df <- df 
    size = 4
  } else{
    plot_df <- highlight_df
    size = 10
  }
  
  plot_df <- plot_df %>%
    dplyr::mutate(hover_text = umapCreateHoverText(plot_df, colours))
  nrow(plot_df)
  p <- plotly::plot_ly(
    data = plot_df,
    width = 1000, height = 650,
    x = ~V1, y = ~V2,
    key = ~universal_message_id, 
    type = "scattergl",
    mode = "markers",
    text = ~hover_text,
    hoverinfo = "text",
    color = ~topic_title,
    colors = colours,
    hoverlabel = list(bgcolor = 'rgba(255,255,255,0.75)',
                      font = list(family = "Cinzel-Regular")
    ),
    
    marker = list(opacity = 0.6,
                  size = size
    ),
    source = "umap_plot",
    showlegend = TRUE
  ) 
  
  if (!is.null(grey_df)){
    grey_df <- grey_df %>% dplyr::mutate(hover_text = "",
                                         topic_title = "")
    nrow(grey_df)
    
    p <- p %>%
      plotly::add_trace(data = grey_df,
                        x = ~V1, y = ~V2,
                        name = 'No search similarity',
                        type = "scattergl",
                        mode = "markers",
                        key = ~universal_message_id,
                        showlegend = TRUE,
                        marker = list(
                          size = 4,
                          opacity = 0.3,
                          # opacity = 0.4, size = 4, 
                                      # color = "#ececec",
                          # color = "red",
                                      color = "#cccccc",
                                      showlegend = TRUE),
                        hoverinfo = "skip")
    
  }
  
  if (!is.null(grey_highlight)){
    
    grey_highlight <- grey_highlight %>%
      # dplyr::mutate(hover_text = umapCreateHoverText(grey_highlight, colours))
      dplyr::mutate(hover_text = text_clean,
                    topic_title = "")
    
    print(nrow(grey_highlight))
    
    p <- p %>%
      plotly::add_trace(data = grey_highlight,
                        x = ~V1, y = ~V2,
                        name = 'idk what to call this',
                        type = "scattergl",
                        mode = "markers",
                        key = ~universal_message_id,
                        text = ~hover_text,
                        showlegend = TRUE,
                        marker = list(opacity = 0.6, size = 10, 
                                      # color = "#cccccc", "#bfbfbf", "#b5b5b5", "#bababa"
                                      # color = "#b5b5b5",
                                      color = "#FFFDD0",
                                      showlegend = TRUE),
                        hoverinfo = "text",
                        hoverlabel = list(bgcolor = 'rgba(255,255,255,0.75)',
                                          font = list(family = "Cinzel-Regular")
                        )
                        # visible = "legendonly"
                        )
  }
  
  p <- createUmapLayout(p)
   
  
  p <- createClusterLabels(p = p, cluster_lookup = cluster_lookup)

  
  return(p)
  
  
}
