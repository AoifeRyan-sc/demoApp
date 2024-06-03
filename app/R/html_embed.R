# sets the max height and makes element scrollable
style_embedding <- function(height = 400) {
  start_div <- paste0("<div style='height: ", height, "px; overflow-y: auto;'>")
  end_div <- "</div>"
  
  return(list(start_div = start_div, end_div = end_div))
}

# maybe too brittle
extract_source <- function(permalink) stringr::str_extract(permalink, "(\\w+)\\.com", group = 1)

# Twitter ----
extract_tweet_id <- function(permalink) stringr::str_extract(permalink, "status/(\\d+)", group = 1)

# extract_tweet_screen_name <- function(permalink) stringr::str_extract(permalink, "twitter.com/(\\w+)", group = 1)
extract_tweet_screen_name <- function(permalink) stringr::str_extract(permalink, "x.com/(\\w+)", group = 1)

# Yoinked from Gadenbuie's Tweet Conf package
get_tweet_blockquote <- function(screen_name, status_id, ..., null_on_error = TRUE, theme = "light") {
  oembed <- list(...)$oembed
  if (!is.null(oembed) && !is.na(oembed)) return(unlist(oembed))
  # oembed_url <- glue::glue("https://publish.twitter.com/oembed?url=https://twitter.com/{screen_name}/status/{status_id}&omit_script=1&dnt=1&theme={theme}")
  oembed_url <- glue::glue("https://publish.twitter.com/oembed?url=https://x.com/{screen_name}/status/{status_id}&omit_script=1&dnt=1&theme={theme}")
  bq <- purrr::possibly(httr::GET, list(status_code = 999))(URLencode(oembed_url))
  if (bq$status_code >= 400) {
    if (null_on_error) return(NULL)
    '<blockquote style="font-size: 90%">Sorry, unable to get tweet ¯\\_(ツ)_/¯</blockquote>'
  } else {
    httr::content(bq, "parsed")$html
  }
}


create_tweet_embed <- function(permalink) {
  tweet_id <- extract_tweet_id(permalink)
  tweet_screen_name <- extract_tweet_screen_name(permalink)
  embedding_style <- style_embedding(height = 500)
  
  tweet_blockquote <- get_tweet_blockquote(screen_name = tweet_screen_name, status_id = tweet_id)
  
  # x_logo <-  shiny::tags$img(src ="x_logo.png", align = "left",
  #                            width = "36px", height = "30px",
  #                            style = "margin-left: 20px; margin-right: 10px")

  x_logo <-  shiny::tags$img(src ="x_logo.png", align = "right",
                             width = "36px", height = "30px",
                             style = "margin-right: 20px; margin-botom: 50px")
 
  if (!is.null(tweet_blockquote)) {
    html <- paste0(
      embedding_style$start_div,
      tweet_blockquote, 
      x_logo,
      embedding_style$end_div)
    
    return(shiny::HTML(html))
  } else {
    
    return(NULL)
  }
  
  
  
}


# Reddit ---- 
extract_subreddit <- function(permalink) stringr::str_extract(permalink, "/r/(\\w+)", group = 1)

extract_reddit_html <- function(url, subreddit) {
  
  embedding_style <- style_embedding(height = 500)
  html <- paste0(embedding_style$start_div, '<blockquote class="reddit-embed-bq" style="height:400px" data-embed-height="597"><a href="',
                 url, '">Link to post</a><br></a> in<a href="https://www.reddit.com/r/', subreddit, '">',
                 subreddit, '</a></blockquote><script async="" src="https://embed.reddit.com/widgets.js" charset="UTF-8"></script>',
                 embedding_style$end_div)
  
  return(shiny::HTML(html))
}

create_reddit_embed <- function(permalink){
  if(!grepl("reddit", permalink)) stop("Not a reddit link")
  
  subreddit <- extract_subreddit(permalink)
  
  reddit_html <- extract_reddit_html(permalink, subreddit)
  
  return(reddit_html)
}

# Instagram ----
create_instagram_embed <- function(permalink) {
    
    embedding_style <- style_embedding(height = 500)
    
    html <- paste0(embedding_style$start_div,'<blockquote class="instagram-media" data-instgrm-permalink="',
                   permalink, '" data-instgrm-version="12" style=" background:#FFF; border:0; border-radius:3px; box-shadow:0 0 1px 0 rgba(0,0,0,0.5),0 1px 10px 0 rgba(0,0,0,0.1); margin: 1px; max-width:540px; min-width:326px; padding:0; width:99.375%; width:-webkit-calc(100% - 2px); width:calc(100% - 2px);">
      <div style="padding:16px;">
      </div>
    </blockquote>
    <script async defer src="//www.instagram.com/embed.js"></script>', embedding_style$end_div)
    
    return(shiny::HTML(html))
}

# Switch ----
embed_switch <- function(permalink) {
  source <- extract_source(permalink)
  
  embed <- switch(
    source,
    "reddit" = create_reddit_embed(permalink),
    "instagram" = create_instagram_embed(permalink),
    "x" = create_tweet_embed(permalink)
  )
  
  return(embed )
}

create_list_embed <- function(permalinks) {
  
  if(!inherits(permalink, "list") & !is.vector(permalinks)) message("Input should be a list or vector")
  
  list_embeds <- lapply(permalinks, function(x) {
    embed_switch(x)
  })
  
  return(list_embeds)
  
}


reverse_link_click_html <- function(permalinks) {
  
  permalinks <- stringr::str_remove_all(permalinks, "<a href='")
  permalinks <- stringr::str_remove_all(permalinks, "' target=.*")
  
  return(permalinks)
}
