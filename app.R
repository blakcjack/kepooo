v <- "0.2.8"
vdt <- "2020-01-19"
nmax <- 1000
header_title <- "Kepooo"

# devtools::install_github("nurandi/katadasaR") #(>= 0.1)
# devtools::install_github("rstudio/leaflet") #(>= 2.0.3.9000) #dev version
# devtools::install_github("lchiffon/wordcloud2") # (>= 0.2.2)


library(textclean)
library(katadasaR)
library(tokenizers)
library(wordcloud2)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(rtweet)
library(shiny)
library(shinyalert)
library(DT)
library(bs4Dash)
library(leaflet)
library(httr)
library(htmltools)
library(shinyWidgets)

stemming <- function(x){
  paste(lapply(x, katadasar), collapse = " ")
}

faq <- function(q, a){
  sprintf("<strong>%s</strong><br/>%s<br/><br/>", q, a)
}

faqtext <- 
  p(HTML(faq(q = "How many tweets collected?",
                 a = sprintf("Current version will collect about %s (or less) tweets each time query", nmax))
         ),
    HTML(faq(q = "I found an issue/error, where do I report it?", a = "https://github.com/aephidayatuloh/kepooo/issues"))
    )

ui <- bs4DashPage(navbar = bs4DashNavbar(skin = "light", 
                                         status = "white", 
                                         border = FALSE, 
                                         sidebarIcon = "bar", 
                                         compact = FALSE, 
                                         leftUi = fluidRow(img(src = "img/logo_animation.png", width = "8%"), 
                                                           h3(header_title, style = "color:#000;margin-top: 10px;font-weight:bold;margin-left: 20px;")),
                                         rightUi = actionBttn(inputId = "help", label = NULL, style = "pill", icon = icon("question-circle"))
), 
sidebar = bs4DashSidebar(inputId = "sider", disable = TRUE), 
body = bs4DashBody(
  useShinyalert(),
  br(),
  fluidRow(
    column(4, 
           bs4Card(width = 12, title = NULL, headerBorder = FALSE, maximizable = FALSE, closable = FALSE, collapsible = FALSE,
                   fluidRow(
                     textInput("userid", NULL, placeholder = "username or @username", width = "63%"),
                     actionButton("stalking", "Kepoooin", class = "btn btn-info", icon = icon("twitter"), width = "35%",
                                  style = "height: 38px;background:#fff;color:#4dc3eb;font-weight:bold;")
                   )
           ),
           uiOutput("user"),
           uiOutput("latest_tweet_ui")
    ),
    column(8,
           uiOutput("source_tweets_ui"),
           uiOutput("nchar_tweets_ui")
    ),
    column(4,
           uiOutput("hashtags_ui")
    ),
    column(8, 
           uiOutput("map_tweets_ui")
    ),
    column(12,
           uiOutput("most_mentioned_ui"),
           uiOutput("tweets_ui")
    )
    # bs4Card(width = 12, height = 600, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
    #         title = "Most Reply",
    #         wordcloud2Output("most_replied", height = 550)
    #         )
    
  )
), 
controlbar = NULL, 
footer = bs4DashFooter(copyrights = HTML("&copy; 2020 @aephidayatuloh"), right_text = sprintf("version %s (%s)", v, vdt)), 
title = sprintf("%s - v%s", header_title, v), 
old_school = FALSE, 
sidebar_mini = FALSE, 
sidebar_collapsed = TRUE, 
controlbar_collapsed = TRUE, 
controlbar_overlay = NULL, 
enable_preloader = FALSE, 
loading_duration = 2, 
loading_background = "blue")

server <- function(input, output, session){
  tk <- readRDS("data/tkkepooo.rds")
  myStopwords <- readLines("data/stopword_list_id_2.txt")
  
  shinyalert(title = "Welcome, Kepooo!", 
             text = sprintf("Kepooo v%s (%s)<br/><br/>Just give me a username and I'll handle for you", v, vdt), 
             imageUrl = "img/logo_animation.gif", imageWidth = 400, imageHeight = 150, confirmButtonText = "Get Started", 
             html = TRUE)
  
  tweets <- eventReactive(input$stalking, {
    if(input$userid == ""){
      shinyalert(title = "Ooops!", 
                 text = "Please provide the username.", 
                 type = "error",
                 html = TRUE)
      return(NULL)
    } else {
      # isolate(input$userid)
      shinyalert(title = "Please Wait...", 
                 text = paste0("Your request will be done in minutes."), 
                 imageUrl = "img/loader.gif", imageWidth = 200, imageHeight = 200,
                 closeOnEsc = FALSE, closeOnClickOutside = FALSE, showConfirmButton = FALSE,
                 # type = "info",
                 html = TRUE)
      
      # req(input$userid)
      userid <- trimws(gsub("@", "", input$userid), "both")
      message(sprintf("Username : @%s at %s", userid, Sys.time()))
      start_time <- Sys.time()
      tweets <- rtweet::get_timelines(userid, n = nmax, token = tk)
      ntweets <- nrow(tweets)
      elapsed_time <- Sys.time() - start_time
      if(ntweets == 0){
        shinyalert(title = "Sorry, that page does not exist.", 
                   text = sprintf("Is @%s really a twitter username?
                                  <br/>
                                  <br/>
                                  <ul style='margin-left:40px;text-align:left;list-style-type:disc;'>
                                  <li>Check the spelling</li>
                                  <li>Username has been changed or deleted</li>
                                  <li>Account is private</li>
                                  </ul>
                                  You can check on https://twitter.com/%s.", userid, userid), 
                   type = "error", html = TRUE
        )
        return(NULL)
      } else {
        
        shinyalert(title = "Done!", text = sprintf("Elapsed time: %s %s", round(elapsed_time, 2), attributes(elapsed_time)$units), timer = 3*1000,
                   type = "success",
                   html = TRUE)
        return(tweets)
      }
      # read_twitter_csv("data/andriani_19.csv")
    }
  })
  
  observeEvent(input$stalking, {
    output$user <- renderUI({
      isolate(input$userid)
      if(is.null(tweets())){
        return(NULL)
      } else {
        bs4UserCard(width = 12, type = 2, imageElevation = 4,
                    src = unique(tweets()$profile_image_url),
                    status = "info",
                    title = unique(tweets()$name),
                    subtitle = paste0('@', unique(tweets()$screen_name)),
                    elevation = 4,
                    h6(sprintf("Details (Based on the last about %s statuses)", nmax)),
                    
                    bs4Table(
                      cardWrap = FALSE,
                      bordered = FALSE,
                      striped = TRUE,
                      headTitles = c("", ""),
                      bs4TableItems(
                        bs4TableItem("Account Location"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$location)))
                      ),
                      bs4TableItems(
                        bs4TableItem("Country"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$country)[complete.cases(unique(tweets()$country))], collapse = "-"))
                      ),
                      bs4TableItems(
                        bs4TableItem("Account Created At"),
                        bs4TableItem(dataCell = TRUE, paste(unique(tweets()$account_created_at), "UTC"))
                      ),
                      bs4TableItems(
                        bs4TableItem("Followers"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$followers_count)))
                      ),
                      bs4TableItems(
                        bs4TableItem("Friends"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$friends_count)))
                      ),
                      bs4TableItems(
                        bs4TableItem("Statuses"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$statuses_count)))
                      ),
                      bs4TableItems(
                        bs4TableItem("Retweets"),
                        bs4TableItem(dataCell = TRUE, paste0(sum(tweets()$is_retweet)))
                      ),
                      bs4TableItems(
                        bs4TableItem("Favourites"),
                        bs4TableItem(dataCell = TRUE, paste0(unique(tweets()$favourites_count)))
                      )
                    )
        )
      }
    })
    
  })
  
  observeEvent(input$stalking, {
    output$latest_tweet_ui <- renderUI({
      isolate(input$userid)
      if(!is.null(tweets())){
        bs4SocialCard(width = 12, solidHeader = TRUE, collapsible = TRUE, closable = FALSE, 
                      title = "Latest Tweet", src = unique(tweets()$profile_image_url),
                      subtitle = tweets()$created_at[1],
                      tweets()$text[1]
        )
      } else {
        return(NULL)
      }
    })
  })
  
  observeEvent(input$stalking, {
    if(is.null(tweets())){
      return(NULL)
    } else if(nrow(tweets()) == 0){
      return(NULL)
    } else {
      output$source_tweets_ui <- renderUI({
        isolate(input$userid)
        output$source_tweets <- renderDataTable({
          tweets() %>%
            mutate(source = str_replace_all(source, "  ", " ")) %>%
            count(source) %>%
            arrange(desc(n)) %>%
            mutate(pct = round(n/sum(n)*100, 2)) %>% 
            rename(Source = source, `Number of Tweets` = n, Percentage = pct) %>% 
            datatable(options = list(pageLength = 5), rownames = FALSE)
        })
        
        bs4Card(width = 12, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                title = paste("Tweet Sources", sprintf(" (Based on the last about %s statuses)", nmax)),
                dataTableOutput("source_tweets")
        )
        
      })
    }
  })
  
  observeEvent(input$stalking, {
    output$most_mentioned_ui <- renderUI({
      if(!is.null(tweets())){
        output$most_mentioned <- renderWordcloud2({
          isolate(input$userid)
          tweets() %>%
            filter(!is.na(mentions_screen_name)) %>%
            unnest(cols = mentions_screen_name) %>%
            count(mentions_screen_name) %>%
            wordcloud2(size = 1)
        })
        bs4Card(width = 12, height = 600, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                title = "Most Mentions",
                wordcloud2Output("most_mentioned", height = 550)
        )
      } else {
        return(NULL)
      }
    })
  })
  # 
  # output$most_replied <- renderWordcloud2({
  #   req(input$userid)
  #   message("most_replied active")
  #   tweets() %>%
  #         count(reply_to_screen_name) %>%
  #         filter(!is.na(reply_to_screen_name))%>%
  #         wordcloud2(size = 1)
  # })
  # 
  observeEvent(input$stalking, {
    output$nchar_tweets_ui <- renderUI({
      if(!is.null(tweets())){
        output$nchar_tweets <- renderPlot({
          isolate(input$userid)
          tweets() %>%
            filter(!is_retweet) %>%
            select(display_text_width) %>%
            ggplot(aes(x = display_text_width)) +
            geom_histogram(fill = "lightblue", color = "white") +
            scale_x_continuous(breaks = seq(0, 300, by = 70)) +
            labs(x = "Number of character in a tweet") +
            theme_light()
        })
        bs4Card(width = 12, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                title = "Distribution of Number of Character in Tweets",
                plotOutput("nchar_tweets")
        )
      } else {
        return(NULL)
      }
    })
  })
  
  
  observeEvent(input$stalking, {
    output$map_tweets_ui <- renderUI({
      if(!is.null(tweets())){
        output$map_tweets <- renderLeaflet({
          isolate(input$userid)
          place <- tweets() %>%
            filter(!is.na(place_full_name)) %>%
            select(text, created_at, place_full_name, source, bbox_coords) %>%
            unnest(bbox_coords) %>%
            group_by(text, created_at, place_full_name, source) %>%
            summarise(long = max(bbox_coords, na.rm = TRUE),
                      lat = min(bbox_coords, na.rm = TRUE)) %>% 
            mutate(popuptxt = sprintf("<strong style='color:#4dc3eb;'>%s on %s</strong><br/>%s<br/><br/><strong>Source: %s</strong>", place_full_name, created_at, text, source))
          
          if(nrow(place) > 1){
            leaflet(place) %>%
              addTiles() %>%
              addAwesomeMarkers(lng = ~long, lat = ~lat, popup = ~popuptxt)
          } else {
            leaflet() %>% 
              addTiles()
          }
        })
        bs4Card(width = 12, height = 550, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                title = "Tweets Location",
                leafletOutput("map_tweets", height = 500),
                helpText("Disclaimer: marker position may be not accurate due to location data from twitter.")
        )
      } else {
        return(NULL)
      }
    })
  })
  
  observeEvent(input$stalking, {
    output$hashtags_ui <- renderUI({
      if(!is.null(tweets())){
        output$hashtags <- renderDataTable({
          isolate(input$userid)
          tweets() %>%
            filter(!is.na(hashtags)) %>%
            unnest(cols = c(hashtags)) %>%
            count(hashtags) %>%
            arrange(desc(n)) %>%
            rename(Hashtags = hashtags, `Number of Tweets` = n) %>%
            head(10) %>% 
            datatable(options = list(dom = 't', PageLength = 5), rownames = FALSE)
        })
        bs4Card(width = 12, height = 550, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                title = "The Most 10 #Hashtags",
                dataTableOutput("hashtags")
        )
      } else {
        return(NULL)
      }
    })
  })
  
  observeEvent(input$stalking, {
    if(is.null(tweets())){
      return(NULL)
    } else if(nrow(tweets()) == 0){
      return(NULL)
    } else {
        output$tweets_ui <- renderUI({
        isolate(input$userid)
        bs4Card(width = 12, height = 600, solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                title = fluidRow(h5("Most Tweets", style="margin-top:5px;margin-right:20px;"), 
                                 actionButton("wctweetsbtn", "Extract Tweets", class = "btn btn-info", icon = icon("twitter"), 
                                              style = "height: 38px;background:#fff;color:#4dc3eb;font-weight:bold;")),
                wordcloud2Output("wctweets", height = 550)
        )
      })
    }
  })
  observeEvent(input$wctweetsbtn, {
    isolate(input$userid)
    if(is.null(tweets())){
      shinyalert(type = "error", title = "Ooops!", 
                 text = "Seems like you has not clicked the 'Stalking' button yet.")
    } else if(nrow(tweets()) > 0){
      shinyalert(title = "Please Wait...", 
                 text = paste0("Your request will be done in minutes."), 
                 imageUrl = "img/loader.gif", imageWidth = 200, imageHeight = 200, 
                 closeOnEsc = FALSE, closeOnClickOutside = FALSE, showConfirmButton = FALSE,
                 # type = "info",
                 html = TRUE)
      
      output$wctweets <- renderWordcloud2({
        isolate(input$userid)
        tweet_content <- tweets()$text %>%
          replace_html() %>% # replace html with blank
          replace_url()  %>% # replace URLs with blank
          replace_emoji() %>%
          replace_tag(tweets, pattern = "@([A-Za-z0-9_]+)", replacement = "") %>%  # remove mentions
          replace_hash(tweets, pattern = "#([A-Za-z0-9_]+)", replacement = "") %>% # remove hashtags
          replace_number() %>%
          replace_emoticon() %>%
          gsub("[0-9]", "", .)
        
        tweet_content <- tweet_content %>%
          strip()
        
        tweet_content <- as.character(tweet_content)
        tweet_stem <- lapply(tokenize_words(tweet_content[]), stemming)
        
        tweet_token <- tokenize_words(tweet_stem)
        
        tweet_token <- as.character(tweet_token)
        tweet_token <- tokenize_words(tweet_token, stopwords = c(myStopwords, stop_words$word), strip_punct = TRUE, strip_numeric = TRUE)
        
        tweet <- do.call(c, tweet_token)
        tweet <- as.character(tweet)
        teetweet <- tibble(tweet = tweet) %>%
          count(tweet) %>%
          arrange(desc(n)) %>%
          filter(nchar(tweet) > 3 & !str_detect(tweet, "^lexicon"))
        shinyalert(type = "success", title = "Done!", timer = 3*1000)
        wordcloud2::wordcloud2(teetweet, size = 1, minSize = 5, shape = "circle")
      })
    } else {
      shinyalert(type = "error", title = "Ooops!", 
                 text = "May be this username '@%s is private or has no tweet.")
    }
  })
  
  observeEvent(input$help, {
    showModal(
      modalDialog(easyClose = FALSE, 
                  title = fluidRow(img(src="img/help.png", width = 25, height = 25), h5("FAQ", style="font-weight:bold;margin-left:10px;")), 
                  faqtext, 
                  footer = modalButton("Close"))
      )
  })
}

shinyApp(ui, server)
