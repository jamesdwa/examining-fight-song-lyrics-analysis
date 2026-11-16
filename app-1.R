library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

source("analysis.R") #loads in your analysis file

# Overview ---------------------------------------------------------------------

# Homework 4: Examining Fight Song Lyrics - Shiny App
# Before you begin, make sure you read and understand the assignment description
# on canvas first! This assignment will not make sense otherwise. 
# For each question/prompt, write the necessary code to create your shiny app. 
# Make sure you have written AND TESTED all of your code in analysis.R before 
# beginning this part of the assignment. 

songs_df <- read.csv("fight_songs.csv") #DO NOT CHANGE!

# Creating your UI -------------------------------------------------------------
#
# Part of this UI is created for you. You need to add elements to make your UI
# match the example provided in the instruction. 
#
#-------------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Examining Lyrics in College Fights Songs"),
  
  br(), # the br() function adss line breaks
  
  p("Let's examine the fight songs from 65 different universities across the US 
      (i.e. all those in the Power Five conferences (the ACC, Big Ten, Big 12, Pac-12 and SEC), plus Notre Dame). 
      Our analysis looks at how many times certain words appear in the lyrics to see how each song compares. 
      We also look at song length and speed of each song based on the official versions available on Spotify"),
  br(),
  
  p(paste("Overall out of the 65 songs we looked at,", fight_count," songs 
      contain the word 'fight' -- usually more than once. In fact, the word 'fight' 
      appears", total_fight, "in total throughout all the songs!", win_count, "songs contain 
      'win' or 'victory' and", rah_count, "contain the phrase 'rah'. Gendered terms 
      are also fairly common in college fight songs, but usually the songs only 
      reference men.", male_count, "songs contain references to men (i.e. the 
      lyrics contain words like 'men', 'boys', or 'sons') whereas 
      only", female_count, "contain references to women (i.e. the lyrics contain
      words like 'women', 'girls', or 'daughters').")),
  br(),
  
  #this code just adds some custom HTML styling 
  tags$style(HTML("
    h2 {
      background-color: #ccd5d8;
      color: Black;
    }")),
  
  htmlOutput(outputId = "songs"),
  br(),
  
  # --------------------------------------------------------------------------
  # Add code for a sidebar and maine panel here, Your sidebar should appear 
  # on the left and should contain the following elements:
  # 1) drop down menu that lets the user select a particular college from the 
  #    dataset. The inputId should be `school_name` and the prompt should be 
  #    "Choose a School"
  # 2) Three html outputs that we will use to contain the information about the
  #   lyrics for that particular school. Use the following 4 lines of code to 
  #   add these elements: 
  #htmlOutput(outputId = "song_name"),
  #htmlOutput(outputId = "composer"),
  #htmlOutput(outputId = "lyrics"),
  #br()
  #
  # The main panel should appear on the right and should contain the following
  # elelemnts:
  # 1) An h3 header with the text "Comparing Lyrics"
  # 2) a plotlyOutput with the outputID set as `scatter`
  #---------------------------------------------------------------------------
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "school_name",
        label = "Choose a School",
        choices = unique(songs_df$school)
      ),
      htmlOutput(outputId = "song_name"),
      htmlOutput(outputId = "composer"),
      htmlOutput(outputId = "lyrics"),
      br()
    ),
    
    mainPanel(
      h3("Comparing Lyrics"),
      plotlyOutput(outputId = "scatter")
    )
  ),
  
  titlePanel("Filter All Songs by Specific Words"),
  textInput(
    inputId = "word",
    label = "Enter text to search for song lyrics that contain that word"
  ),
  htmlOutput(outputId = "filtered_songs")
)

# Creating your Server ---------------------------------------------------------
#
# In this section you will add the logic to make your UI fully interactive. Make
# sure you name all your input and output elements correctly! 
#
#-------------------------------------------------------------------------------
server <- function(input, output) {
  
  output$song_name <- renderUI({
    school_name <- input$school_name
    song_name <- get_song_name(songs_df, school_name)
    HTML(paste(song_name))
  })
  
  output$composer <- renderUI({
    school_name <- input$school_name
    composer <- get_info(songs_df, school_name)
    HTML(paste(composer))
  })
  
  output$lyrics <- renderUI({
    school_name <- input$school_name
    lyrics <- get_lyrics(songs_df, school_name)
    HTML(paste(lyrics))
  })
  
  output$scatter <- renderPlotly({
    school_name <- input$school_name
    
    p <- ggplot(data = songs_df, aes(x = sec_duration, y = bpm, text = school)) +
      geom_point(aes(color = school), size = 2) +
      geom_text(data = filter(songs_df, school == school_name), aes(label = school), 
                hjust = 1.2, vjust = 0.5, size = 3, fontface = "bold") +
      labs(x = "sec_duration", y = "bpm")
    
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
  
  output$filtered_songs <- renderUI({
    word <- input$word
    filtered_df <- get_relevant_songs(songs_df, word)
    
    return(make_cols(filtered_df))
  })
}

# Run the application
shinyApp(ui = ui, server = server)