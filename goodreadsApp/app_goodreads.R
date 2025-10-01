#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shiny)
library(plotly)
library(tm)
library(wordcloud)
library(tidytext)
library(SnowballC)
library(wordcloud2)


# Load Goodreads dataset
#goodreads_cleaned <- read.csv("/Users/anumdamani/stats422/web_app_project/stats422_web_app_Anum_Damani/goodreads_cleaned2.csv")
goodreads_cleaned <- read.csv("data/goodreads_cleaned2.csv")

# Define UI
ui <- fluidPage(
  navbarPage(
    title = "Top Bestselling Books on Goodreads",
    tabPanel("Welcome",
             h1("Welcome to the Goodreads Insights Web App!"),
             p("This web app provides insights into the Goodreads dataset located here: https://www.kaggle.com/datasets/cristaliss/ultimate-book-collection-top-100-books-up-to-2023"),
             p("Please use the navigation tabs above to explore different insights."),
             p("Check it out!")
    ),
    tabPanel("Publisher Insights",
             sidebarLayout(
               sidebarPanel(
                 selectInput("publisher_select", "Select Publisher:", choices = c(sort(unique(goodreads_cleaned$publisher)))),
                 selectInput("plot_select", "Select Plot:", choices = c("Top Bestselling Authors", "Average Rating per Year", "Top Rated Genres"))
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.plot_select == 'Top Bestselling Authors'",
                   plotOutput("publisher_plot")
                 ),
                 conditionalPanel(
                   condition = "input.plot_select == 'Average Rating per Year'",
                   plotOutput("average_rating_year_plot")
                 ),
                 conditionalPanel(
                   condition = "input.plot_select == 'Top Rated Genres'",
                   plotOutput("top_rated_genres_plot")
                 )
               )
             )),
    tabPanel("Author Insights",
             sidebarLayout(
               sidebarPanel(
                 selectInput("author_select", "Select Author:", choices = c("All", sort(unique(goodreads_cleaned$authors)))),
                 selectInput("display_select", "Select Output:", choices = c("Author Performance Table", "Word Cloud"))
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.display_select == 'Author Performance Table'",
                   tags$b("Selected Author's Goodreads Performance (Averaged Across Their Books):"),
                   dataTableOutput("author_performance_table")
                 ),
                 conditionalPanel(
                   condition = "input.display_select == 'Word Cloud'",
                   tags$b("Wordcloud of Selected Author's Book Descriptions:"),
                   wordcloud2Output("author_wordcloud")
                 )
               )
             )
    ),
    tabPanel("Genre Insights",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plot_type", "Select Plot Type:", 
                             choices = c("Histogram of Number of Pages by Genre", 
                                         "Scatterplot of Number of Pages versus Want to Read",
                                         "Line Plot of Average Rating Per Year by Genre", "Word Cloud of Genre-Specific Keywords", 
                                         "Box Plot of Average Rating by Genre")),
                 selectInput("genre_select_books", "If Histogram, Scatterplot, Line Plot, or Word Cloud Selected, Please Select a Genre:", 
                             choices = c("Children/Young Adult", "Fantasy", "Mystery/Thriller", "Nonfiction", "Other", "Romance", "Science Fiction")),
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.plot_type == 'Histogram of Number of Pages by Genre'",
                   plotOutput("book_length_histogram")
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'Scatterplot of Number of Pages versus Want to Read'",
                   plotOutput("scatterplot")
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'Line Plot of Average Rating Per Year by Genre'",
                   plotOutput("average_rating_line_plot")
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'Word Cloud of Genre-Specific Keywords'",
                   tags$b("Wordcloud of Genre-Specific Keywords:"),
                   wordcloud2Output("genre_wordcloud")
                 ),
                 conditionalPanel(
                   condition = "input.plot_type == 'Box Plot of Average Rating by Genre'",
                   plotOutput("rating_boxplot")
                 )
             )
    )),
    tabPanel("Data Table",
             downloadButton("download_data", "Download Data"),
             dataTableOutput("data_table"))
  )
)

# Define server logic
server <- function(input, output) {

  # Downloadable CSV of selected dataset
  output$download_data <- downloadHandler(
    filename = function() {
      "goodreads_cleaned.csv"
    },
    content = function(file) {
      write.csv(goodreads_cleaned, file)
    }
  )
  
 # PUBLISHER TAB
  # Function to filter data based on selected publisher and calculate average rating score for each author
  filtered_data <- reactive({
    filtered <- goodreads_cleaned %>%
      filter(publisher == input$publisher_select) %>%
      group_by(authors) %>%
      summarise(average_rating = mean(rating_score, na.rm = TRUE)) %>%  # Calculate total number of ratings for each author
      arrange(desc(average_rating)) %>%
      top_n(5)  # Select only the top 10 authors based on average rating score
    return(filtered)
  })
  # Plot top 10 bestselling authors by average rating score for selected publisher
  output$publisher_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = reorder(authors, average_rating), y = average_rating)) +
      geom_bar(stat = "identity", fill = "skyblue") +  # Add text labels for total ratings on top of bars
      geom_text(aes(label = round(average_rating, 2)), hjust = -0.2, size = 4, color = "black") +
      coord_flip() +
      labs(title = paste("Top Bestselling Authors by Average Rating for Publisher:", input$publisher_select),
           x = "Author",
           y = "Average Rating") +
      theme_minimal()
  })


  # Function to calculate average rating per year for selected publisher
  average_rating_year <- reactive({
    if (!is.null(input$publisher_select)) {
      filtered <- goodreads_cleaned %>%
        filter(publisher == input$publisher_select) %>%
        group_by(year) %>%
        summarise(average_rating = mean(rating_score, na.rm = TRUE))
      return(filtered)
    }
  })
  # Plot average rating per year for selected publisher
  output$average_rating_year_plot <- renderPlot({
    if (!is.null(input$publisher_select)) {
      ggplot(average_rating_year(), aes(x = year, y = average_rating)) +
        geom_line(color = "blue") +
        geom_point(color = "blue") +
        labs(title = paste("Average Rating per Year for Publisher:", input$publisher_select),
             x = "Year",
             y = "Average Rating") +
        theme_minimal()
    
      }
  })
  
  # Create top rated genres plot by publihser
  top_rated_genres <- reactive({
    filtered <- goodreads_cleaned %>%
      filter(publisher == input$publisher_select) %>%
      group_by(main_genre) %>%
      summarise(average_rating = mean(rating_score, na.rm = TRUE)) %>%
      arrange(desc(average_rating)) %>%
      top_n(5)  
    return(filtered)
  })
  output$top_rated_genres_plot <- renderPlot({
    ggplot(top_rated_genres(), aes(x = reorder(main_genre, -average_rating), y = average_rating)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      geom_text(aes(label = round(average_rating, 2)), vjust = -0.5, size = 4, color = "black") + # Add labels to the bars
      labs(title = paste("Top Rated Genres for Publisher:", input$publisher_select),
           x = "Genre",
           y = "Average Rating") +
      theme_minimal()
    
  })
  
  
  # AUTHOR TAB
  # Function to generate word cloud based on selected author
  author_wordcloud <- reactive({
    filtered <- goodreads_cleaned
    if (input$author_select != "All") {
      filtered <- filtered %>%
        filter(authors == input$author_select)
    }
    filtered %>%
      select(description) %>%
      mutate(description = tolower(description)) %>%
      unnest_tokens(word, description) %>%
      anti_join(stop_words) %>%
      #mutate(word = wordStem(word)) %>%
      count(word) %>%
      arrange(desc(n)) %>%
      head(50)
  })
  # Render word cloud for the selected author
  output$author_wordcloud <- renderWordcloud2({
    wordcloud2(author_wordcloud())
  })
  

  # Function to filter data based on selected author
  author_performance <- reactive({
    filtered <- goodreads_cleaned
    if (input$author_select != "All") {
      filtered <- filtered %>%
        filter(authors == input$author_select)
    }
    filtered %>%
      summarise(
        average_rating_score = mean(rating_score, na.rm = TRUE),
        total_num_ratings = sum(num_ratings, na.rm = TRUE),
        total_num_reviews = sum(num_reviews, na.rm = TRUE),
        total_current_readers = sum(current_readers, na.rm = TRUE),
        total_want_to_read = sum(want_to_read, na.rm = TRUE)
      )
  })
  # Create Author Performance data table
  output$author_performance_table <- renderDataTable({
    datatable(author_performance(), options = list(pageLength = 10))
  })
  


  # GENRE TAB
  # Filtered dataset based on selected genre
  filtered_books <- reactive({
    genre <- input$genre_select_books
    if (genre == "All") {
      return(goodreads_cleaned)
    } else {
      return(goodreads_cleaned[goodreads_cleaned$main_genre == genre, ])
    }
  })
  
  # assigning each genre to a color
  genre_colors <- c(
    "Children/Young Adult" = "lightblue",
    "Fantasy" = "purple",
    "Mystery/Thriller" = "#7FFF7F",
    "Nonfiction" = "orange",
    "Other" = "grey",
    "Romance" = "pink",
    "Science Fiction" = "cyan"
  )
  
  # creating histogram of book lengths
  output$book_length_histogram <- renderPlot({
    genre <- input$genre_select_books
    ggplot(filtered_books(), aes(x = num_pages)) +
      geom_histogram(binwidth = 50, fill = genre_colors[genre], color = "black") +
      labs(title = paste("Distribution of Book Lengths in Genre:", genre),
           x = "Number of Pages",
           y = "Frequency") +
      theme_minimal()
  })
  
  # creating boxplot of average rating by genre
  output$rating_boxplot <- renderPlot({
    ggplot(goodreads_cleaned %>% 
             filter(main_genre %in% names(genre_colors)), 
           aes(x = main_genre, y = rating_score, fill = main_genre)) +
      geom_boxplot(color = "black") +
      scale_fill_manual(values = genre_colors) +
      labs(title = "Average Rating by Genre",
           x = "Genre",
           y = "Average Rating") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Remove legend
  })
  
  
  # Creatomg Scatterplot showing the relationship between num_ratings and want_to_read
  output$scatterplot <- renderPlot({
    genre <- input$genre_select_books
    genre_data <- goodreads_cleaned[goodreads_cleaned$main_genre == genre, ]
    
    # Calculating correlation coefficient
    correlation <- cor(genre_data$num_ratings, genre_data$want_to_read)
    
    # Plot scatterplot
    ggplot(genre_data, aes(x = num_ratings, y = want_to_read)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +  # Add regression line without confidence interval
      labs(title = "Scatterplot of Number of Ratings versus Want to Read",
           x = "Number of Ratings",
           y = "Want to Read",
           subtitle = paste("Correlation Coefficient:", round(correlation, 2))) +  # Add correlation coefficient to subtitle
      theme_minimal()
  })
  
 
  
  # # Define a reactive expression to filter data by genre
  filtered_genre_data <- reactive({
    genre <- input$genre_select_books
    if (genre == "All") {
      return(goodreads_cleaned)
    } else {
      return(goodreads_cleaned[goodreads_cleaned$main_genre == genre, ])
    }
  })
  
  # Calculate average rating by year for the filtered genre data
  average_rating_by_year <- reactive({
    filtered_data <- filtered_genre_data()
    average_rating_by_year <- filtered_data %>%
      group_by(year) %>%
      summarise(average_rating = mean(rating_score, na.rm = TRUE))
    return(average_rating_by_year)
  })
  
  # Create line plot for average rating over the years by genre
  output$average_rating_line_plot <- renderPlot({
    ggplot(average_rating_by_year(), aes(x = year, y = average_rating)) +
      geom_line(color = "blue") +
      geom_point(color = "blue") +
      labs(title = "Average Rating Per Year by Genre",
           x = "Year",
           y = "Average Rating") +
      theme_minimal()
  })
  
  # Function to generate word cloud based on selected genre
  genre_wordcloud <- reactive({
    filtered <- goodreads_cleaned
    if (input$genre_select_books != "All") {
      filtered <- filtered %>%
        filter(main_genre == input$genre_select_books)
    }
    filtered %>%
      select(description) %>%
      mutate(description = tolower(description)) %>%
      unnest_tokens(word, description) %>%
      anti_join(stop_words) %>%
      count(word) %>%
      arrange(desc(n)) %>%
      head(50)
  })
  
  # Render word cloud for the selected genre
  output$genre_wordcloud <- renderWordcloud2({
    wordcloud2(genre_wordcloud())
  })
  
  
  
  # DATA TAB:
  # Render data table
  output$data_table <- renderDataTable({
    goodreads_cleaned
  })
  
}



# Run the application
shinyApp(ui = ui, server = server)





