## Load packages for shiny app UI
suppressMessages(
  suppressMessages(
    suppressWarnings({
      library(shiny)
      library(shinyjs)
      library(bslib)
      library(ggiraph)
      library(bsicons)
      library(DT)
      library(rlang)
    })
  )
)

## Set my bootstrap theme object
my_theme <- bs_theme(
  version = 5,
  bootswatch = "sketchy",
  # "border-radius" = "8px",
  # primary = "#007BFF", # A fresh, professional blue
  # secondary = "#6C757D", # A clean gray for secondary elements
  info = "#4e9387ff", # A purple accent for information
  success = "#28A745", # Green for success
  danger = "#DC3545", # Red for warnings/errors
  # bg = "#F8F9FA", # A soft, light gray for the background
  # fg = "#212529" # A dark, readable gray for the text
)

## File input button and box
file_input <-
  fileInput(
    inputId = "conversation_history",
    label = "Upload chat history (.json)",
    accept = ".json",
    multiple = FALSE,
    width = "400px"
  )

## Static information boxes regarding ChatGPT usage worldwide
value_boxes <- list(
  value_box(
    title = "ChatGPT Release Date",
    value = "November 30th, 2022",
    showcase = bs_icon("robot", size = "4rem"),
    showcase_layout = "left center",
    theme = "info"
  ),
  value_box(
    title = "Weekly Active Users",
    value = "700,000,000",
    showcase = bs_icon("bar-chart", size = "4rem"),
    showcase_layout = "left center",
    theme = "info"
  )
)

## Cards containing user data
cards <- list(
  card(
    class = "bg-primary text-white",
    bs_icon("chat-left-dots"),
    div("Total Messages Sent to ChatGPT", class = "fw-bold"),
    textOutput("total_messages")
  ),
  card(
    class = "bg-primary text-white",
    bs_icon("robot"),
    div("Total Messages Received from ChatGPT", class = "fw-bold"),
    textOutput("total_tokens")
  ),
  card(
    class = "bg-primary text-white",
    bs_icon("emoji-smile"),
    div("Total Chats Started with ChatGPT", class = "fw-bold"),
    textOutput("total_chats")
  )
  # card(
  #   class = "bg-primary text-white",
  #   bs_icon("sun"),
  #   div("% of Weekdays Talking to Chat", class = "fw-bold"),
  #   textOutput("percentage_weekdays")
  # ),
  # card(
  #   class = "bg-primary text-white",
  #   bs_icon("sun"),
  #   div("What You Ask Chat About", class = "fw-bold"),
  #   textOutput("user_favourite_word")
  # ),
  # card(
  #   class = "bg-primary text-white",
  #   bs_icon("sun"),
  #   div("What Chat Talks to You About", class = "fw-bold"),
  #   textOutput("assistant_favourite_word")
  # )
)

## Define the app
page_navbar(
  useShinyjs(),
  id = "selected_tab",
  theme = my_theme,
  title = bs_icon("robot", size = "2.5rem"),
  window_title = "ChatGPT & Me - Dean Hansen",
  navbar_options = navbar_options(
    underline = FALSE,
    position = "static-top",
    style = "font-weight: 500; font-size: 20px;",
    collapsible = FALSE
  ),
  footer = div("© Dean Hansen", style = "padding: 12px; background: #ffffffff; margin: 0 auto;"),
  nav_panel(
    "Dashboard",
    fillable = FALSE,
    style = "display: flex; flex-direction: column; overflow-y: auto;",

    ## File input
    conditionalPanel(
      condition = "input.selected_tab === 'Dashboard' && output.dataReady == false",
      div(
        id = "conversation_history_wrapper",
        style = "padding: 12px; font-size: 14pt;",
        file_input
      )
    ),

    ## Charts and value boxes when data is ready
    conditionalPanel(
      condition = "input.selected_tab === 'Dashboard' && output.dataReady == true",

      ## Value boxes side by side
      layout_column_wrap(
        width = 2,
        gap = "12px",
        !!!value_boxes
      ),

      ## Cards side by side
      layout_columns(
        width = 3,
        gap = "12px",
        !!!cards
      ),

      ## Word plot
      layout_column_wrap(
        width = 1,
        card(
          card_header("Word Usage Analysis"),
          plotOutput("wordPlot", height = "400px")
        )
      ),

      # Two girafe plots side by side
      layout_column_wrap(
        width = 2,
        card(
          card_header("You Say Goodbye, And I Say Hello", class = "font-weight: 600;"),
          girafeOutput("wdayPlot", width = "100%"),
          card_footer("Source: OpenAI")
        ),
        card(
          card_header("Chatty, Aren't Chya", class = "font-weight: 600;"),
          girafeOutput("distPlot", width = "100%"),
          card_footer("Source: OpenAI")
        )
      )
    )
  ),
  nav_panel(
    title = "Raw Data",
    fillable = FALSE,
    style = "padding: auto; margin: auto;",
    conditionalPanel(
      condition = "input.selected_tab === 'Raw Data' && output.dataReady == true",
      div(
        dataTableOutput("conversation_table"),
        style = "overflow-y: auto;"
      )
    )
  ),
  tags$head(
    tags$style(HTML(
      "
      #submit_survey {
        background-color: rgba(110, 56, 117, 1) !important;
        border-color: #eee !important;
        color: white !important;
      }

      #submit_survey:hover {
        background-color: rgba(46, 24, 49, 1) !important;
        border-color: #7a1f87 !important;
      }

      #skip_btn {
        background-color: #7b3687ff !important;
        border-color: #eee !important;
        color: white !important;
      }

      #skip_btn:hover {
        background-color: rgba(56, 15, 61, 1) !important;
        border-color: #7a1f87 !important;
      }
      
      .survey-btn {
        border-color: #ffffffff;
        background-color: #e7e7e7ff;
        color: #000;
      }

      .survey-btn:hover {
        border-color: #fff;
        background-color: #c0c0c0ff;
        color: #000;
      }

      .survey-btn.selected {
        border-color: #232323ff;
        border: 4px;
        background-color: #000000ff;
        color: #eee;
      }

      .navbar {
        width: 99%;
        margin: 0 auto;
        margin-top: 1rem;
      }

      .shiny-file-input-progress {
        margin-top: 6px !important;
        background-color: #9c9c9cff;
      }
    
      .progress-bar {
        background-color: #00000018;
        color: #eee;
      }

      .card-header {
        font-size: 1.5rem;
      }
      
      .card-body {
        font-size: 1.3rem;
      }
    "
    ))
  ),
  tags$script(
    HTML(
      "
    $(document).on('click', '.survey-btn', function() {
      // Remove 'selected' from other buttons in the same group
      $(this).siblings().removeClass('selected');
      // Add 'selected' to the clicked button
      $(this).addClass('selected');
    });"
    )
  )
)
