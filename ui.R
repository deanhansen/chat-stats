options(shiny.autoreload.legacy_warning = FALSE)

## Load packages for shiny app UI
suppressMessages(
  suppressWarnings({
    library(shiny)
    library(shinyjs)
    library(bslib)
    library(ggiraph)
    library(bsicons)
    library(DT)
    library(rlang)
    library(showtext)
    library(sysfonts)
  })
)

## Set Matrix-style bootstrap theme
my_theme <- bs_theme(
  version = 5,
  bg = "#0a0a0a",          # deep black background
  fg = "#00ff00",          # neon green text
  primary = "#00ff00",
  secondary = "#003300",
  info = "#00ff00",
  success = "#00ff00",
  danger = "#ff0000",
  base_font = font_google("Share Tech Mono") # monospaced techy font
)

## File input
file_input <- fileInput(
  inputId = "conversation_history",
  label = "Upload Conversation History",
  accept = ".json",
  multiple = FALSE, 
  width = "100%"
)

## Value boxes
value_boxes <- list(
  value_box(
    id = "chatgpt-release",
    title = "ChatGPT Release Date",
    value = span(id = "chatgpt-release-val", class = "matrix-text to-type", "November 30th, 2022"),
    showcase = HTML('<img src="openai.svg" height="100%">'),
    showcase_layout = "left center",
    theme = "info",
    class = "matrix-box"
  ),
  value_box(
    title = "Weekly Active Users",
    value = "700,000,000",
    showcase = bs_icon("person-workspace", size = "4rem", class = "matrix-icon"),
    showcase_layout = "left center",
    theme = "info",
    class = "matrix-box",
    span(id = "weekly-users", class = "matrix-text")
  )
)

## Info cards
cards <- list(
  value_box(
    title = "Total Messages Exchanged",
    value = textOutput("total_messages_exchanged"),
    showcase = HTML('<img src="messages.svg" height="90%">'),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  ),
  value_box(
    title = "Favourite Day to Chat",
    value = textOutput("favourite_day_of_the_week"),
    showcase = HTML('<img src="calendar.svg" height="90%">'),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  ),
  value_box(
    title = "Average Tokens per Message",
    value = textOutput("average_token_length"),
    showcase = HTML('<img src="coins.svg" height="90%">'),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  ),
  value_box(
    title = "What You Ask Chat About",
    value = textOutput("user_favourite_word"),
    showcase = bs_icon("sun-fill", size = "3rem", class = "matrix-icon"),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  )
)

## Shiny app UI
page_navbar(
  useShinyjs(),
  id = "selected_tab",
  theme = my_theme,
  title = bs_icon("robot", size = "2.5rem", class = "matrix-icon"),
  window_title = "ChatGPT & Me - Dean Hansen",
  navbar_options = navbar_options(
    underline = FALSE,
    style = "font-size: 1.5rem; color: #00ff00;",
    collapsible = TRUE
  ),
  nav_panel(
    "Dashboard",
    fillable = TRUE,
    
    # File input when data not ready
    conditionalPanel(
      condition = "input.selected_tab === 'Dashboard' && output.dataReady == false",
      div(
        id = "conversation_history_wrapper",
        style = "padding: 12px;",
        align = "center",
        file_input
      )
    ),

    # Dashboard content when data is ready
    conditionalPanel(
      condition = "input.selected_tab === 'Dashboard' && output.dataReady == true",
      layout_column_wrap(gap = "12px", !!!value_boxes),
      layout_column_wrap(gap = "12px", !!!cards),
      layout_column_wrap(
        gap = "12px",
        card(
          class = "matrix-box",
          card_header(
            "You Say Goodbye, And I Say Hello",
            tooltip(
              bs_icon("info-circle", class = "matrix-icon"),
              "This plot shows the total separate conversations started on each day of the week."
            )
          ),
          girafeOutput("wdayPlot", width = "100%")
        ),
        card(
          class = "matrix-box",
          card_header(
            "Chatty, Aren't Chya",
            tooltip(
              bs_icon("info-circle", class = "matrix-icon"),
              "Compare your messages length with ChatGPT's. Short vs. chatty messages!"
            )
          ),
          girafeOutput("distPlot", width = "100%")
        )
      )
    )
  ),
  nav_panel(
    id = "nav_panel_raw_data",
    title = "Raw Data",
    fillable = TRUE, 
    style = "padding: auto; margin: auto; background: none !important; background-color: #0a0a0a; background-image: none !important;",
    conditionalPanel(
      condition = "input.selected_tab === 'Raw Data' && output.dataReady == true",
      downloadButton("download_conversations", label = "Export to CSV"),
      div(
        dataTableOutput("conversation_table"),
        style = "overflow-y: auto;"
      )
    )
  ),
  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://github.com/deanhansen/chat-stats",
      target = "_blank",
      bs_icon(name = "github", size = "2.5rem", class = "matrix-icon")
    )
  ),
  includeCSS("www/styles.css"),
  includeScript("www/matrix_text.js"),
  includeScript("www/survey_btn.js")
)
