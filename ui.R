# Packages ----------------------------------------

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

# Theme ---------------------------------------------

my_theme <- bs_theme(
  version = 5,
  bg = "#0a0a0a",
  fg = "#00ff00",
  primary = "#00ff00",
  secondary = "#003300",
  info = "#00ff00",
  success = "#00ff00",
  danger = "#ff0000",
  base_font = font_google("Share Tech Mono")
)

# File Input ----------------------------------------

file_input <- fileInput(
  inputId = "conversation_history",
  label = "Upload Conversation History",
  accept = ".json",
  multiple = FALSE,
  width = "100%"
)

# Top Row Boxes ----------------------------------------

first_row_boxes <- list(
  value_box(
    id = "chatgpt-release",
    title = "ChatGPT Release Date",
    value = span(
      id = "chatgpt-release-val",
      class = "matrix-text to-type",
      "November 30th, 2022"
    ),
    showcase = bs_icon("openai", size = "4rem", class = "matrix-icon"),
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

# Second Row Boxes ----------------------------------------

second_row_boxes <- list(
  value_box(
    title = "Messages Exchanged",
    value = textOutput("total_messages_exchanged"),
    showcase = bs_icon("chat-left-dots", size = "3rem", class = "matrix-icon"),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  ),
  value_box(
    title = "When You Like to Chat",
    value = textOutput("favourite_day_of_the_week"),
    showcase = bs_icon("calendar-week", size = "3rem", class = "matrix-icon"),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  ),
  value_box(
    title = "Avg Tokens Sent (Rec)",
    value = textOutput("average_token_length"),
    showcase = bs_icon("coin", size = "3rem", class = "matrix-icon"),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  ),
  value_box(
    title = "Days Used in 2025",
    value = textOutput("percentage_of_days_in_2025"),
    showcase = bs_icon("arrow-through-heart", size = "3rem", class = "matrix-icon"),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  )
)

# First Row Plots ----------------------------------------

first_row_plots <- list(
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

# Shiny UI ---------------------------------------------------------

page_navbar(
  useShinyjs(),
  id = "selected_tab",
  theme = my_theme,
  fluid = TRUE,
  window_title = "ChatGPT Conversation History - Dean Hansen",
  navbar_options = navbar_options(underline = TRUE, collapsible = TRUE),

  # Nav Panel - Dashboard ------------------------------------------------
  nav_panel(
    id = "nav_panel_dashboard",
    title = "Dashboard",
    conditionalPanel(
      condition = "input.selected_tab === 'Dashboard' && output.dataReady == false",
      div(
        id = "conversation_history_wrapper",
        file_input,
        tags$div(
          "To download your conversation history, follow this ",
          tags$a(
              href = "https://help.openai.com/en/articles/7260999-how-do-i-export-my-chatgpt-history-and-data",
              target = "_blank",
              "link"
            )
          )
        ),
    #   div(
    #     id = "instructions_wrapper",
    #     tags$ul(
    #       tags$h4("How to Download Your Conversation History"),
    #       tags$li("Go to your OpenAI account settings: Settings > Data Controls > Export Data"),
    #       tags$li("Click Request Export. OpenAI will prepare a zip file containing conversation_history.json"),
    #       tags$li("Download and extract the zip file to get conversation_history.json"),
    #       tags$li("Load it in R or Python for processing.")
    #     )
    #   )
    ),
    conditionalPanel(
      condition = "input.selected_tab === 'Dashboard' && output.dataReady == true",
      div(id = "first_row_boxes_wrapper",  style = "display: none;", layout_column_wrap(gap = "12px", !!!first_row_boxes)),
      div(id = "second_row_boxes_wrapper", style = "display: none;", layout_column_wrap(gap = "12px", !!!second_row_boxes)),
      div(id = "first_row_plots_wrapper",  style = "display: none;", layout_column_wrap(gap = "12px", !!!first_row_plots))
    )
  ),

  # Nav Panel - Raw Data -------------------------------------------------
  nav_panel(
    id = "nav_panel_raw_data",
    title = "Raw Data",
    conditionalPanel(
      condition = "input.selected_tab === 'Raw Data' && output.dataReady == true",
      downloadButton("download_conversations", label = "Export to CSV"),
      div(
        dataTableOutput("conversation_table"),
        style = "overflow-y: auto;"
      )
    )
  ),

  # Navbar - GitHub Icon -------------------------------------------------
  nav_item(
  tags$li(
    class = "nav-item",
    tags$a(
      href   = "https://open.spotify.com/playlist/1ajovwTqrfeMBiytNq2Yrg?si=07eda3c58ae542a5",
      target = "_blank",
      title  = "Matrix (1999) Official Soundtrack",
      class  = "nav-link",
      style  = "margin-left: -1rem;",
      "Some Inspiration"
    )
  )
),
  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://github.com/deanhansen/chat-stats",
      target = "_blank", 
      title  = "Source Code",
      bs_icon(name = "github", size = "1.5rem", class = "matrix-icon")
    )
  ),

  # CSS ------------------------------------------------------------------
  includeCSS("www/styles.css"),

  # JavaScript -----------------------------------------------------------
  includeScript("www/matrix_text.js"),
  includeScript("www/survey_btn.js")
)
