# Theme ------------------------------------------------------------------

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

# File Input -------------------------------------------------------------

file_input <- fileInput(
  inputId = "conversation_history",
  label = NULL,
  placeholder = "Upload your conversation_history.json file", 
  accept = ".json",
  multiple = FALSE,
  width = "100%"
)

# Top Row Boxes ----------------------------------------------------------

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

# Second Row Boxes -------------------------------------------------------

second_row_boxes <- list(
  value_box(
    title = "When You First Met",
    value = textOutput("first_conversation_date"),
    showcase = bs_icon("heart", size = "3rem", class = "matrix-icon"),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  ),
  value_box(
    title = "Total Conversations",
    value = textOutput("total_days"),
    showcase = bs_icon("chat-left-dots", size = "3rem", class = "matrix-icon"),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  ),
  value_box(
    title = "Messages Exchanged",
    value = textOutput("total_messages_exchanged"),
    showcase = bs_icon("arrow-left-right", size = "3rem", class = "matrix-icon"),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  ),
  value_box(
    title = "Average Tokens Sent (Rec)",
    value = textOutput("average_token_length"),
    showcase = bs_icon("cup-hot", size = "3rem", class = "matrix-icon"),
    showcase_layout = "left center",
    theme = "secondary",
    class = "matrix-box"
  )
)

# First Row Plots --------------------------------------------------------

first_row_plots <- list(
  card(
    class = "matrix-box",
    card_header(
      "Now Tell Me, What's On Your Mind",
      tooltip(
        bs_icon("info-circle", class = "matrix-icon"),
        "What topics you're asking ChatGPT about!"
      )
    ),
    girafeOutput("wordPlot")
  ),
  # card(
  #   class = "matrix-box",
  #   card_header(
  #     "You Say Goodbye, And I Say Hello",
  #     tooltip(
  #       bs_icon("info-circle", class = "matrix-icon"),
  #       "The number of separate conversations started by day of the week."
  #     )
  #   ),
  #   girafeOutput("wdayPlot", width = "100%")
  # ),
  card(
    class = "matrix-box",
    card_header(
      "Chatty, Aren't Chya",
      tooltip(
        bs_icon("info-circle", class = "matrix-icon"),
        "Compare your message lengths with ChatGPT's. Short vs. chatty!"
      )
    ),
    girafeOutput("distPlot")
  )
)

# Second Row Plots -------------------------------------------------------

# second_row_plots <- list(
#   card(
#     class = "matrix-box",
#     card_header(
#       "Now Tell Me, What's On Your Mind",
#       tooltip(
#         bs_icon("info-circle", class = "matrix-icon"),
#         "What topics you're asking ChatGPT about!"
#       )
#     ),
#     girafeOutput("wordPlot", width = "100%")
#   )
# )

# Shiny UI ---------------------------------------------------------------

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
          "To download your ChatGPT conversation history, follow this ",
          tags$a(
              href = "https://help.openai.com/en/articles/7260999-how-do-i-export-my-chatgpt-history-and-data",
              target = "_blank",
              "link",
              style = "color: #00ff00;"
            ),
          )
        )
      ),
    conditionalPanel(
      condition = "input.selected_tab === 'Dashboard' && output.dataReady == true",
      div(id = "first_row_boxes_wrapper",  style = "display: none;", layout_columns(col_widths = c(6, 6),       gap = "12px", !!!first_row_boxes)),
      div(id = "second_row_boxes_wrapper", style = "display: none;", layout_columns(col_widths = c(3, 3, 3, 3), gap = "12px", !!!second_row_boxes)),
      div(id = "first_row_plots_wrapper",  style = "display: none;", layout_columns(col_widths = c(6, 6),       gap = "12px", !!!first_row_plots)),
      div(id = "second_row_plots_wrapper", style = "display: none;",
        layout_columns(
          col_widths = c(-3, 6, -3),
          gap = "12px",
          navset_card_underline(
            title = tagList(
              "You Say Goodbye, And I Say Hello",
              tooltip(
                bs_icon("info-circle", class = "matrix-icon"),
                "Total messages sent to ChatGPT by weekday, hour of the day and minute."
              )
            ),
            nav_panel("Weekday", girafeOutput("wdayPlot")),
            nav_panel("Hour", girafeOutput("hourPlot")),
            nav_panel("Minute", girafeOutput("minutePlot"))
          )
        )
      )
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
  # nav_item(
  # tags$li(
  #   class = "nav-item",
  #   tags$a(
  #     href   = "https://open.spotify.com/playlist/1ajovwTqrfeMBiytNq2Yrg?si=07eda3c58ae542a5",
  #     target = "_blank",
  #     title  = "Matrix (1999) Official Soundtrack",
  #     class  = "nav-link",
  #     style  = "margin-left: -1rem;",
  #     "Some Inspiration"
  #     )
  #   )
  # ),
  nav_item(
    tags$a(
      href = "https://open.spotify.com/playlist/1ajovwTqrfeMBiytNq2Yrg?si=07eda3c58ae542a5",
      target = "_blank", 
      title  = "Matrix (1999) Official Soundtrack",
      onclick = "return confirm('This will take you to a Spotify playlist. Do you want to open this link?');",
      bs_icon(name = "sunglasses", size = "1.5rem", class = "matrix-icon")
    ),
    class = "sunglasses"
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

  ## Google analytics
  tags$head(
    HTML('
      <script async src="https://www.googletagmanager.com/gtag/js?id=G-8PS2C6ZQNR"></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag("js", new Date());
        gtag("config", "G-8PS2C6ZQNR");
      </script>
    ')
  ),

  # CSS ------------------------------------------------------------------
  includeCSS("www/styles.css"),

  # JavaScript -----------------------------------------------------------
  includeScript("www/survey_btn.js")

)
