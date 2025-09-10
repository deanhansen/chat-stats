options(shiny.autoreload.legacy_warning = FALSE)

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
      library(showtext)
      library(sysfonts)
    })
  )
)

## Set my bootstrap theme object
my_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  info = "#282828ff",
  success = "#28A745",
  danger = "#DC3545", 
  base_font = "Inter"
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

## Info boxes at the top of the screen --------------------
value_boxes <- list(
  value_box(
    title = "ChatGPT Release Date",
    value = "November 30th, 2022",
    showcase = HTML('<img src="openai.svg" height="100%">'),
    showcase_layout = "left center",
    theme = "info",
    class = "no-border-card"
  ),
  value_box(
    title = "Weekly Active Users",
    value = "700,000,000",
    showcase = bs_icon("bar-chart-fill", size = "4rem"),
    showcase_layout = "left center",
    theme = "info",
    class = "no-border-card"
  )
)

## Cards --------------------------------------------------
cards <- list(
    card(
      class = "bg-primary text-white no-border-card",
      bs_icon("emoji-smile", size = "2rem"),
      div("Total Chats Started with ChatGPT", class = "fw-bold"),
      textOutput("total_chats")
    ),
    card(
      class = "bg-primary text-white no-border-card",
      bs_icon("chat-left-dots", size = "2rem"),
      div("Total Messages Sent to ChatGPT", class = "fw-bold"),
      textOutput("total_messages")
    ),
    card(
      class = "bg-primary text-white no-border-card",
      bs_icon("robot", size = "2rem"),
      div("Total Messages Received from ChatGPT", class = "fw-bold"),
      textOutput("total_tokens")
    ),
    card(
      class = "bg-primary text-white no-border-card",
      bs_icon("sun-fill"),
      div("What You Ask Chat About", class = "fw-bold"),
      textOutput("user_favourite_word")
  )
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
    style = "font-weight: 500; font-size: 1.5rem;",
    collapsible = TRUE
  ),
  footer = div(
    "© Dean Hansen",
    style = "padding: 6px; background: #ffffffff; margin: 0 auto;"
  ),
  nav_panel(
    "Dashboard",
    # Use fillable = TRUE to make the page content fill the available vertical space
    fillable = TRUE,
    # Remove the fixed styles to allow for responsive layout
    # style = "display: flex; flex-direction: column; overflow-y: auto;",

    ## File input
    conditionalPanel(
      condition = "input.selected_tab === 'Dashboard' && output.dataReady == false",
      div(
        id = "conversation_history_wrapper",
        style = "padding: 12px; font-size: 1.25rem;",
        file_input
      )
    ),

    ## Charts and value boxes when data is ready
    conditionalPanel(
      condition = "input.selected_tab === 'Dashboard' && output.dataReady == true",
      
      # Use layout_column_wrap for the top value boxes
      layout_column_wrap(
        width = "400px", # Set a reasonable width for each value box
        height = "300px",
        gap = "12px",
        # Use an outer card to group these two items
        !!!value_boxes
      ),

      # Use layout_column_wrap for the info cards
      layout_column_wrap(
        width = "200px",
        height = "160px",
        gap = "12px",
        class = "pt-2",
        !!!cards
      ),

      # Use layout_column_wrap for the word plot
      layout_column_wrap(
        width = "100%",
        height = "500px",
        card(
          card_header(
            "Now, Tell Me What's On Your Mind",
            tooltip(
              bs_icon("info-circle"),
              "This plot shows the top 25 words that appeaer in your conversation names. Since these are auto-generated based on your first prompt, they are a good indication of what you're asking ChatGPT about."
            )
          ),
          imageOutput("wordPlot")
        )
      ),

      # Use layout_column_wrap for the two girafe plots
      layout_column_wrap(
        width = "440px", # Define the minimum width of each card
        height = "880px",
        gap = "12px",
        card(
          card_header(
            "You Say Goodbye, And I Say Hello",
            tooltip(
              bs_icon("info-circle"),
              "This plot shows the total separate conversations started on each day of the week. Check out which weekdays you use ChatGPT the most on!"
            )
          ),
          girafeOutput("wdayPlot", width = "100%"),
          card_footer()
        ),
        card(
          card_header(
            "Chatty, Aren't Chya",
            tooltip(
              bs_icon("info-circle"),
              HTML(
                "This plot looks a bit complicated but it compares how long your messages are compared to ChatGPT's. <br><br>",
                "You'll likely see your curve climbs to the top quickly, meaning you send very short messages to ChatGPT. Comparitively, ChatGPT will have a much slower climb to the top, indicating that it is very chatty, even when you're not!"
              )
            )
          ),
          girafeOutput("distPlot", width = "100%"),
          card_footer()
        )
      )
    )
  ),
  nav_panel(
    title = "Raw Data",
    # Set to TRUE to allow the table to fill the available space
    fillable = TRUE,
    style = "padding: auto; margin: auto;",
    conditionalPanel(
      condition = "input.selected_tab === 'Raw Data' && output.dataReady == true",
      downloadButton("download_conversations", label = "Export to CSV"),
      div(
        dataTableOutput("conversation_table"),
        style = "overflow-y: auto;"
      )
    )
  ),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"
    ),
    tags$style(HTML(
      "
      /* Base button style */
      #submit_survey,
      #skip_btn,
      #continue_btn,
        .survey-btn {
          background-color: #dbdbdbff;
          border-color: #FFF;
          color: #000000ff;
          font-weight: 500;
          margin: 4px;
        }

        /* Hover state */
        #submit_survey:hover,
        #skip_btn:hover,
        #continue_btn:hover,
        .survey-btn:hover {
          background-color: var(--bs-info);
          border-color: var(--bs-info);
          color: #fff;
        }

      #btn-correct {
          background-color: #3EB489;
          border-color: #FFF;
          color: #ffffffff;
          margin: 4px;
        }

        /* Selected state for survey buttons */
        .survey-btn.selected {
          background-color: var(--bs-success);
          border-color: var(--bs-success);
          color: #fff;
        }

      img {
        display:block; 
        margin-left:auto;
        margin-right:auto;
      }

      .no-border-card {
        border: none !important;
        box-shadow: none !important; /* optional if you also want to remove shadow */
      }
      
      /* Make sure the navbar doesn't stretch outside the viewport */
      .navbar {
        width: 100%;
        max-width: 100%;
      }

      .shiny-file-input-progress {
        margin-top: 6px !important;
      }

      .progress-bar {
        color: #ffffffff;
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
    });
    "
    )
  ),
  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://github.com/deanhansen/chat-stats",
      target = "_blank",
      bs_icon(name = "github", size = "1.5rem")
    )
  )
)
