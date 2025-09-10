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
  base_font = font_google("Inter")
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

## Cards containing user data
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
  # card(
  #   class = "bg-primary text-white",
  #   bs_icon("sun"),
  #   div("% of Weekdays Talking to Chat", class = "fw-bold"),
  #   textOutput("percentage_weekdays")
  card(
    class = "bg-primary text-white no-border-card",
    bs_icon("sun-fill"),
    div("What You Ask Chat About", class = "fw-bold"),
    textOutput("user_favourite_word")
  )
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
    style = "font-weight: 500; font-size: 1.5rem;",
    collapsible = TRUE
  ),
  footer = div(
    "© Dean Hansen",
    style = "padding: 6px; background: #ffffffff; margin: 0 auto;"
  ),
  nav_panel(
    "Dashboard",
    fillable = FALSE,
    style = "display: flex; flex-direction: column; overflow-y: auto;",

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

      ## ...
      # layout_column_wrap(
      #   width = 1,
      #   card(
      #     class = "border-color: none",
      #     div(
      #       class = "alert alert-info",
      #       "
      #       You may wonder how ChatGPT generates text.
      #       If you've taken a course in linear algebra, you're almost there.
      #       Inside a generative pre-trained transformer (GPT) model, there are billions of numbers which are learned during model training.
      #       These numbers are arranged in matrices, which are then multiplied together in a particular way with the goal at the end of generating a list of decisions.
      #       Why I say decisions is because GPT models do not always select the most probable token, but provide a ranking of sorts that can then be used to sample from.
      #       I should clarify, a token is not a word, but kind of like an atom.
      #       You can use tokens to build up words.
      #       For example, if you want to spell 'are' you will need the tokens 'a' and 're'.
      #       Tokens are generally different between LLM's, because tokens are really vectors that are learned from data.
      #       So, when you type into ChatGPT using a keyboard, your text is parsed into tokens (that all have unique ID's like [2, 5002, 14, 24]) then converted into a matrix of tokens.
      #       Training these models requires lots of plain text data, and an ability to take a piece of a word and convert it into a vector. Once we have a numerical representation of the each token, we can train a model to output a new token (i.e. vector) given the preceeding tokens. There are many tricks employed to get LLM's feeling more human, but at a basic level, they are converting your input text into numbers and back."
      #       )
      #     )
      # ),

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
          card_header(
            "Now, Tell Me What's On Your Mind",
          tooltip(
              bs_icon("info-circle-fill"),
              "This plot shows the top 25 words that appeaer in your conversation names. Since these are auto-generated based on your first prompt, they are a good indication of what you're asking ChatGPT about."
            )
          ),
          plotOutput("wordPlot", height = "420px")
        )
      ),

      ## Two girafe plots side by side
      layout_column_wrap(
        width = 2,
        card(
          card_header(
            "You Say Goodbye, And I Say Hello",
            tooltip(
              bs_icon("info-circle-fill"),
              "This plot shows the total conversations started on each day of the week."
            )
          ),
          girafeOutput("wdayPlot", width = "100%"),
          card_footer()
        ),
        card(
          card_header(
            "Chatty, Aren't Chya",
          tooltip(
              bs_icon("info-circle-fill"),
              "This plot shows the number of tokens generated per response by ChatGPT and you. If you have a lot of data, you'll notice ChatGPT is quite chatty compared to you!"
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
    fillable = FALSE,
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

      .no-border-card {
        border: none !important;
        box-shadow: none !important; /* optional if you also want to remove shadow */
      }

      .navbar {
        width: 99%;
        margin: 0 auto;
        margin-top: 1rem;
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
