shinyUI(fluidPage(

  tags$head(tags$link(rel = "shortcut icon", type="image/vnd.microsoft.icon",
                      href = "../static/images/favicon.ico"),

            tags$style(".shiny-notification {position: fixed !important;
                       top: 40vh !important;
                       left: 37.5vw !important;
                       width: 25vw !important;}"),

            tags$title("EV West Performance Calculator"),

            tags$link(href="https://fonts.googleapis.com/css?family=Roboto",
                      rel="stylesheet"),

            tags$link(rel = "stylesheet", type = "text/css",
                      href = "styles.css"),

            tags$script(type="text/javascript", src = "heap.js")
            ),

  div(id = "body",
      column(10,
             style = 'padding:0;
                      margin: 0 0 5px;
             text-align:left;
             vertical-align: bottom;',
             span(id = "logo",
                  tags$img(src = 'shield.png', width = 36, height = 30),
                 span(id = "title", "EV West"))
      ),
      # column(2,
             # uiOutput('mytabs')
      uiOutput("viewbar"),
      # ),
      column(12,
             uiOutput("views")
      )
  )
))
