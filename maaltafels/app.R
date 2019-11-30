if (!require(shiny)) {
  install.packages("shiny")
  library(shiny)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

start_data <- function() {
  expand.grid(
    x = 0:10,
    y = 0:10,
    operator = c("\u00d7", "\u00f7"),
    stringsAsFactors = FALSE
  ) %>%
    filter(y > 0 | operator == "\u00d7") %>%
    mutate(
      product = x * y,
      vraag = ifelse(operator == "\u00d7", x, product),
      antwoord = ifelse(operator == "\u00d7", product, x)
    ) %>%
    select(a = vraag, operator, b = y, antwoord) -> combinaties
  if (!file.exists("maaltafels.txt")) {
    combinaties %>%
      mutate(duur = 0, goed = 0, fout = 0) -> combinaties
  } else {
    read.table(
      "maaltafels.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t"
    ) %>%
      group_by(a, operator, b) %>%
      summarise(
        duur = mean(duur),
        goed = sum(correct),
        fout = sum(correct == 0)
      ) %>%
      ungroup() %>%
      left_join(x = combinaties, by = c("a", "operator", "b")) %>%
      mutate(
        duur = pmax(duur, 0, na.rm = TRUE),
        goed = pmax(goed, 0, na.rm = TRUE),
        fout = pmax(fout, 0, na.rm = TRUE)
      ) -> combinaties
  }
  combinaties %>%
    mutate(
      kans = (duur + 30) * (fout + 1) * (fout + goed + 1) ^ -1.5
    ) -> combinaties
  return(combinaties)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h1 {
        font-weight: 400;
      }
    "))
  ),
  tags$script('
    $(document).on("keydown", function (e) {
    Shiny.onInputChange("lastkeypresscode", e.keyCode);
    });
  '),
  fluidRow(
    column(1, htmlOutput("a")),
    column(1, htmlOutput("operator")),
    column(1, htmlOutput("b")),
    column(2, numericInput("antwoord", value = NA, label = "", width = "100px"))
  ),
  htmlOutput("vorige"),
  htmlOutput("score")
)

server <- function(session, input, output) {
  data <- reactiveValues(
    combinaties = start_data(),
    ok = 0,
    score = 0
  )

  observeEvent(
    data$combinaties,
    {
      i <- sample(
        nrow(data$combinaties),
        size = 1,
        prob = data$combinaties$kans
      )
      data$a <- data$combinaties$a[i]
      data$operator <- data$combinaties$operator[i]
      data$b <- data$combinaties$b[i]
      data$antwoord <- data$combinaties$antwoord[i]
      output$a <- renderText(
        sprintf('<h1 align="center">%i</h1>', data$a)
      )
      output$operator <- renderText(
        sprintf('<h1 align="center">%s</h1>', data$operator)
      )
      output$b <- renderText(
        sprintf('<h1 align="center">%i</h1>', data$b)
      )
      updateNumericInput(session, "antwoord", value = NA)
      data$timestamp <- Sys.time()
    }
  )

  observeEvent(
    input$lastkeypresscode,
    {
      if (input$lastkeypresscode == 13) {
        data$ok <- data$ok + 1
      }
    }
  )

  observeEvent(
    data$ok,
    {
      if (is.na(input$antwoord)) {
        return(NULL)
      }
      correct <- data$antwoord == input$antwoord
      a <- data$a
      operator <- data$operator
      b <- data$b
      antwoord <- data$antwoord
      write.table(
        data.frame(
          tijd = data$timestamp,
          duur = as.numeric(Sys.time()) - as.numeric(data$timestamp),
          a = a,
          operator = operator,
          b = b,
          antwoord = input$antwoord,
          correct = as.integer(correct),
          stringsAsFactors = FALSE
        ),
        file = "maaltafels.txt",
        sep = "\t",
        row.names = FALSE,
        col.names = !file.exists("maaltafels.txt"),
        append = file.exists("maaltafels.txt")
      )
      z <- data$combinaties$a == a & data$combinaties$operator == operator &
        data$combinaties$b == b
      if (correct) {
        output$vorige <- renderText(
          sprintf(
            "<h3>%i %s %i = %i Goed zo!</h3>",
            a,
            operator,
            b,
            antwoord
          )
        )
        data$combinaties$goed[z]  <- data$combinaties$goed[z] + 1
        data$score <- data$score + 1
      } else {
        output$vorige <- renderText(
          sprintf(
            "<h2>%i %s %i = <font color = 'red'>%i</font> Fout!</h2>",
            a,
            operator,
            b,
            antwoord
          )
        )
        data$combinaties$fout[z] <- data$combinaties$fout[z] + 1
        data$score <- data$score - 2
      }
      output$score <- renderText(sprintf("<h1>Score: %i</h1>", data$score))
    }
  )

  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)
