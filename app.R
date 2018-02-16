library(shiny)
library(ggplot2)
library(dplyr)

start_data <- function() {
  basis <- do.call(rbind, lapply(
    0:10,
    function(i) {
      data.frame(som = i, vraag = 0:i, antwoord = i:0)
    }
  ))
  if (file.exists("splitskaart.txt")) {
    read.delim("splitskaart.txt") %>%
      mutate(correct = som == vraag + antwoord) %>%
      group_by(som, vraag) %>%
      summarise(
        goed = sum(correct),
        fout = sum(!correct)
      ) %>%
      left_join(x = basis, by = c("som", "vraag")) %>%
      mutate(
        goed = pmax(goed, 0, na.rm = TRUE),
        fout = pmax(fout, 0, na.rm = TRUE)
      ) -> z
  } else {
    basis %>%
      mutate(goed = 0, fout = 0) -> z
  }
  return(z)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h1 {
        font-weight: 400;
      }
      .som {
        border: 1px solid;
      }
      .vraag {
        border-right: 1px solid;
      }
    "))
  ),
  tags$script('
    $(document).on("keydown", function (e) {
    Shiny.onInputChange("lastkeypresscode", e.keyCode);
    });
  '),
  fluidRow(
    column(2, htmlOutput("som"))
  ),
  fluidRow(
    column(1, htmlOutput("vraag")),
    column(1, numericInput("antwoord", value = NA, label = "", width = 100))
  ),
  htmlOutput("vorige"),
  plotOutput("score")
)

server <- function(session, input, output) {
  data <- reactiveValues(
    combinaties = start_data(),
    ok = 0
  )

  observeEvent(
    data$combinaties,
    {
      i <- sample(
        seq_along(data$combinaties$som),
        size = 1,
        prob = (data$combinaties$fout + 1) / (data$combinaties$goed + data$combinaties$fout + 1)
      )
      data$som <- data$combinaties$som[i]
      data$vraag <- data$combinaties$vraag[i]
      output$score <- renderPlot(
        ggplot(
          data$combinaties,
          aes(x = som, y = vraag, size = goed + fout, colour = goed / (goed + fout))
        ) +
          geom_point() +
          scale_size("aantal") +
          scale_colour_gradient2("score", low = "red", high = "green", midpoint = 0.5)
      )
      output$som <- renderText(
        sprintf('<h1 align="center", class="som">%i</h1>', data$som)
      )
      output$vraag <- renderText(
        sprintf('<h1 align="center", class="vraag">%i</h1>', data$vraag)
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
      som <- data$som
      vraag <- data$vraag
      antwoord <- input$antwoord
      write.table(
        data.frame(
          tijd = data$timestamp,
          duur = as.numeric(Sys.time()) - as.numeric(data$timestamp),
          som = som,
          vraag = vraag,
          antwoord = antwoord
        ),
        file = "splitskaart.txt",
        sep = "\t",
        row.names = FALSE,
        col.names = !file.exists("splitskaart.txt"),
        append = file.exists("splitskaart.txt")
      )
      i <- data$combinaties$som == som & data$combinaties$vraag == vraag
      if (som == vraag + antwoord) {
        output$vorige <- renderText(
          sprintf(
            "%i = %i + %i\nGoed zo!",
            som,
            vraag,
            antwoord
          )
        )
        data$combinaties$goed[i] <- data$combinaties$goed[i] + 1
      } else {
        output$vorige <- renderText(
          sprintf(
            "%i = %i + <font color = 'red'>%i</font>\nFout!",
            som,
            vraag,
            som - vraag
          )
        )
        data$combinaties$fout[i] <- data$combinaties$fout[i] + 1
      }
    }
  )

  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)
