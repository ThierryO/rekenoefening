if (!require(shiny)) {
  install.packages("shiny")
  library(shiny)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require(INLA)) {
  install.packages(
    "INLA",
    repos = c(
      getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"
    ),
    dependencies = TRUE
  )
  library(INLA)
}

start_data <- function() {
  expand.grid(
    x = 0:10,
    b = 0:10,
    operator = c("\u00d7", "\u00f7"),
    stringsAsFactors = FALSE
  ) %>%
    filter(b > 0 | operator == "\u00d7") %>%
    mutate(
      a = ifelse(operator == "\u00d7", x, x * b),
      antwoord = ifelse(operator == "\u00d7", x * b, x),
      kans = 100
    ) -> default
  if (!file.exists("maaltafels.txt")) {
    default <- select(default, -x)
    return(default)
  }
  read.delim("maaltafels.txt", stringsAsFactors = FALSE) %>%
    mutate(
      x = ifelse(operator == "\u00d7", a, a / b),
      duur = ifelse(correct == 0, 100, 1) * duur
    ) %>%
    filter(b > 0 | operator == "\u00d7") %>%
    filter(x %in% b) %>%
    filter(b %in% x) %>%
    filter(x %in% b) -> combinaties
  if (length(unique(combinaties$operator)) < 2) {
    return(default)
  }
  if (length(unique(combinaties$x)) < 2) {
    return(default)
  }
  m <- inla(
    duur ~ operator + f(x, model = "iid") + f(b, copy = "x"),
    family = "gamma",
    data = combinaties,
    control.predictor = list(link = 1),
    control.compute = list(config = TRUE)
  )
  combinaties %>%
    select(x, operator, b) %>%
    mutate(
      ucl = m$summary.fitted.values[, "0.975quant"]
    ) %>%
    left_join(x = default, by = c("x", "b", "operator")) %>%
    mutate(
      kans = ifelse(is.na(ucl), 10 * max(ucl, na.rm = TRUE), ucl)
    ) %>%
    select(-ucl) -> default
  m$summary.random$x %>%
    transmute(tafel = ID, mean, var = sd ^ 2) -> rf
  attr(default, "plot") <- expand.grid(
    tafel = rf$tafel,
    operator = c("\u00d7", "\u00f7")
  ) %>%
    inner_join(rf, by = "tafel") %>%
    mutate(
      mean = mean + m$summary.fixed[1, "mean"],
      var = var + m$summary.fixed[1, "sd"] ^ 2,
      mean = mean + ifelse(operator == "\u00d7", m$summary.fixed[1, "mean"], 0),
      var = var + ifelse(operator == "\u00d7", m$summary.fixed[1, "sd"] ^ 2, 0),
      lcl = exp(qnorm(0.025, mean, sqrt(var))),
      ucl = exp(qnorm(0.975, mean, sqrt(var))),
      mean = exp(mean)
    ) %>%
    ggplot(
      aes(x = tafel, y = mean, ymin = lcl, ymax = ucl, colour = operator)
    ) +
    geom_errorbar() +
    geom_point() +
    scale_x_continuous(breaks = 0:10) +
    ylab("moeilijkheidsgraad")
  return(default)
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
  htmlOutput("score"),
  plotOutput("moeilijkheidsgraad")
)

server <- function(session, input, output) {
  data <- reactiveValues(
    combinaties = start_data(),
    ok = 1,
    score = 0,
    a = NULL
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
    data$combinaties,
    {
      if (!"plot" %in% names(attributes(data$combinaties))) {
        return(NULL)
      }
      output$moeilijkheidsgraad <- renderPlot(
        attr(data$combinaties, "plot")
      )
    }
  )

  observeEvent(
    data$ok,
    {
      if (is.null(data$a)) {
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
        return(NULL)
      }
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
        data$score <- data$score - 2
      }
      output$score <- renderText(sprintf("<h1>Score: %i</h1>", data$score))
      if (data$ok %% 10 == 0) {
        data$combinaties <- start_data()
      }
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

  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)
