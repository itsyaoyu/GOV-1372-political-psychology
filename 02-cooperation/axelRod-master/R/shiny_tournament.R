.all_values <- function(x) {
  if (is.null(x))
    return(NULL)

  paste0("Score: ", x$stack_upr)
}


#' @title Axelrod Tournament Shiny App
#'
#' @description This function launches a Shiny app to run "Prisoner's Dilemma"
#'  matches between pairs of strategies.
#'
#' @param payoff A 2x2 payoff matrix of the tournament.
#'
#' @param custom_strategies A list of custom strategies, following the model of
#'  the list returned by \code{\link{defaultStrategies}}.
#'
#' @param with_default_strategies If TRUE, the tournament app uses the default
#'  strategies returned by \code{\link{defaultStrategies}} (the default), in
#'  addition of any custom strategy provided by the user. If FALSE, it uses only
#'  the custom strategies provided by the user.
#'
#' @return This function launches a Shiny application.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#'
#' @export
#'
shiny_tournament <- function(payoff = matrix(c(3, 5, 0, 1), nrow = 2),
                             custom_strategies = NULL,
                             with_default_strategies = TRUE) {
  require(shiny)
  require(dplyr)
  require(ggvis)
  # load strategies in memory
  if (is.null(custom_strategies)) {
    strats <- defaultStrategies()
  } else {
    strats <- custom_strategies

    if (with_default_strategies) {
      strats <- c(strats, defaultStrategies())
    }
  }

  # define global variables
  counter <- 1
  steps <- 1
  dat <- data.frame(rep = 1, player = c("Player 1", "Player 2"), cum_score = 0)

  shinyApp(
    ui = navbarPage(
      title = "Axelrod Tournament",
      theme = "www/bootstrap.css",
      fluid = FALSE,
      collapsible = TRUE,

      tabPanel("Tournament",

               fluidRow(

                 column(width = 3, align = "center", style = "background-color:#ebb397;",
                        div(style = "height:450px;",
                            selectInput("strat1", label = h5("Player 1's Strategy"),
                                        choices = lapply(strats, function(x) x$name),
                                        selected = 1),
                            tags$hr(),
                            verbatimTextOutput("strat1"))),

                 column(width = 6, align = "center", style = "background-color:#f5f5f5;",
                        ggvisOutput("display")),

                 column(width = 3, align = "center", style = "background-color:#9ecfbf;",
                        div(style = "height:450px;",
                            selectInput("strat2", label = h5("Player 2's Strategy"),
                                        choices = lapply(strats, function(x) x$name),
                                        selected = 1),
                            tags$hr(),
                            verbatimTextOutput("strat2")))
               ),

               fluidRow(style = "background-color:#f5f5f5;",

                        column(width = 2, align = "center", offset = 2,
                               selectInput("tournament", label = h5("Tournament type"),
                                           choices = list("One time" = "onetime", "Repeated" = "repeated"),
                                           selected = 1)),

                        column(width = 2, align = "center",
                               uiOutput("rounds")),

                        column(width = 2, align = "center",
                               sliderInput("reps", label = h5("Number of replicates"), min = 1,
                                           max = 100, value = 20)),

                        column(width = 2, align = "center",
                               h5(HTML("&nbsp;")),
                               actionButton("play", "Play", width = "100%"))
               )
      ),

      tabPanel("Instructions"),

      tabPanel("About",

               fluidRow(
                 tags$hr(),

                 p(strong("Author:"), " Simon Garnier (", a("New Jersey Institute of Technology",
                                                            href = "http://www.njit.edu",
                                                            target = "_blank"), ")"),

                 p(strong("Twitter:"), a("@sjmgarnier",
                                         href = "https://twitter.com/sjmgarnier",
                                         target = "_blank")),

                 p(strong("Website:"), a("http://www.theswarmlab.com",
                                         href = "http://www.theswarmlab.com",
                                         target = "_blank")),

                 p(strong("Source code:"),
                   a("GitHub",
                     href = "https://github.com/swarm-lab/axelRod",
                     target = "_blank")),

                 p(strong("Created with:"),
                   a("RStudio",
                     href = "http://www.rstudio.com/",
                     target = "_blank"),
                   " and ",
                   a("Shiny.",
                     href = "http://shiny.rstudio.com",
                     target = "_blank")),

                 p(strong("License:"),
                   a("GPL v3",
                     href = "http://www.gnu.org/copyleft/gpl.html",
                     target = "_blank")),

                 tags$hr()
               )
      ),

      tabPanel(tagList(tags$html("Powered by"),
                       tags$img(src = "www/white-rstudio-logo.png",
                                height = "20")),
               value = "RStudio",
               tags$head(tags$script(src = "www/actions.js"))
      )
    ),
    server = function(input, output) {
      react <- reactiveValues(reset_draw = 0, draw = 0)

      observe({
        react$reset_draw
        counter <<- 1
        steps <<- max(dat$rep)
        isolate({ react$draw <- react$draw + 1 })
      })

      react_dat <- reactive({
        react$draw

        if (counter <= steps) {
          counter <<- counter + 1
          invalidateLater(60, NULL)
        }

        dat %>% filter(rep == counter - 1)
      })

      output$strat1 <- renderText({
        idx <- which(sapply(strats, function(x, y) x$name == y, y = input$strat1))
        strats[[idx]]$description
      })

      output$strat2 <- renderText({
        idx <- which(sapply(strats, function(x, y) x$name == y, y = input$strat2))
        strats[[idx]]$description
      })

      output$rounds <- renderUI({
        if (input$tournament == "onetime") {
          sliderInput("rounds", label = h5("Number of rounds"), min = 1,
                      max = 1, value = 1)
        } else {
          sliderInput("rounds", label = h5("Number of rounds"), min = 1,
                      max = 100, value = 20)
        }
      })

      observe({
        if (input$play > 0) {
          isolate({
            idx1 <- which(sapply(strats, function(x, y) x$name == y, y = input$strat1))
            idx2 <- which(sapply(strats, function(x, y) x$name == y, y = input$strat2))

            tournament <- Tournament$new(type = input$tournament,
                                         players = list("Player 1" = strats[[idx1]]$fn,
                                                        "Player 2" = strats[[idx2]]$fn),
                                         nreps = input$reps, nrounds = input$rounds,
                                         payoff = payoff)
            tournament$play()

            dat <<- group_by(tournament$res, player, rep) %>%
              summarize(score = sum(score)) %>%
              mutate(cum_score = cumsum(score) / rep)

            react$reset_draw <- react$reset_draw + 1
          })
        }
      })

      react_dat %>%
        mutate(player1 = player, player2 = player) %>%
        ggvis(x = ~player, y = ~cum_score, fill = ~player1, stroke = ~player2) %>% #
        layer_bars() %>%
        scale_nominal("fill", range = c("#ebb397", "#9ecfbf")) %>%
        scale_nominal("stroke", range = c("#ebb397", "#9ecfbf")) %>%
        hide_legend(c("fill", "stroke")) %>%
        add_tooltip(.all_values, "hover") %>%
        add_axis("x", title = "", properties = axis_props(labels = list(fontSize = 18))) %>%
        add_axis("y", title = "y", properties = axis_props(
          labels = list(fontSize = 16),
          title = list(fontSize = 20, stroke = "#f5f5f5", fill = "#f5f5f5"))) %>%
        add_axis("y", title = "y", orient = "right", properties = axis_props(
          labels = list(fontSize = 16),
          title = list(fontSize = 20, stroke = "#f5f5f5", fill = "#f5f5f5"))) %>%
        set_options(width = "auto", height = "450px", resizable = FALSE, duration = 30) %>%
        bind_shiny("display")
    }
  )
}
