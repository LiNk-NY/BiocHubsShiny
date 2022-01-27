write_session_info <- function() {
    rmd <- tempfile(fileext=".Rmd")
    writeLines(
        text = "#' ---
            #' output:
            #'   reprex::reprex_document:
            #'     venue: 'html'
            #'     session_info: TRUE
            #' ---
            packageVersion('AnnotationHubShiny')
            ", con = rmd
    )
    rmd
}

#' Initialize the shiny application for Bioconductor Hub resources
#'
#' The shiny app will allow the user to view a table of either `AnnotationHub`
#' or `ExperimentHub` resources.
#'
#' @details Note. The code here was adapted from `interactiveDisplayBase` and
#' `?'display,Hub-method'`.
#'
#' @seealso ?`interactiveDisplayBase::display`, ?`display,Hub-method`
#'
#' @param ... Further arguments to the `runApp` function
#'
#' @md
#'
#' @import shiny AnnotationHub ExperimentHub
#' @export
shinyhubs <- function(...) {
    ui <- fluidPage(
        shinytoastr::useToastr(),
# https://stackoverflow.com/questions/53616176/shiny-use-validate-inside-downloadhandler
        shinyjs::useShinyjs(),
        titlePanel(windowTitle = "AnnotationHubShiny"),
        fluidRow(
            column(8, "Bioconductor *Hub Resources"),
            helpText("The online shop for AnnotationHub and ExperimentHub Data"),
            column(4, br(),
                div(
                    style = "margin-right:10px",
                    img(
                        src = "images/bioconductor_logo_rgb_small.png",
                        align = "right"
                    )
                )
            )
        ),
        fluidRow(
            column(2,
                wellPanel(
                    h3("Hub Selection"),
                    radioButtons(
                        "hub",
                        label = "Choose either ExperimentHub or AnnotationHub",
                        choices = c("AnnotationHub", "ExperimentHub")
                    )
                )
            )
        ),
        fluidRow(
            column(9,
                tabsetPanel(
                    tabPanel(
                        title = "Bioconductor Hub",
                        h3 = textOutput("hubtitle"),
                        { DT::dataTableOutput('tbl') }
                    ),
                    tabPanel("About", {
                    HTML(paste0(
                      "<b>shinyhubs</b> version: ",
                      packageVersion("shinyhubs"),
                      "<br>",
                      "Last updated: 2022-01-27",
                      "<br>",
                      "Source: ",
                      "<a href='https://github.com/LiNk-NY/shinyhubs' class='fa fa-github'></a>",
                      "<br>",
                      markdown::markdownToHTML(rmarkdown::render(write_session_info(), "md_document", quiet = TRUE), fragment.only = TRUE)
                    ))
                  })  # end about panel
                )
            )
        )
    ) # end fluidpage
    ## from interactiveDisplayBase:::.dataFrame3
    server <- function(input, output, session) {

        # data retrieval, massaging
        hub_obj <- reactive({
            # let the user know that action is ongoing during loading
            shinytoastr::toastr_info(
                "retrieving *Hub data...", timeOut=3000
            )
            if (identical(input$hub, "AnnotationHub")) {
                hub <<- AnnotationHub::AnnotationHub()
            } else if (identical(input$hub, "ExperimentHub")) {
                hub <<- ExperimentHub::ExperimentHub()
            }
            md <- mcols(hub)
            ans <- as.data.frame(md)
            ans <- as.data.frame(
                append(as.list(ans), list(HUBID = rownames(ans)), 0L),
                row.names = NULL
            )

            ans <- ans[,
                -which(names(ans) %in%
                    c("rdataclass", "rdatapath", "sourceurl",
                      "sourcetype", "preparerclass"))
            ]
            ans$tags <- vapply(
                unname(unclass(md$tags)),
                base::paste,
                character(1), collapse = ", "
            )
            ans
        })

        # table rendering
        output$tbl <- DT::renderDataTable(
            {
                shinytoastr::toastr_info(
                    "preparing *Hub data...", timeOut=4500
                )
                on.exit({shinytoastr::toastr_info("done.", timeOut=2500)})
                biochub <<- hub_obj()
            },
            server = TRUE,
            filter = "top",
            options = list(orderClasses = TRUE)
        )

        # render title text
        output$hubtitle = renderText({
            nrec <- nrow(biochub)
            nspec <- length(unique(hub_obj[["species"]]))
            sprintf(
                paste(
                    "Search through %d", input$hub,
                    "resources from %d distinct species in Bioconductor",
                ),
                nrec, nspec
            )
        })

    }
    shinyApp(ui, server, ...)
}
