.getInit <- function(hub, hubid = "click_a_row") {
    paste0(
        "## Make sure BiocManager is installed
if (!require('BiocManager', quietly = TRUE))
    install.packages('BiocManager')
## Make sure ", hub, " is installed
if (!require(", dQuote(hub), ", quietly = TRUE))
    BiocManager::install(", dQuote(hub), ")

## Use this code to download the resource
library(", dQuote(hub), ")
hub <- ", hub, "()
## Select rows in the table
",
        paste0(paste0("hub[['", hubid, "']]"), collapse = "\n")
    )
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
BiocHubsShiny <- function(...) {
    ui <- fluidPage(
        theme = shinythemes::shinytheme("simplex"),
        shinytoastr::useToastr(),
# https://stackoverflow.com/questions/53616176/shiny-use-validate-inside-downloadhandler
        shinyjs::useShinyjs(),
        fluidRow(
            column(8,
                titlePanel(
                    title = strong("Bioconductor *Hub Resources"),
                    windowTitle = "BiocHubsShiny"
                ),
                helpText(
                    "The online shop for AnnotationHub and ExperimentHub Data"
                )
            ),
            column(4, br(),
                img(
                    src = "images/bioconductor_logo_rgb_small.png",
                    align = "right",
                    style = "margin-right:10px"
                )
            )
        ),
        fluidRow(
            column(2,
                wellPanel(
                    wellPanel(
                        radioButtons(
                            "hub",
                            label = h4(strong("Select A Bioconductor Hub")),
                            choices = c("AnnotationHub", "ExperimentHub")
                        )
                    ),
                    wellPanel(
                        h4(strong("Download")),
                        br(),
                        h5("*Hub Resources"),
                        helpText(
                            "Select the rows of interest and then run the code",
                            "found in the Download tab within an R session."
                        ),
                        br(),
                        h5("*Hub Metadata"),
                        helpText(
                            "Select rows and click 'Download metadata'."
                        ),
                        downloadButton("btnSend", "Download metadata"),
                    ),
                    helpText(
                        strong("Tip"), ": Use the search box at the top",
                        "right of the table to filter records."
                    ),
                    hr(),
                    actionButton(
                        "stopBtn", "Stop BiocHubsShiny", class = "btn-primary"
                    )
                )
            ),
            column(9,
                tabsetPanel(
                    tabPanel(
                        title = "Bioconductor Hub",
                        h3(textOutput("hubtitle")),
                        { DT::dataTableOutput('tbl') }
                    ),
                    tabPanel("Download", {
                        fluidRow(
                            column(6,
                                uiOutput("ace_input")
                            )
                        )
                    }),
                    aboutPanel()
                )
            )
        ) # end fluidRow
    ) # end fluidPage
    ## from interactiveDisplayBase:::.dataFrame3
    server <- function(input, output, session) {

        output$ace_input <- renderUI({
            shinyAce::aceEditor(
                outputId = "code",
                value = .getInit(hub = input$hub),
                mode = "r"
            )
        })
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
            md <- S4Vectors::mcols(hub)
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
                on.exit({
                    shinytoastr::toastr_info("done.", timeOut=2500)
                })
                biochub <<- hub_obj()
            },
            server = TRUE,
            filter = "top",
            options = list(orderClasses = TRUE)
        )

        # render title text
        output$hubtitle <- renderText({
            nrec <- nrow(biochub)
            nspec <- length(unique(biochub[["species"]]))
            sprintf(
                paste(
                    "Search through %d", input$hub,
                    "resources from %d distinct species in Bioconductor"
                ),
                nrec, nspec
            )
        })

        observeEvent(
            input$tbl_rows_selected,
            {
                idx <- input$tbl_rows_selected
                ans <- biochub[idx, ]
                shinyAce::updateAceEditor(
                    session,
                    "code",
                    value = .getInit(hub = input$hub, hubid = ans$HUBID)
                )
            }
        )

        output$btnSend <- downloadHandler(
            filename = function() {
                fprefix <- paste0(input$hub, "_meta_sel_")
                paste0(
                    fprefix, format(Sys.time(), "%F_%Hh%Mm"), ".Rds"
                )
            },
            content = function(con) {
                idx <- input$tbl_rows_selected
                ans <- biochub[idx, ]
                saveRDS(ans, file=con)
            },
            contentType="application/octet-stream"
        )

        observeEvent(
            input$stopBtn,
            {
                # could return information here
                stopApp(returnValue=NULL)
            }
        )

        output$sessioninfo <- renderPrint({
            if (requireNamespace("sessioninfo", quietly = TRUE))
                capture.output(sessioninfo::session_info())
            else
                capture.output(utils::sessionInfo())
        })
    }
    shinyApp(ui, server, ...)
}
