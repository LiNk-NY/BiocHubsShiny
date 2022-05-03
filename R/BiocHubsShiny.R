.shinyAce_template <- function(hub, hubid = "click_a_row") {
    hubname <- dQuote(hub, FALSE)
    p0 <- paste0
    paste(
        "## Make sure BiocManager is installed",
        "if (!require('BiocManager', quietly = TRUE))",
        "    install.packages('BiocManager')\n",
        p0("## Make sure ", hub, " is installed"),
        p0("if (!require(", hubname, ", quietly = TRUE))"),
        p0("    BiocManager::install(", hubname, ")", "\n"),
        "## Use this code to download the resource",
        p0("library(", hubname, ")"),
        p0("hub <- ", hub, "()", "\n"),
        "## Select rows in the table",
        p0(p0("hub[['", hubid, "']]"), collapse = "\n"),
        sep = "\n"
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
        titlePanel(
            windowTitle = "BiocHubsShiny",
            title = div(
               img(
                 src = "images/bioconductor_logo_rgb_small.png",
                 align = "right",
                 style = "margin-right:10px"
               ),
               strong("Bioconductor *Hub Resources")
            )
        ),
        helpText(
            "The online shop for AnnotationHub and ExperimentHub Data"
        ),
        br(),
        sidebarLayout(
            div(class = "sidebar",
                sidebarPanel(
                    radioButtons(
                        "hub",
                        label = h4(strong("Select A Bioconductor Hub")),
                        choices = c("AnnotationHub", "ExperimentHub")
                    ),
                    h4(strong("Download")),
                    br(),
                    h5("*Hub Resources"),
                    helpText(
                        "Select the rows of interest and then run the code",
                        "below the table within an R session."
                    ),
                    br(),
                    h5("*Hub Metadata"),
                    helpText(
                        "Select rows and click 'Download metadata'."
                    ),
                    downloadButton("btnSend", "Download metadata"),
                    helpText(
                        strong("Tip"), ": Use the search box at the top",
                        "right of the table to filter records."
                    ),
                    hr(),
                    actionButton(
                        "stopBtn", "Stop BiocHubsShiny", class = "btn-primary"
                    ),
                    width = 2
                )
            ),
            mainPanel(navbarPage(
                title = tagList(
                    actionLink("sidebar_button", "", icon = icon("bars")), ""
                ),
                id = "navbarID",
                tabPanel(
                    title = "Bioconductor Hub",
                    h3(textOutput("hubtitle")),
                    fluidRow(
                        DT::dataTableOutput('tbl')
                    ),
                    fluidRow(
                        uiOutput("ace_input")
                    )
                ),
                tabPanel(
                    title = "About",
                    aboutPanel(),
                    value = "about"
                )
            ), width = 10)
        )
    ) # end fluidPage
    ## from interactiveDisplayBase:::.dataFrame3
    server <- function(input, output, session) {

        output$ace_input <- renderUI({
            shinyAce::aceEditor(
                outputId = "code",
                value = .shinyAce_template(hub = input$hub),
                height = "380px", fontSize = 18, mode = "r"
            )
        })
        # data retrieval, massaging
        hub_obj <- reactive({
            # let the user know that action is ongoing during loading
            shinytoastr::toastr_info(
                "retrieving *Hub data...", timeOut=3000
            )
            if (identical(input$hub, "AnnotationHub")) {
                hub <- AnnotationHub::AnnotationHub()
            } else if (identical(input$hub, "ExperimentHub")) {
                hub <- ExperimentHub::ExperimentHub()
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
                hub_obj()
            },
            server = TRUE,
            rownames = FALSE,
            filter = "top",
            options = list(orderClasses = TRUE, pageLength = 6)
        )

        # render title text
        output$hubtitle <- renderText({
            biochub <- hub_obj()
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
                biochub <- hub_obj()
                ans <- biochub[idx, ]
                shinyAce::updateAceEditor(
                    session,
                    "code",
                    value = .shinyAce_template(
                        hub = input$hub, hubid = ans$HUBID
                    )
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
                biochub <- hub_obj()
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

        observeEvent(input$sidebar_button,{
            shinyjs::toggle(selector = ".sidebar")
        })

        output$sessioninfo <- renderPrint({
            if (requireNamespace("sessioninfo", quietly = TRUE))
                capture.output(sessioninfo::session_info())
            else
                capture.output(utils::sessionInfo())
        })
    }
    shinyApp(ui, server, ...)
}
