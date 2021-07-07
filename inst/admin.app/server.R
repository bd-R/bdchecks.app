library(shiny)
library(shinyjs)
library(DT)
library(rhandsontable)
library(yaml)
library(bdchecks)

source("functions.R")

shinyServer(function(input, output, session) {
    checks <- list()
    tests <- list()
    scripts <- list()
    
    write_path <- NULL
    read_path <- NULL
    save_origin <- F
    
    temp <- "../../../bdchecks"
    if (!is.null(.GlobalEnv$.bdchecksLocation)) {
        temp <- .GlobalEnv$.bdchecksLocation
    }
    
    modal <- modalDialog(
        title = "Configuration",
        textInput(
            "src_location",
            "bdchecks Source Directory Path:",
            temp
        ),
        checkboxInput(
            "save_to_src",
            "Save to Source? If unticked, will be saved in WorkingDirectory/bdchecks.edited",
            FALSE
        ),
        footer = tagList(actionButton("ok", "OK"))
    )
    
    # Show modal if path not set, load data if set
    if (is.null(.GlobalEnv$.bdchecksLocation)) {
        showModal(modal)
    } else {
        withProgress(message = "Building bdchecks Admin App", {
            read_path <- .GlobalEnv$.bdchecksLocation
            save_origin <- .GlobalEnv$.saveToOrigin
            write_path <- paste0(getwd(), "/bdchecks_edited")
            
            if (save_origin) {
                write_path <- read_path
            }
            
            #-------- Source Data ------
            checks <-
                read_yaml(paste0(read_path, "/inst/extdata/data_check.yaml"))
            tests <-
                yaml_to_dataframe(path = paste0(read_path, "/inst/extdata/data_test.yaml"))
            scripts <- paste0(read_path, "/R")
        })
    }
    
    # when modal clicked, set path and reload session
    observeEvent(input$ok, {
        if (dir.exists(input$src_location)) {
            #-------- Source Data ------
            .GlobalEnv$.bdchecksLocation <- input$src_location
            .GlobalEnv$.saveToOrigin <- input$save_to_src
            
            session$reload()
        } else {
            showNotification("Incorrect location")
            return()
        }
    })
    
    observeEvent(input$setting, {
        showModal(modal)
    })
    
    
    #------ Side Bar Menus
    output$sideBar_menu_UI <- renderMenu({
        if (length(checks) != 0) {
            menus <- list()
            
            for (i in 1:length(checks)) {
                menus[[i]] <-
                    menuItem(
                        checks[[i]]$name,
                        tabName = checks[[i]]$name,
                        icon = icon("search", lib = "glyphicon")
                    )
            }
            
            return(sidebarMenu(id = "tabs", menus))
        }
    })
    
    # Main body
    output$tab <- renderUI({
        if (length(checks) != 0) {
            tabs <- list()
            elem_placeholder <- list()
            
            create_layer <- function(listElems, prefix) {
                n <- names(listElems)
                
                for (index in 1:length(listElems)) {
                    if (class(listElems[[index]]) == "list") {
                        elem_placeholder[[length(elem_placeholder) + 1]] <<-
                            h3(names(listElems)[[index]])
                        create_layer(listElems[[index]], paste0(prefix, "$", n[[index]]))
                        elem_placeholder[[length(elem_placeholder) + 1]] <<-
                            hr()
                        
                    } else {
                        id <- paste0(prefix, "$", n[[index]])
                        elem_placeholder[[length(elem_placeholder) + 1]] <<-
                            textInput(id,
                                      label = names(listElems)[[index]],
                                      value = listElems[[index]])
                    }
                }
                
                return(elem_placeholder)
            }
            
            names <- names(checks)
            for (i in 1:length(checks)) {
                elem_placeholder <- list()
                meta_input_fields <-
                    create_layer(checks[[i]], paste0("`", names[[i]], "`"))
                
                tabs[[i]] <-
                    tabItem(checks[[i]]$name,
                            fluidRow(column(
                                12,
                                h1(paste0(
                                    "Check ", i, ": ", checks[[i]]$name
                                ), class =
                                    "primaryHeader"),
                                
                                column(
                                    12,
                                    tabsetPanel(
                                        type = "tabs",
                                        id = paste0(checks[[i]]$name, "_table_tab"),
                                        tabPanel("Meta Data",
                                                 column(
                                                     12,
                                                     div(class = "secondaryHeaders", h3("Edit Meta Data")),
                                                     
                                                     tagList(meta_input_fields)
                                                 )),
                                        
                                        tabPanel(
                                            "Test Data",
                                            column(
                                                12,
                                                div(class = "secondaryHeaders", h3("Edit Test Data")),
                                                rHandsontableOutput(paste0(checks[[i]]$name, "_table")),
                                                
                                                div(class = "secondaryHeaders", h3("Run Manual Test")),
                                                column(3,
                                                       textInput(
                                                           paste0(checks[[i]]$name, "_test_val"),
                                                           label = "Value"
                                                       )),
                                                column(
                                                    3,
                                                    selectInput(
                                                        paste0(checks[[i]]$name, "_test_exp"),
                                                        label = "Expected",
                                                        c("Pass", "Fail")
                                                    ),
                                                    offset = 1
                                                ),
                                                column(2,
                                                       actionButton(
                                                           paste0(checks[[i]]$name, "_test_btn"), "Test!"
                                                       )),
                                                column(3,
                                                       textOutput(
                                                           paste0(checks[[i]]$name, "_test_result")
                                                       ))
                                            )
                                        ),
                                        
                                        tabPanel("R Code",
                                                 column(
                                                     12,
                                                     div(class = "secondaryHeaders", h3("Edit R Code")),
                                                     
                                                     textAreaInput(
                                                         paste0(checks[[i]]$name, "_rcode"),
                                                         label = "R Code",
                                                         value = paste(suppressWarnings(readLines(
                                                             paste0(scripts, "/dc_", checks[[i]]$name, ".R")
                                                         ))
                                                         , collapse = "\n")
                                                     )
                                                 ))
                                    )
                                )
                            )))
            }
            
            
            
            return(tagList(fluidRow(column(
                12,
                column(
                    7,
                    tags$div(tagList(tabs), class = "tab-content", id = "sideTabs")
                ),
                column(5,
                       div(
                           id = "yamlDiv",
                           fluidRow(
                               textAreaInput(
                                   "yaml",
                                   label = "YAML File",
                                   value = paste(as.yaml(checks), collapse = "\n")
                               )
                           )
                       ))
            ))))
        }
    })
    
    # give string for yaml text box
    output$textWithNewlines <- renderUI({
        if (!is.null(write_path)) {
            rawText <-
                readLines(paste0(write_path, "/inst/extdata/data_check.yaml"))
            
            splitText <-
                stringi::stri_split(str = rawText, regex = '\\n')
            
            # wrap a paragraph tag around each element in the list
            replacedText <- lapply(splitText, p)
            
            return(replacedText)
        }
    })
    
    
    
    getDTTests <- function() {
        elems <- reactiveValuesToList(input)
        tests[[checks[[1]]$name]]
    }
    
    # Code Testing Feature
    if (length(checks) != 0) {
        source(paste0(file.path(read_path, "R", "datacheck_helpers.R")))
        source(paste0(file.path(read_path, "R", "perform.R")))
        source(paste0(file.path(read_path, "R", "filter.R")))
        
        lapply(1:length(checks), function(index) {
            observeEvent(input[[paste0(checks[[index]]$name, "_test_btn")]], {
                withProgress(message = "Testing Custom Case", {
                    script_path <- paste0(write_path,
                                          "/R",
                                          "/dc_",
                                          checks[[index]]$name,
                                          ".R")
                    source(script_path)
                    
                    script <-
                        paste(readLines(script_path), collapse = "")
                    function_list <-
                        strsplit(script, "<- function", fixed = T)[[1]]
                    head_list <-
                        strsplit(function_list[1], " ", fixed = T)[[1]]
                    result <-
                        do.call(head_list[length(head_list)], list(input[[paste0(checks[[index]]$name, "_test_val")]]))
                    
                    out <- "Fail"
                    if (result == TRUE || result == "TRUE") {
                        out <- "Pass"
                    }
                    output[[paste0(checks[[index]]$name, "_test_result")]] <<-
                        renderText({
                            if (out == input[[paste0(checks[[index]]$name, "_test_exp")]]) {
                                "Test Passed!"
                            } else {
                                "Test Failed"
                            }
                        })
                })
            })
        })
    }
    
    # populate test table
    if (length(checks) != 0) {
        lapply(1:length(checks), function(index) {
            output[[paste0(checks[[index]]$name, "_table")]] <-
                renderRHandsontable({
                    if (is.null(tests[[checks[[index]]$name]])) {
                        rhandsontable(data.frame())
                    } else {
                        rhandsontable(tests[[checks[[index]]$name]])
                    }
                })
        })
    }
    
    # write test yaml file
    observe({
        if (length(checks) != 0) {
            lapply(1:length(checks), function(index) {
                if (!is.null(input[[paste0(checks[[index]]$name, "_table")]])) {
                    DF <-  hot_to_r(input[[paste0(checks[[index]]$name, "_table")]])
                    tests[[checks[[index]]$name]] <<- DF
                }
                
            })
            dataframe_to_yaml(tests, paste0(write_path, "/inst/extdata"))
        }
    })
    
    # Main yaml, R files writing
    observe({
        if (!is.null(write_path)) {
            elems <- reactiveValuesToList(input)
            
            for (index in 1:length(elems)) {
                nameOri <- names(elems[index])
                
                name <-
                    gsub("``",
                         "`",
                         gsub("$", "`$`", gsub("$", "`", nameOri), fixed = T),
                         fixed = T)
                
                if ((!is.null(nameOri)) && (length(elems) > 0)) {
                    if (nchar(elems[[index]]) > 0 && grepl("`DC_", name)) {
                        tryCatch({
                            eval(parse(
                                text = paste0(
                                    "checks$",
                                    name,
                                    " <<-",
                                    "'",
                                    elems[index],
                                    "'"
                                )
                            ))
                        },
                        error = function(cond) {
                            tryCatch({
                                eval(parse(
                                    text = paste0(
                                        "checks$",
                                        name,
                                        " <<-",
                                        '"',
                                        elems[index],
                                        '"'
                                    )
                                ))
                            },
                            error = function(cond) {
                                
                            })
                        })
                        
                    } else if (nchar(elems[[index]]) > 0 &&
                               grepl("_rcode", name)) {
                        dir.create(
                            file.path(paste0(
                                write_path, "/R"
                            )),
                            recursive = T,
                            showWarnings = F
                        )
                        
                        writeLines(elems[[index]],
                                   paste0(
                                       write_path,
                                       "/R",
                                       "/dc_",
                                       strsplit(name, "_rcode")[[1]][1],
                                       ".R"
                                   ))
                    }
                }
            }
            
            updateTextAreaInput(session, "yaml", value = paste(as.yaml(checks), collapse = "\n"))
            write_yaml(checks,
                       paste0(write_path, "/inst/extdata/data_check.yaml"))
        }
    })
    
    
    
    observe({
        input$tabs
        shinyjs::runjs(
            paste0(
                "yaml.selectionStart = yaml.value.indexOf('",
                input$tabs,
                "');yaml.selectionEnd = yaml.value.indexOf('",
                input$tabs,
                "');yaml.blur();yaml.focus();"
            )
        )
    })
    
})
