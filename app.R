if(interactive()){
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(tidyr)
  library(dashboardthemes)
  library(gt)
  library(PASWR2)
  library(tidyr)
  library(plotly)
  shinyApp(
    # UI------------------------------------------------------------------------
    ui <- dashboardPage(skin = "black",
                        dashboardHeader(),
                        dashboardSidebar(
                          collapsed = TRUE,
                          sidebarMenu(
                            id = "sidebar_menu",
                            menuItem("home", tabName = "dashboard"),
                            menuItem("How to fill tables with block data", 
                                     tabName = "dashboard2")
                          )
                        ),
                        dashboardBody(
                          shinyDashboardThemes(
                            theme = "blue_gradient"
                          ),
                          tabItem(tabName = 'dashboard',
                                  uiOutput("ui")),
                          tabItem(tabName = 'dashboard2',
                                  uiOutput('ui2'))
                        )
    ),
    # Server--------------------------------------------------------------------
    server = function(input, output, session) { 
      output$ui2 <- renderUI({
        if (input$sidebar_menu == "dashboard2") {
          fluidRow(
            tabBox(
              tabPanel("Data Apearance 1",
                       gt_output('appearance1')),
              tabPanel("Data Apearance 2",
                       gt_output('appearance2'))
            )
            ,
            box(
              status = "warning",
              DTOutput('fillable')  
            )
          )
        }
      })
      output$appearance1 <- render_gt({  # one way to form table
         TIREWEAR %>% spread(treat, wear) 
        })
      output$appearance2 <- render_gt(TIREWEAR) # other way
      
      output$fillable <- renderDT({   # table as it appears in fillable setting
        Blocks <-  paste("Block", 1:length(unique(TIREWEAR$block)))
        Factors <- paste("Factor", 1:length(unique(TIREWEAR$treat)))
        columns <- matrix(ncol= length(Factors), nrow = length(Blocks))
        for (i in 1:length(Factors)) {
          columns[, i] <-  unlist(subset(TIREWEAR, treat==unique(TIREWEAR$treat)[i])[1])
        }
        dt <- as.data.frame(cbind(1:4, columns))
        names(dt) <- c("Block", Factors)
        rownames(dt) <- Blocks
        dt
      })

      output$ui <- renderUI({
        if (input$sidebar_menu == "dashboard") {
          fluidRow(
            tabBox(
              tabPanel("K sample Example Data",
                       div(style = 'overflow-y:scroll;height:200px;',
                       gt_output('twoksample'))), 
              tabPanel("Output",
                       verbatimTextOutput('infotwok'),
                       actionLink("twok", "Custom analysis", icon = icon("check"))),
              tabPanel("About",
                       uiOutput('abouttwok'))
            ),
            tabBox(
              tabPanel("Regression Example Data", 
                       div(style = 'overflow-y:scroll;height:200px;',
                       gt_output('regex'))),
              tabPanel("Output",
                       verbatimTextOutput('inforeg'),
                       actionLink("reg", "Custom analysis", icon = icon("check"))),
              tabPanel("About",
                       uiOutput('aboutreg'))
            ),
            tabBox(
              tabPanel("Block Design Example Data", 
                       gt_output('blockex')),
              tabPanel("Output",
                       verbatimTextOutput('infoblock'),
                       actionLink("fac2", "Custom analysis", icon = icon("check"))),
              tabPanel("About",
                       uiOutput('aboutblock'))
            ),
            tabBox(
              tabPanel("Factorial Example Data", 
                       div(style = 'overflow-y:scroll;height:200px;',
                       gt_output('factorex'))),
              tabPanel("Output",
                       verbatimTextOutput('infofactorial'),
                       actionLink("block", "Custom analysis", icon = icon("check"))),
              tabPanel("About",
                       uiOutput('aboutfact')))
          )
        }
      })
      
      # Two to k-Samples Tables Box
      output$twoksample <- render_gt(TIRE[, 1:2])
      
      output$infotwok <- renderPrint({
        summary(aov(stopdist~tire, data=TIRE))
      })
      
      output$abouttwok <- renderUI({HTML(
                  "<ul><li>About design: Data involving two or more independent 
                    samples
                   </li><li> About example data: TIRE {PASWR2} </li> 
                    &#9675 stopdistances of tires with different thread patterns 
                  are compared between them</li></ul>")
      })
      
      observeEvent(input$twok,{     # custom analysis
        output$ui <- renderUI({fluidPage(
          column(2, numericInput("row1", "Number of rows", 2)),
          column(2, numericInput("col1", "Number of cols", 2)),
          DTOutput('table1'),
          verbatimTextOutput('anova1'),
          actionButton("do1", "ANOVA")
        )
        })
        
        # Fill empty table with custom data
        data <- reactiveValues()        
        data$df <- as.data.frame(matrix(nrow = 2))
        names(data$df)[1] <- "y"
        
        # Format table 
        observeEvent(input$row1,{   
          if (input$row1 > NROW(data$df)) { # more rows are desired in df
            diff <- input$row1 - NROW(data$df) # missing amount of rows 
            newr <- as.data.frame(matrix(nrow = diff, ncol =  NCOL(data$df))) # new rows
            names(data$df) <- names(newr) 
            data$df <- rbind(data$df, newr)  # bind rows to existing df
            names(data$df)[1] <- "y"
            names(data$df)[2:NCOL(data$df)] <- paste0("x", 2:NCOL(data$df)-1) 
          } else if (input$row1 < NROW(data$df)) { 
            data$df <- data$df[1:input$row1, ]   # reduce rows in df 
          }
        })
        
        observeEvent(input$col1,{         # now format columns
          if (input$col1 > NCOL(data$df)) { 
            newc <- as.data.frame(matrix(nrow = NROW(data$df), 
                                         ncol = input$col1 - NCOL(data$df)))
            data$df <- cbind(data$df, newc)
            names(data$df)[1] <- "y"
            names(data$df)[2:NCOL(data$df)] <- paste0("x", 2:NCOL(data$df) - 1)
          } else if (input$col1 < NCOL(data$df)) { 
            data$df <- data$df[, 1:input$col1]
          }
        })
        output$table1 <- renderDT(data$df, editable = "cell") 
        
        # Save table entries 
        observeEvent(input$table1_cell_edit,{ 
          entry <- input$table1_cell_edit
          data$df[entry$row, entry$col]  <- isolate(  
            DT::coerceValue((entry$value), data$df[entry$row, entry$col])
            )
        })
        
        # Do analysis
        observeEvent(input$do1, { 
          output$anova1 <- renderPrint({
            if (NCOL(data$df) == 2) {
              summary(lm(data$df[, 1] ~ (data$df[, 2:NCOL(data$df)])))
            } else {
              additive <- ""
              for (i in 2:length(names(data$df))) {
                if (i == NCOL(data$df)) {
                  additive <- paste0(additive, names(data$df)[i])
                } else {
                  additive <- paste0(additive, names(data$df)[i], sep = "+")
                }
              }
              summary(aov(as.formula(paste(paste(data$df[1], "~"), additive)), 
                          data=data$df))
            }
          })
        })
      })     
      
      # Regression Tables Box 
      output$regex <- render_gt(as.data.frame(cbind(dist=cars[, 2], speed=cars[, 1])))
      
      output$inforeg <- renderPrint({
        summary(lm(dist~speed, data=head(cars)))
      })
     
      output$aboutreg <- renderUI({HTML(
                      "<ul><li>About design: models one variable as a response to 
                               one or more predictor variables. </li><li> 
                      About exaple data: cars{datasets}  </li>
                      &#9675 the response is distance and the predictor is the 
                      speed</li></ul>")
      })
      
      observeEvent(input$reg,{
          output$ui <- renderUI({fluidPage(
          column(2, numericInput("rowreg", "Number of rows", 2)),
          column(2, numericInput("colreg", "Number of cols", 2)),
          DTOutput('tablereg'),
          actionButton("doreg", "ANOVA"),
          verbatimTextOutput('anovareg')
        )
        })
          
        data <- reactiveValues()
        data$df <- as.data.frame(matrix(nrow=2))
        names(data$df)[1] <- "y"
        
        observeEvent(input$rowreg,{
          if (input$rowreg > NROW(data$df)) {
            diff <- input$rowreg - NROW(data$df)
            newr <- as.data.frame(matrix(nrow = diff, ncol = NCOL(data$df)))
            names(data$df) <- names(newr) 
            data$df <- rbind(data$df, newr)  
            names(data$df)[1] <- "y"
            names(data$df)[2:NCOL(data$df)] <- paste0("x", 2:NCOL(data$df)-1)
          } else if (input$rowreg < NROW(data$df)) {
            data$df <- data$df[1:input$rowreg, ]
          }
        })
        
        observeEvent(input$colreg,{
          if (input$colreg > NCOL(data$df)) {
            newc <- as.data.frame(matrix(nrow = NROW(data$df), 
                                         ncol = input$colreg - NCOL(data$df)))
            data$df <- cbind(data$df, newc)
            names(data$df)[1] <- "y"
            names(data$df)[2:NCOL(data$df)] <- paste0("x", 2:NCOL(data$df)-1)
          } else if (input$colreg < NCOL(data$df)) {
            data$df <- data$df[, 1:input$colreg]
          }
        })
        output$tablereg <- renderDT(data$df, editable = "cell")
        
        observeEvent(input$tablereg_cell_edit,{
          entry <- input$tablereg_cell_edit
          data$df[entry$row, entry$col]  <- isolate(
            DT::coerceValue((entry$value), data$df[entry$row, entry$col]))
        })
        
        observeEvent(input$doreg, {
          output$anovareg <- renderPrint({
            if (NCOL(data$df)==2) { # simple linear regression
              summary(lm(data$df[, 1] ~ (data$df[, 2:NCOL(data$df)])))
            } else {               # multiple linear regression
              additive <- ""
              for (i in 2:length(names(data$df))) {
                if (i == NCOL(data$df)) {
                  additive <- paste0(additive, names(data$df)[i])
                } else {
                  additive <- paste0(additive, names(data$df)[i], sep = "+")
                }
              }
              summary(lm(formula(paste(paste(data$df[1], "~") , additive)), 
                         data=data$df))
            }
          })
        })
      })
      
      # Blocks Tables Box
      output$blockex <- render_gt(spread(TIREWEAR, treat, wear))
      
      output$infoblock <- renderPrint({
        summary(aov(wear~treat+block, data=TIREWEAR))
      })

      output$aboutblock <- renderUI({HTML(
              "<ul><li>About design: Two or more treatments are applied to units 
                    grouped depending on some characteristics which are not 
                    interest of study(blocks)</li><li>
                    About example data: TIREWEAR{PASWR2}  </li> 
                   &#9675 the blocks are the types of cars Car1, Car2, Car3, 
                   Car4 </li><br> &#9675 The tratment levels are A, B, C, D </li><br> 
                   &#9675 Cars tread loss is recorded in the wear variable </li></ul>")
      })
      
      observeEvent(input$fac2,{
        output$ui <- renderUI({fluidPage(
          column(2, numericInput("rowfac", "Number of rows", 2)),
          column(2, numericInput("colfac", "Number of cols", 3)),
          DTOutput('tablefac'),
          verbatimTextOutput('anovafac'),
          actionButton("dofac", "ANOVA")
        )
        })
        
        data <- reactiveValues()
        data$df <- as.data.frame(matrix(nrow = 2, ncol=3))
        names(data$df) <- paste("Treatment", 1:NCOL(data$df))
        data$df <- cbind(Block = 1:2, data$df)
        rownames(data$df)[1:NROW(data$df)] <- paste("Block", 1:NROW(data$df))
        
        observeEvent(input$rowfac,{
          if (input$rowfac > NROW(data$df)) {
            diff <- input$rowfac - NROW(data$df)
            newr <- as.data.frame(matrix(nrow = diff, ncol =  NCOL(data$df)))
            names(data$df) <- names(newr)
            data$df <- rbind(data$df, newr)  
            rownames(data$df)[2:NROW(data$df)] <- paste("Block", 2:NROW(data$df))
            names(data$df)[1] <- "Block"
            names(data$df)[2:NCOL(data$df)] <- paste("Treatment", 1:NCOL(data$df))
            data$df[1:NROW(data$df), 1] <- 1:NROW(data$df)
          } else if (input$rowfac<= NROW(data$df)) {
            data$df <- data$df[1:input$rowfac,]
          }
        })
        
        observeEvent(input$colfac,{
          if (input$colfac > NCOL(data$df)) {
            newc <- as.data.frame(matrix(nrow = NROW(data$df), 
                                         ncol = input$colfac - NCOL(data$df)))
            data$df <- cbind(data$df, newc)
            names(data$df)[1] <- "Block"
            names(data$df)[2:NCOL(data$df)] <- paste("Factor", 1:NCOL(data$df))
            data$df[1:NROW(data$df), 1] <- 1:NROW(data$df)
            rownames(data$df)[1] <- "Block 1"
            rownames(data$df)[2:NROW(data$df)] <- paste("Block", 2:NROW(data$df))
          } else if (input$colfac < NCOL(data$df)) {
            data$df <- data$df[, 1:input$colfac]
          }
        })
        
        output$tablefac <- renderDT(data$df, editable=list(target = "cell", 
                                                           disable = list(columns = 1)))
        
        observeEvent(input$tablefac_cell_edit,{
          entry <- input$tablefac_cell_edit
          entry$value <- ifelse(is.character(entry$value), as.character(entry$value), 
                                as.numeric(entry$value))
          data$df[entry$row, entry$col]  <- isolate(entry$value)
        })
        
        observeEvent(input$dofac, {
          output$anovafac <- renderPrint({
            aovdf <- gather(data$df, Block)
            aovdf <- cbind(data$df[, 1], aovdf)
            names(aovdf) <- c("Block", "Treatment", "Value")
            summary(aov(Value ~ Treatment + Block, data = aovdf))
          })
        })
      })
      
      # Factorial Box
      output$factorex <- render_gt(ToothGrowth)
      output$infofactorial <- renderPrint({
        summary(aov(len~supp*dose, data=ToothGrowth))
      })
      
      output$aboutfact <- renderUI({HTML(
              "<ul><li>About design: Two or more treatments are of interest. The 
                      treatments contain levels that may have interactive 
                      effects on the outcome in study </li><li>
                      About example data: TOOTHGROWTH{datasets}. Data about the effect 
                     of two factors in the teeth length in guinea pigs. </li> 
                    &#9675 Factor supp(supplement) having levels:
                            VC, and OJ</li><br> 
                    &#9675 Factor dose with levels: 0.5, 1.0 and 2.0.</li></ul>")
      })
      
      observeEvent(input$block,{
        output$ui <- renderUI({fluidPage(
          numericInput("rowfac", "Number of rows", 2),
          numericInput("colfac", "Number of cols", 2),
          DTOutput('tablefactorial'),
          verbatimTextOutput('anovafactorial'),
          actionButton("dofactorial", "ANOVA")
        )
        })
        
        data <- reactiveValues()
        data$df <- as.data.frame(matrix(nrow = 2))
        
        observeEvent(input$rowfac,{
          if (input$rowfac > NROW(data$df)) {
            diff <- input$rowfac - NROW(data$df)
            newr <- as.data.frame(matrix(nrow = diff, ncol =  NCOL(data$df)))
            names(data$df) <- names(newr) 
            data$df <- rbind(data$df, newr)  
            names(data$df)[1] <- "y"
            names(data$df)[2:NCOL(data$df)] <- paste0("Factor", 1:NCOL(data$df))
          } else if (input$rowfac < NROW(data$df)) {
            data$df <- data$df[1:input$rowfac, ]
          }
        })
        
        observeEvent(input$colfac,{
          if (input$colfac > NCOL(data$df)) {
            newc <- as.data.frame(matrix(nrow = NROW(data$df), 
                                         ncol =  input$colfac - NCOL(data$df)))
            data$df <- cbind(data$df, newc)
            names(data$df)[1] <- "y"
            names(data$df)[2:NCOL(data$df)] <- paste0("Factor", 1:NCOL(data$df))
          } else if (input$colfac < NCOL(data$df)) {
            data$df <- data$df[, 1:input$colfac]
          }
        })
        output$tablefactorial <- renderDT(data$df, editable = "cell")
        
        observeEvent(input$tablefactorial_cell_edit,{
          entry <- input$tablefactorial_cell_edit
          entry$value <- ifelse(is.character(entry$value), as.character(entry$value), 
                                as.numeric(entry$value))
          data$df[entry$row, entry$col]  <- isolate(((entry$value)))
        })
        
        observeEvent(input$dofactorial, {
          output$anovafactorial <- renderPrint({
            if (NCOL(data$df) == 2) {
              summary(aov(data$df[,1] ~ (data$df[,2])))
            } else {
              str <- ""
              for (i in 2:length(names(data$df))) {
                if (i == NCOL(data$df)) {
                  str <- paste0(str, names(data$df)[i])
                } else {
                  str <- (paste0(str, names(data$df)[i], sep = "*"))
                }
              }
              summary(aov(as.formula(paste(paste(data$df[1], "~"), str)), 
                          data=data$df))
            }
          })
        })
      })  
    }
    )
  }
