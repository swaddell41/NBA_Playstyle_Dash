library(shiny)
library(plotly)


ui <- fluidPage(
    selectInput("dataSelect", "Choose a dataset:", 
                choices = c("Drives" = "player_drives", 
                            "Post Touch" = "player_post_touch", 
                            "Elbow Touch" = "player_elbow_touch",
                            "Paint Touch" = "player_paint_touch",
                            "Catch and Shoot" = "player_catch_shoot",
                            "Pull Up Shot" = "player_pull_up")),
    
    selectInput("teamSelect", "Choose a team:", choices = sort(unique(player_drives$TEAM_ABBREVIATION))),
    
    uiOutput("teamLogo"),

    checkboxInput("toggleCharts", "Toggle Chart Sets", value = FALSE),
    
    # Conditional panels to show/hide based on the toggle's state
    conditionalPanel(
        condition = "input.toggleCharts == false",
        fluidRow(
            column(6, plotOutput("teamComparisonChart_pct")),
            column(6, plotOutput("touchTypePercentageChart"))
        )
    ),
    conditionalPanel(
        condition = "input.toggleCharts == true",
        fluidRow(
            column(6, plotOutput("teamComparisonChart")),
            column(6, plotOutput("touchTypeChart"))
        )
    ),



    # Conditional panels for specific dataset plots
    conditionalPanel(
        condition = "input.dataSelect == 'player_drives'",
        plotlyOutput("teamDrivePlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_catch_shoot'",
        plotlyOutput("teamCatchShootPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_pull_up'",
        plotlyOutput("teamPullUpPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_post_touch'",
        plotlyOutput("teamPostTouchPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_elbow_touch'",
        plotlyOutput("teamElbowTouchPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_paint_touch'",
        plotlyOutput("teamPaintTouchPlot")
    ),

    # Data table
    DT::dataTableOutput("table"),

    # Player-specific plots
    conditionalPanel(
        condition = "input.dataSelect == 'player_drives'",
        plotlyOutput("drivePlot"),
        plotOutput("barChart")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_post_touch'",
        plotlyOutput("postTouchPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_elbow_touch'",
        plotlyOutput("elbowTouchPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_paint_touch'",
        plotlyOutput("paintTouchPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_catch_shoot'",
        plotlyOutput("playerCatchShootPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_pull_up'",
        plotlyOutput("playerPullUpPlot")
    )
)
