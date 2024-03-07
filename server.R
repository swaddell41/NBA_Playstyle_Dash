server <- function(input, output) {



library(googleCloudStorageR)
library(readr)

gcs_auth("/NBAdata/GCS_service_account_key/studious-vector-416020-f6ff74e5bdc1.json")

# Define the GCS URL of your CSV file
player_catch_shoot_url <- "gs://nba_dash_data/player_catch_shoot.csv"
player_drives_url <- "gs://nba_dash_data/player_drives.csv"
player_elbow_touch_url <- "gs://nba_dash_data/player_elbow_touch.csv"
player_paint_touch_url <- "gs://nba_dash_data/player_paint_touch.csv"
player_passing_url <- "gs://nba_dash_data/player_passing.csv"
player_post_touch_url <- "gs://nba_dash_data/player_post_touch.csv"
player_pull_up_url <- "gs://nba_dash_data/player_pull_up.csv"
player_shot_dashboard_url <- "gs://nba_dash_data/player_shot_dashboard.csv"
team_catch_shoot_url <- "gs://nba_dash_data/team_catch_shoot.csv"
team_drives_url <- "gs://nba_dash_data/team_drives.csv"
team_elbow_touch_url <- "gs://nba_dash_data/team_elbow_touch.csv"
team_info_url <- "gs://nba_dash_data/team_info.csv"
team_mapping_url <- "gs://nba_dash_data/team_mapping.csv"
team_paint_touch_url <- "gs://nba_dash_data/team_paint_touch.csv"
team_passing_url <- "gs://nba_dash_data/team_passing.csv"
team_post_touch_url <- "gs://nba_dash_data/team_post_touch.csv"
team_pull_up_url <- "gs://nba_dash_data/team_pull_up.csv"
team_shot_dashboard_url <- "gs://nba_dash_data/team_shot_dashboard.csv"
team_stats_per_game_url <- "gs://nba_dash_data/team_stats_per_game.csv"

# Use gcs_get_object to read the CSV file content
player_catch_shoot <- gcs_get_object(player_catch_shoot_url)
player_drives <- gcs_get_object(player_drives_url)
player_elbow_touch <- gcs_get_object(player_elbow_touch_url)
player_paint_touch <- gcs_get_object(player_paint_touch_url)
player_passing <- gcs_get_object(player_passing_url)
player_post_touch <- gcs_get_object(player_post_touch_url)
player_pull_up <- gcs_get_object(player_pull_up_url)
player_shot_dashboard <- gcs_get_object(player_shot_dashboard_url)
team_catch_shoot <- gcs_get_object(team_catch_shoot_url)
team_drives <- gcs_get_object(team_drives_url)
team_elbow_touch <- gcs_get_object(team_elbow_touch_url)
team_info <- gcs_get_object(team_info_url)
team_mapping <- gcs_get_object(team_mapping_url)
team_paint_touch <- gcs_get_object(team_paint_touch_url)
team_passing <- gcs_get_object(team_passing_url)
team_post_touch <- gcs_get_object(team_post_touch_url)
team_pull_up <- gcs_get_object(team_pull_up_url)
team_shot_dashboard <- gcs_get_object(team_shot_dashboard_url)
team_stats_per_game <- gcs_get_object(team_stats_per_game_url)


library(shiny)
library(plotly)

data_to_display <- reactive({
        dataset <- switch(input$dataSelect,
                          "player_drives" = player_drives,
                          "player_post_touch" = player_post_touch,
                          "player_elbow_touch" = player_elbow_touch,
                          "player_paint_touch" = player_paint_touch,
                          "player_catch_shoot" = player_catch_shoot,
                          "player_pull_up" = player_pull_up)
        dataset[dataset$TEAM_ABBREVIATION == input$teamSelect, ]
    })

  output$teamLogo <- renderUI({
    selected_team <- input$teamSelect
    selected_team_logo <- team_info$logo[team_info$abbreviation == selected_team]
    if (length(selected_team_logo) > 0 && !is.na(selected_team_logo)) {
        tags$img(src = selected_team_logo, height = "100px")
    }
})

    
    output$table <- DT::renderDataTable({
        datatable <- data_to_display()

        # Using datatable function from DT package
        DT::datatable(datatable, options = list(
            order = list(list(9, 'desc')),  # Assuming "DRIVES" is the first column
            pageLength = 25  # Sets the default number of rows to display
        ))
    })








 output$barChart <- renderPlot({
        if (input$dataSelect == "player_drives") {
            df <- data_to_display()
            filtered_df <- df[df$MIN > 10, ]  # Filter to include only players with > 10 MIN

            # Ensure color codes have a hash at the beginning
            team_info$color <- paste0("#", team_info$color)

            # Merge with team_info for color
            merged_df <- merge(filtered_df, team_info, by.x = "TEAM_ABBREVIATION", by.y = "abbreviation")

            # Reordering PLAYER_NAME
            merged_df$PLAYER_NAME <- factor(merged_df$PLAYER_NAME, 
                                            levels = merged_df$PLAYER_NAME[order(merged_df$DRIVE_PASSES_PCT, decreasing = TRUE)])

            ggplot(merged_df, aes(x = PLAYER_NAME, y = DRIVE_PASSES_PCT, fill = color)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = round(DRIVE_PASSES_PCT, 2)), vjust = -0.5) +  # Add text on top of bars
                scale_fill_identity() +  # Use the actual color values in 'color' column
                theme_minimal() +
                labs(x = "Player", y = "Drive Passes Percentage") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels
                      legend.position = "none")  # Remove legend
        }
    })

output$postTouchPlot <- renderPlotly({
    if (input$dataSelect == "player_post_touch") {
        # Merge datasets directly, similar to drivePlot
        merged_df <- merge(player_post_touch, player_passing, by = "PLAYER_NAME")

        # Remove players with zero or NA post touches and less than 10 minutes per game
        merged_df <- merged_df[merged_df$POST_TOUCHES >= .2 & merged_df$MIN.x >= 10, ]

        # Replace NAs or zeros in relevant columns
        merged_df$AST[is.na(merged_df$AST) | merged_df$AST == 0] <- 1
        merged_df$AST_POINTS_CREATED[is.na(merged_df$AST_POINTS_CREATED)] <- 0

        # Calculate metrics
        merged_df$PostTouchPct <- merged_df$POST_TOUCHES / merged_df$TOUCHES
        merged_df$Efficiency <- (merged_df$POST_TOUCH_PTS + (merged_df$POST_TOUCH_AST * (merged_df$AST_POINTS_CREATED / merged_df$AST))) / merged_df$POST_TOUCHES

        # Prepend '#' to the alternate color code for the selected team
        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])

        # Filter selected team players with at least 10 minutes per game
        selected_team_players_filtered <- merged_df[merged_df$TEAM_ABBREVIATION.x == input$teamSelect & merged_df$MIN.x >= 10, ]

        p <- ggplot(merged_df, aes(x = PostTouchPct, y = Efficiency, text = PLAYER_NAME)) +
            geom_point(color = "grey") +
            geom_point(data = selected_team_players_filtered, color = selected_team_color, size = 3)+
            theme_minimal() +
            labs(x = "Percentage of Total Touches That Are Post Touches", y = "Post Touch Efficiency")


        ggplotly(p, tooltip = "text")
            # Shadow layer
#geom_label(data = selected_team_players_filtered, aes(x = PostTouchPct + 0.0001, y = Efficiency - 0.01, label = PLAYER_NAME), vjust = -.5, color = "black", fill = "grey80", label.padding = unit(0.5, "lines"), label.r = unit(0.15, "lines"), label.size = 0) +

# Main label layer
#geom_label(data = selected_team_players_filtered, aes(x = PostTouchPct, y = Efficiency, label = PLAYER_NAME), vjust = -.5, color = "black", fill = "white", label.padding = unit(0.5, "lines"), label.r = unit(0.15, "lines"), label.size = 0) +
    }
})

output$elbowTouchPlot <- renderPlotly({
    if (input$dataSelect == "player_elbow_touch") {
        merged_df <- merge(player_elbow_touch, player_passing, by = "PLAYER_NAME")
        merged_df <- merged_df[merged_df$ELBOW_TOUCHES >= .2 & merged_df$MIN.x >= 10, ]
        merged_df$AST[is.na(merged_df$AST) | merged_df$AST == 0] <- 1
        merged_df$AST_POINTS_CREATED[is.na(merged_df$AST_POINTS_CREATED)] <- 0
        merged_df$ElbowTouchPct <- merged_df$ELBOW_TOUCHES / merged_df$TOUCHES
        merged_df$Efficiency <- (merged_df$ELBOW_TOUCH_PTS + (merged_df$ELBOW_TOUCH_AST * (merged_df$AST_POINTS_CREATED / merged_df$AST))) / merged_df$ELBOW_TOUCHES

        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])
        selected_team_players_filtered <- merged_df[merged_df$TEAM_ABBREVIATION.x == input$teamSelect & merged_df$MIN.x >= 10, ]
        p <- ggplot(merged_df, aes(x = ElbowTouchPct, y = Efficiency, text = PLAYER_NAME, color = TeamColor)) +
            geom_point(color = "grey") +
            geom_point(data = selected_team_players_filtered, color = selected_team_color, size = 3) +
            theme_minimal() +
            labs(x = "Percentage of Total Touches That Are Elbow Touches", y = "Elbow Touch Efficiency")
        ggplotly(p, tooltip = "text")
    }
})


output$paintTouchPlot <- renderPlotly({
    if (input$dataSelect == "player_paint_touch") {
        # Merge datasets and calculate metrics
        merged_df <- merge(player_paint_touch, player_passing, by = "PLAYER_NAME")
        merged_df <- merged_df[merged_df$PAINT_TOUCHES >= .2 & merged_df$MIN.x >= 10, ]
        merged_df$AST[is.na(merged_df$AST) | merged_df$AST == 0] <- 1
        merged_df$AST_POINTS_CREATED[is.na(merged_df$AST_POINTS_CREATED)] <- 0
        merged_df$PaintTouchPct <- merged_df$PAINT_TOUCHES / merged_df$TOUCHES
        merged_df$Efficiency <- (merged_df$PAINT_TOUCH_PTS + (merged_df$PAINT_TOUCH_AST * (merged_df$AST_POINTS_CREATED / merged_df$AST))) / merged_df$PAINT_TOUCHES

        # Visualization
        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])
        selected_team_players_filtered <- merged_df[merged_df$TEAM_ABBREVIATION.x == input$teamSelect & merged_df$MIN.x >= 10, ]
        p <- ggplot(merged_df, aes(x = PaintTouchPct, y = Efficiency, text = PLAYER_NAME)) +
            geom_point(color = "grey") +
            geom_point(data = selected_team_players_filtered, color = selected_team_color, size = 3) +
            theme_minimal() +
            labs(x = "Percentage of Total Touches That Are Paint Touches", y = "Paint Touch Efficiency")
        ggplotly(p, tooltip = "text")
    }
})



output$drivePlot <- renderPlotly({
    if (input$dataSelect == "player_drives") {
        # Merge datasets directly, similar to postTouchPlot
        merged_df <- merge(player_drives, player_passing, by = "PLAYER_NAME")
        merged_df <- merge(merged_df, player_elbow_touch, by = "PLAYER_NAME")

        # Remove players with zero or NA drives and less than 10 minutes per game
        merged_df <- merged_df[merged_df$DRIVES >= .2 & merged_df$MIN.x >= 10, ]

        # Replace NAs or zeros in relevant columns
        merged_df$AST[is.na(merged_df$AST) | merged_df$AST == 0] <- 1
        merged_df$AST_POINTS_CREATED[is.na(merged_df$AST_POINTS_CREATED)] <- 0

        # Calculate metrics
        merged_df$DrivePct <- merged_df$DRIVES / merged_df$TOUCHES
        merged_df$Efficiency <- (merged_df$DRIVE_PTS + (merged_df$DRIVE_AST * (merged_df$AST_POINTS_CREATED / merged_df$AST))) / merged_df$DRIVES

        # Prepend '#' to the alternate color code for the selected team
        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])

        # Filter selected team players with at least 10 minutes per game
        selected_team_players_filtered <- merged_df[merged_df$TEAM_ABBREVIATION == input$teamSelect & merged_df$MIN >= 10, ]


p <- plot_ly(data = merged_df, x = ~DrivePct, y = ~Efficiency, text = ~PLAYER_NAME, type = 'scatter', mode = 'markers',
             marker = list(color = 'grey', size = 8)) %>%  # Adjusted size for all data points
  add_trace(data = selected_team_players_filtered, x = ~DrivePct, y = ~Efficiency, text = ~PLAYER_NAME, type = 'scatter', mode = 'markers',
            marker = list(color = selected_team_color, size = 8,  # Adjusted size to match original intent
                          line = list(color = 'black', width = 1))) %>%  # Adjusted outline width for visibility
  layout(title = 'Drives per Touch vs. Drive Points Efficiency',
         xaxis = list(title = 'Drives per Touch'),
         yaxis = list(title = 'Drive Points Efficiency'),
         hovermode = 'closest')

p

    }
})

output$playerCatchShootPlot <- renderPlotly({
    if (input$dataSelect == "player_catch_shoot") {
        # Merging datasets
        merged_df <- merge(player_catch_shoot, player_shot_dashboard, by = "PLAYER_NAME")

        # Filtering out players who play less than 10 minutes per game
        merged_df <- merged_df[merged_df$MIN >= 10, ]

        # Calculating the percentage of field goals that are catch and shoot
        merged_df$CatchShootFgPct <- merged_df$CATCH_SHOOT_FGA / merged_df$FGA

        # Setting the y-axis as catch and shoot effective field goal percentage
        merged_df$CatchShootEfgPct <- merged_df$CATCH_SHOOT_EFG_PCT

        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])

        # Creating the plot
        p <- ggplot(merged_df, aes(x = CatchShootFgPct, y = CatchShootEfgPct, text = PLAYER_NAME)) +
            geom_point(aes(color = (TEAM_ABBREVIATION == input$teamSelect)), size = 3) +
            scale_color_manual(values = c("FALSE" = "grey", "TRUE" = selected_team_color)) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "Percent of FG that are Catch and Shoot", y = "Catch and Shoot eFG%")

        ggplotly(p, tooltip = "text")
    }
})

output$playerPullUpPlot <- renderPlotly({
    if (input$dataSelect == "player_pull_up") {
        # Merging player_pull_up data with player-specific shot data
        merged_df <- merge(player_pull_up, player_shot_dashboard, by = "PLAYER_NAME")

        # Ensure data for players with significant playing time is considered
        merged_df <- merged_df[merged_df$MIN >= 10, ]

        # Calculate the metrics for the axes
        merged_df$PullUpFgaPerFga <- merged_df$PULL_UP_FGA / merged_df$FGA
        merged_df$PullUpEfgPct <- merged_df$PULL_UP_EFG_PCT

        # Fetch the team's alternate color for the selected team's players
        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])

        # Create the plot with color differentiation for the selected team's players
        p <- ggplot(merged_df, aes(x = PullUpFgaPerFga, y = PullUpEfgPct, text = PLAYER_NAME)) +
            geom_point(aes(color = (TEAM_ABBREVIATION == input$teamSelect)), size = 3) +
            scale_color_manual(values = c("FALSE" = "grey", "TRUE" = selected_team_color)) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "Pull Up FGA per Total FGA", y = "Pull Up EFG%")

        ggplotly(p, tooltip = "text")
    }
})




output$teamDrivePlot <- renderPlotly({
    if (input$dataSelect == "player_drives") {
        merged_df <- merge(team_drives, team_paint_touch, by = "TEAM_ABBREVIATION")
        merged_df <- merge(merged_df, team_passing, by = "TEAM_ABBREVIATION")

        merged_df$DrivePerTouch <- merged_df$DRIVES / merged_df$TOUCHES
        merged_df$PointsCreatedByDrives <- (merged_df$DRIVE_PTS + (merged_df$DRIVE_AST * (merged_df$AST_POINTS_CREATED / merged_df$AST))) / merged_df$DRIVES

        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])

        p <- ggplot(merged_df, aes(x = DrivePerTouch, y = PointsCreatedByDrives, text = TEAM_ABBREVIATION)) +
            geom_point(aes(color = (TEAM_ABBREVIATION == input$teamSelect)), size = 3) +
            scale_color_manual(values = c("FALSE" = "grey", "TRUE" = selected_team_color)) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "Drive per Touch", y = "Points Created Per Drive")

        ggplotly(p, tooltip = "text")
    }
})

output$teamCatchShootPlot <- renderPlotly({
    if (input$dataSelect == "player_catch_shoot") {
        # Merging datasets
        merged_df <- merge(team_catch_shoot, team_shot_dashboard, by = "TEAM_ABBREVIATION")

        # Calculating the percentage of field goals that are catch and shoot
        merged_df$CatchShootFgPct <- merged_df$CATCH_SHOOT_FGA / merged_df$FGA

        # Setting the y-axis as catch and shoot effective field goal percentage
        merged_df$CatchShootEfgPct <- merged_df$CATCH_SHOOT_EFG_PCT

        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])

        # Creating the plot
        p <- ggplot(merged_df, aes(x = CatchShootFgPct, y = CatchShootEfgPct, text = TEAM_ABBREVIATION)) +
            geom_point(aes(color = (TEAM_ABBREVIATION == input$teamSelect)), size = 3) +
            scale_color_manual(values = c("FALSE" = "grey", "TRUE" = selected_team_color)) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "Percent of FG that are Catch and Shoot", y = "Catch and Shoot eFG%")

        ggplotly(p, tooltip = "text")
    }
})

output$teamPostTouchPlot <- renderPlotly({
    if (input$dataSelect == "player_post_touch") {
        # Assuming team_post_touch and other necessary datasets are already loaded
        merged_df <- merge(team_post_touch, team_passing, by = "TEAM_ABBREVIATION")

        # Calculate metrics for the axes
        merged_df$PostTouchPct <- merged_df$POST_TOUCHES / merged_df$TOUCHES
        merged_df$Efficiency <- (merged_df$POST_TOUCH_PTS + (merged_df$POST_TOUCH_AST * (merged_df$AST_POINTS_CREATED / merged_df$AST))) / merged_df$POST_TOUCHES

        # Fetch the team's alternate color
        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])

        # Create the plot
        p <- ggplot(merged_df, aes(x = PostTouchPct, y = Efficiency, text = TEAM_ABBREVIATION)) +
            geom_point(aes(color = (TEAM_ABBREVIATION == input$teamSelect)), size = 3) +
            scale_color_manual(values = c("FALSE" = "grey", "TRUE" = selected_team_color)) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "Percentage of Total Touches That Are Post Touches", y = "Post Touch Efficiency")

        ggplotly(p, tooltip = "text")
    }
})


output$teamPullUpPlot <- renderPlotly({
    if (input$dataSelect == "player_pull_up") {
        # Assuming team_pull_up and team_shot_dashboard are already loaded and contain the required data
        merged_df <- merge(team_pull_up, team_shot_dashboard, by = "TEAM_ABBREVIATION")

        # Calculate the metrics for the axes
        merged_df$PullUpFgaPerFga <- merged_df$PULL_UP_FGA / merged_df$FGA
        merged_df$PullUpEfgPct <- merged_df$PULL_UP_EFG_PCT

        # Fetch the team's alternate color
        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])

        # Create the plot
        p <- ggplot(merged_df, aes(x = PullUpFgaPerFga, y = PullUpEfgPct, text = TEAM_ABBREVIATION)) +
            geom_point(aes(color = (TEAM_ABBREVIATION == input$teamSelect)), size = 3) +
            scale_color_manual(values = c("FALSE" = "grey", "TRUE" = selected_team_color)) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "Pull Up FGA per Total FGA", y = "Pull Up EFG%")

        ggplotly(p, tooltip = "text")
    }
})

output$teamElbowTouchPlot <- renderPlotly({
    if (input$dataSelect == "player_elbow_touch") {
        merged_df <- merge(team_elbow_touch, team_passing, by = "TEAM_ABBREVIATION")

        merged_df$ElbowTouchPct <- merged_df$ELBOW_TOUCHES / merged_df$TOUCHES
        merged_df$Efficiency <- (merged_df$ELBOW_TOUCH_PTS + (merged_df$ELBOW_TOUCH_AST * (merged_df$AST_POINTS_CREATED / merged_df$AST))) / merged_df$ELBOW_TOUCHES

        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])

        p <- ggplot(merged_df, aes(x = ElbowTouchPct, y = Efficiency, text = TEAM_ABBREVIATION)) +
            geom_point(aes(color = (TEAM_ABBREVIATION == input$teamSelect)), size = 3) +
            scale_color_manual(values = c("FALSE" = "grey", "TRUE" = selected_team_color)) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "Percentage of Total Touches That Are Elbow Touches", y = "Elbow Touch Efficiency")

        ggplotly(p, tooltip = "text")
    }
})


output$teamPaintTouchPlot <- renderPlotly({
    if (input$dataSelect == "player_paint_touch") {
        merged_df <- merge(team_paint_touch, team_passing, by = "TEAM_ABBREVIATION")

        merged_df$PaintTouchPct <- merged_df$PAINT_TOUCHES / merged_df$TOUCHES
        merged_df$Efficiency <- (merged_df$PAINT_TOUCH_PTS + (merged_df$PAINT_TOUCH_AST * (merged_df$AST_POINTS_CREATED / merged_df$AST))) / merged_df$PAINT_TOUCHES

        selected_team_color <- paste0("#", team_info$alternate_color[team_info$abbreviation == input$teamSelect])

        p <- ggplot(merged_df, aes(x = PaintTouchPct, y = Efficiency, text = TEAM_ABBREVIATION)) +
            geom_point(aes(color = (TEAM_ABBREVIATION == input$teamSelect)), size = 3) +
            scale_color_manual(values = c("FALSE" = "grey", "TRUE" = selected_team_color)) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(x = "Percentage of Total Touches That Are Paint Touches", y = "Paint Touch Efficiency")

        ggplotly(p, tooltip = "text")
    }
})




output$teamComparisonChart <- renderPlot({
    selected_team_catch_shoot <- team_catch_shoot[team_catch_shoot$TEAM_ABBREVIATION == input$teamSelect, ]
    selected_team_drives <- team_drives[team_drives$TEAM_ABBREVIATION == input$teamSelect, ]
    selected_team_pull_up <- team_pull_up[team_pull_up$TEAM_ABBREVIATION == input$teamSelect, ]

    plot_data <- data.frame(
        Category = c("Catch and Shoot", "Drives", "Pull Up"),
        Points = c(selected_team_catch_shoot$CATCH_SHOOT_PTS, selected_team_drives$DRIVE_PTS, selected_team_pull_up$PULL_UP_PTS)
    )

    # Fetch the team's alternate color
    team_color <- paste0("#", team_info$color[team_info$abbreviation == input$teamSelect])
    # Assuming 'team_color' fetches a color like "#123456"

    # Map the selected dataset to a category name
    dataset_to_category <- c("player_catch_shoot" = "Catch and Shoot", "player_drives" = "Drives", "player_pull_up" = "Pull Up")
    selected_category <- dataset_to_category[input$dataSelect]

    # Set the color for each category
    category_colors <- ifelse(plot_data$Category == selected_category, team_color, "#808080")

    league_avg_catch_shoot <- mean(team_catch_shoot$CATCH_SHOOT_PTS, na.rm = TRUE)
    league_avg_drives <- mean(team_drives$DRIVE_PTS, na.rm = TRUE)
    league_avg_pull_up <- mean(team_pull_up$PULL_UP_PTS, na.rm = TRUE)

    y_axis_min <- 0  # Replace with your chosen minimum value
    y_axis_max <- 40  # Replace with your chosen maximum value

    ggplot(plot_data, aes(x = Category, y = Points, fill = Category)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = setNames(category_colors, plot_data$Category)) +
        ylim(y_axis_min, y_axis_max) +
        theme_minimal() +
        labs(title = "Team Points Comparison", x = "", y = "Points")+
        theme(legend.position = "none") +
        geom_segment(aes(x = .55, xend = 1.45, y = league_avg_catch_shoot, yend = league_avg_catch_shoot), colour = "black", linetype = "dashed") +  # For Catch and Shoot
        geom_segment(aes(x = 1.55, xend = 2.45, y = league_avg_drives, yend = league_avg_drives), colour = "black", linetype = "dashed") +  # For Drives
        geom_segment(aes(x = 2.55, xend = 3.45, y = league_avg_pull_up, yend = league_avg_pull_up), colour = "black", linetype = "dashed")   # For Pull Up
})


output$teamComparisonChart_pct <- renderPlot({
    # Select data for the chosen team
    selected_team_catch_shoot <- team_catch_shoot[team_catch_shoot$TEAM_ABBREVIATION == input$teamSelect, ]
    selected_team_drives <- team_drives[team_drives$TEAM_ABBREVIATION == input$teamSelect, ]
    selected_team_pull_up <- team_pull_up[team_pull_up$TEAM_ABBREVIATION == input$teamSelect, ]

    # Retrieve total points for the selected team using TEAM_ABBREVIATION directly
    total_points <- team_stats_per_game$PTS[team_stats_per_game$TEAM_ABBREVIATION.y == input$teamSelect]

    # Assuming you've already calculated PercentageOfTotal for each dataset as part of your data preparation
    # If not, you should move these calculations outside of renderPlot to where your data is processed/prepared

    # Compute league averages of these percentages
    league_avg_catch_shoot_pct <- mean(team_catch_shoot$PercentageOfTotal, na.rm = TRUE)
    league_avg_drives_pct <- mean(team_drives$PercentageOfTotal, na.rm = TRUE)
    league_avg_pull_up_pct <- mean(team_pull_up$PercentageOfTotal, na.rm = TRUE)

    # Prepare data for plotting
    points_percentage <- c(selected_team_catch_shoot$PercentageOfTotal[1], selected_team_drives$PercentageOfTotal[1], selected_team_pull_up$PercentageOfTotal[1])

    plot_data <- data.frame(
        Category = c("Catch and Shoot", "Drives", "Pull Up"),
        Points = points_percentage
    )

    dataset_to_category <- c("player_catch_shoot" = "Catch and Shoot", "player_drives" = "Drives", "player_pull_up" = "Pull Up")
    selected_category <- dataset_to_category[input$dataSelect]

    y_axis_min <- 0  # Replace with your chosen minimum value
    y_axis_max <- 35  # Replace with your chosen maximum value

    # Existing plotting code with modifications
    team_color <- paste0("#", team_info$color[team_info$abbreviation == input$teamSelect])
    category_colors <- ifelse(plot_data$Category == dataset_to_category[input$dataSelect], team_color, "#808080")

    ggplot(plot_data, aes(x = Category, y = Points, fill = Category)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = setNames(category_colors, plot_data$Category)) +
        ylim(y_axis_min, y_axis_max) +
        theme_minimal() +
        labs(title = "Team Points Comparison", x = "", y = "Percentage of Total Points") +
        theme(legend.position = "none") +
        geom_segment(aes(x = .55, xend = 1.45, y = league_avg_catch_shoot_pct, yend = league_avg_catch_shoot_pct), colour = "black", linetype = "dashed") +
        geom_segment(aes(x = 1.55, xend = 2.45, y = league_avg_drives_pct, yend = league_avg_drives_pct), colour = "black", linetype = "dashed") +
        geom_segment(aes(x = 2.55, xend = 3.45, y = league_avg_pull_up_pct, yend = league_avg_pull_up_pct), colour = "black", linetype = "dashed")
})



output$touchTypeChart <- renderPlot({
    selected_team_elbow_touch <- team_elbow_touch[team_elbow_touch$TEAM_ABBREVIATION == input$teamSelect, ]
    selected_team_post_touch <- team_post_touch[team_post_touch$TEAM_ABBREVIATION == input$teamSelect, ]
    selected_team_paint_touch <- team_paint_touch[team_paint_touch$TEAM_ABBREVIATION == input$teamSelect, ]

    # Data frame for plotting
    touch_data <- data.frame(
        TouchType = c("Elbow Touches", "Post Touches", "Paint Touches"),
        Counts = c(selected_team_elbow_touch$ELBOW_TOUCHES, selected_team_post_touch$POST_TOUCHES, selected_team_paint_touch$PAINT_TOUCHES),
        IsSelected = c(input$dataSelect == "player_elbow_touch", input$dataSelect == "player_post_touch", input$dataSelect == "player_paint_touch")
    )

    selected_team_color <- paste0("#", team_info$color[team_info$abbreviation == input$teamSelect])

    league_avg_elbow_touch <- mean(team_elbow_touch$ELBOW_TOUCHES, na.rm = TRUE)
    league_avg_post_touch <- mean(team_post_touch$POST_TOUCHES, na.rm = TRUE)
    league_avg_paint_touch <- mean(team_paint_touch$PAINT_TOUCHES, na.rm = TRUE)

    y_axis_min <- 0  # Replace with your chosen minimum value
    y_axis_max <- 30  # Replace with your chosen maximum value

    ggplot(touch_data, aes(x = TouchType, y = Counts, fill = IsSelected)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("TRUE" = selected_team_color, "FALSE" = "#808080")) +
        ylim(y_axis_min, y_axis_max) +
        theme_minimal() +
        labs(title = "Team Touch Types Comparison", x = "", y = "Count") +
        theme(legend.position = "none") +
        geom_segment(aes(x = .55, xend = 1.45, y = league_avg_elbow_touch, yend = league_avg_elbow_touch), colour = "black", linetype = "dashed") +  # For elbow touch
        geom_segment(aes(x = 1.55, xend = 2.45, y = league_avg_paint_touch, yend = league_avg_paint_touch), colour = "black", linetype = "dashed") +  # For paint touch
        geom_segment(aes(x = 2.55, xend = 3.45, y = league_avg_post_touch, yend = league_avg_post_touch), colour = "black", linetype = "dashed") # For paint touch
})

output$touchTypePercentageChart <- renderPlot({
    selected_team_elbow_touch <- team_elbow_touch[team_elbow_touch$TEAM_ABBREVIATION == input$teamSelect, ]
    selected_team_post_touch <- team_post_touch[team_post_touch$TEAM_ABBREVIATION == input$teamSelect, ]
    selected_team_paint_touch <- team_paint_touch[team_paint_touch$TEAM_ABBREVIATION == input$teamSelect, ]

    # Assuming TOUCHES are the same across datasets, take it from one dataset
    total_touches <- selected_team_elbow_touch$TOUCHES[1] # or whichever is appropriate

    # Data frame for plotting percentages
    touch_percentage_data <- data.frame(
        TouchType = c("Elbow Touches", "Post Touches", "Paint Touches"),
        Percentage = c(
            selected_team_elbow_touch$ELBOW_TOUCHES / total_touches * 100,
            selected_team_post_touch$POST_TOUCHES / total_touches * 100,
            selected_team_paint_touch$PAINT_TOUCHES / total_touches * 100
        )
    )

    selected_team_color <- paste0("#", team_info$color[team_info$abbreviation == input$teamSelect])

    league_avg_elbow_touch_pct <- mean(team_elbow_touch$ELBOW_TOUCHES / team_elbow_touch$TOUCHES * 100, na.rm = TRUE)
    league_avg_post_touch_pct <- mean(team_post_touch$POST_TOUCHES / team_post_touch$TOUCHES * 100, na.rm = TRUE)
    league_avg_paint_touch_pct <- mean(team_paint_touch$PAINT_TOUCHES / team_paint_touch$TOUCHES * 100, na.rm = TRUE)

    y_axis_min <- 0
    y_axis_max <- 10

touch_percentage_data$IsSelected <- c(input$dataSelect == "player_elbow_touch", 
                                          input$dataSelect == "player_post_touch", 
                                          input$dataSelect == "player_paint_touch")

    # Use the IsSelected column to conditionally set the bar colors
    ggplot(touch_percentage_data, aes(x = TouchType, y = Percentage)) +
        geom_bar(stat = "identity", aes(fill = IsSelected)) +
        scale_fill_manual(values = c("TRUE" = selected_team_color, "FALSE" = "#808080")) +
        ylim(y_axis_min, y_axis_max) +
        theme_minimal() +
        labs(title = "Team Touch Types Comparison (%)", x = "", y = "Percentage of Total Touches") +
        theme(legend.position = "none") +
        geom_segment(aes(x = .55, xend = 1.45, y = league_avg_elbow_touch_pct, yend = league_avg_elbow_touch_pct), colour = "black", linetype = "dashed") +
        geom_segment(aes(x = 1.55, xend = 2.45, y = league_avg_paint_touch_pct, yend = league_avg_paint_touch_pct), colour = "black", linetype = "dashed") +
        geom_segment(aes(x = 2.55, xend = 3.45, y = league_avg_post_touch_pct, yend = league_avg_post_touch_pct), colour = "black", linetype = "dashed")
})




}
