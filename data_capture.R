library(hoopR)

player_drives <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "Drives")
player_drives <- player_drives[[1]]

player_post_touch <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "PostTouch")
player_post_touch <- player_post_touch[[1]]

player_elbow_touch <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "ElbowTouch")
player_elbow_touch <- player_elbow_touch[[1]]

player_paint_touch <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "PaintTouch")
player_paint_touch <- player_paint_touch[[1]]

player_catch_shoot <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "CatchShoot")
player_catch_shoot <- player_catch_shoot[[1]]

player_pull_up <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "PullUpShot")
player_pull_up <- player_pull_up[[1]]

player_passing <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "Passing")
player_passing <- player_passing[[1]]

player_shot_dashboard <- nba_leaguedashplayerptshot(per_mode = "PerGame")
player_shot_dashboard <- player_shot_dashboard[[1]]


team_drives <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "Drives", player_or_team="Team")
team_drives <- team_drives[[1]]

team_post_touch <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "PostTouch", player_or_team="Team")
team_post_touch <- team_post_touch[[1]]

team_elbow_touch <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "ElbowTouch", player_or_team="Team")
team_elbow_touch <- team_elbow_touch[[1]]

team_paint_touch <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "PaintTouch", player_or_team="Team")
team_paint_touch <- team_paint_touch[[1]]

team_catch_shoot <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "CatchShoot", player_or_team="Team")
team_catch_shoot <- team_catch_shoot[[1]]

team_pull_up <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "PullUpShot", player_or_team="Team")
team_pull_up <- team_pull_up[[1]]

team_passing <- nba_leaguedashptstats(season = "2023-24", pt_measure_type = "Passing", player_or_team="Team")
team_passing <- team_passing[[1]]

team_shot_dashboard <- nba_leaguedashteamptshot(per_mode = "PerGame")
team_shot_dashboard <- team_shot_dashboard[[1]]

team_stats_per_game <- nba_leaguedashteamstats(per_mode = "PerGame")
team_stats_per_game<- team_stats_per_game[[1]]



convert_columns_to_numeric <- function(df) {
    df[, 5:ncol(df)] <- lapply(df[, 5:ncol(df)], function(x) as.numeric(as.character(x)))
    return(df)
}

# Apply this function to each of your data frames
player_drives <- convert_columns_to_numeric(player_drives)
player_post_touch <- convert_columns_to_numeric(player_post_touch)
player_elbow_touch <- convert_columns_to_numeric(player_elbow_touch)
player_paint_touch <- convert_columns_to_numeric(player_paint_touch)
player_catch_shoot <- convert_columns_to_numeric(player_catch_shoot)
player_pull_up <- convert_columns_to_numeric(player_pull_up)
player_passing <- convert_columns_to_numeric(player_passing)
player_shot_dashboard <- convert_columns_to_numeric(player_shot_dashboard)

team_drives <- convert_columns_to_numeric(team_drives)
team_post_touch <- convert_columns_to_numeric(team_post_touch)
team_elbow_touch <- convert_columns_to_numeric(team_elbow_touch)
team_paint_touch <- convert_columns_to_numeric(team_paint_touch)
team_catch_shoot <- convert_columns_to_numeric(team_catch_shoot)
team_pull_up <- convert_columns_to_numeric(team_pull_up)
team_passing <- convert_columns_to_numeric(team_passing)
team_shot_dashboard <- convert_columns_to_numeric(team_shot_dashboard)
team_stats_per_game <- convert_columns_to_numeric(team_stats_per_game)
team_info <- espn_nba_teams()

# Find abbreviations in team_info not in team_paint_touch
difference1 <- setdiff(team_info$abbreviation, team_paint_touch$TEAM_ABBREVIATION)

# Find abbreviations in team_paint_touch not in team_info
difference2 <- setdiff(team_paint_touch$TEAM_ABBREVIATION, team_info$abbreviation)

# Combine the differences
all_differences <- union(difference1, difference2)


mapping <- data.frame(
    old = difference1,
    new = difference2  # Corresponding values from difference2
)


for (i in 1:nrow(mapping)) {
    team_info$abbreviation[team_info$abbreviation == mapping$old[i]] <- mapping$new[i]
}




# Example mapping
team_mapping <- data.frame(
  TEAM_ABBREVIATION = team_info$abbreviation,
  TEAM_NAME = team_info$display_name
)


library(dplyr)

team_stats_per_game <- team_stats_per_game %>%
  left_join(team_mapping, by = "TEAM_NAME")


team_catch_shoot$PercentageOfTotal = (team_catch_shoot$CATCH_SHOOT_PTS / team_stats_per_game$PTS[match(team_catch_shoot$TEAM_NAME, team_stats_per_game$TEAM_NAME)]) * 100
team_drives$PercentageOfTotal = (team_drives$DRIVE_PTS / team_stats_per_game$PTS[match(team_drives$TEAM_NAME, team_stats_per_game$TEAM_NAME)]) * 100
team_pull_up$PercentageOfTotal = (team_pull_up$PULL_UP_PTS / team_stats_per_game$PTS[match(team_pull_up$TEAM_NAME, team_stats_per_game$TEAM_NAME)]) * 100






library(googleCloudStorageR)

# Function to upload a dataset to Google Cloud Storage
upload_dataset_to_gcs <- function(dataset, dataset_name, bucket_name) {
  # Create a temporary file
  temp_file <- tempfile(fileext = ".csv")
  
  # Write the dataset to the temporary file
  write.csv(dataset, temp_file, row.names = FALSE)
  
  # Construct the object name in the bucket
  object_name <- paste0(dataset_name, ".csv")
  
  # Upload the temporary file to Google Cloud Storage
  gcs_upload(temp_file, bucket = bucket_name, name = object_name ,predefinedAcl = 'bucketLevel')
  
  # Clean up by deleting the temporary file
  unlink(temp_file)
}

# Authenticate with your Google Cloud Service Account
gcs_auth("NBAdata/GCS_service_account_key/studious-vector-416020-f6ff74e5bdc1.json")

# List of datasets to upload
datasets <- list(
  player_catch_shoot = player_catch_shoot,
  player_drives = player_drives,
  player_elbow_touch = player_elbow_touch,
  player_paint_touch = player_paint_touch,
  player_passing = player_passing,
  player_post_touch = player_post_touch,
  player_pull_up = player_pull_up,
  player_shot_dashboard = player_shot_dashboard,
  team_catch_shoot = team_catch_shoot,
  team_drives = team_drives,
  team_elbow_touch = team_elbow_touch,
  team_info = team_info,
  team_mapping = team_mapping,
  team_paint_touch = team_paint_touch,
  team_passing = team_passing,
  team_post_touch = team_post_touch,
  team_pull_up = team_pull_up,
  team_stats_per_game = team_stats_per_game,
  team_shot_dashboard = team_shot_dashboard
)

# Upload each dataset
for (name in names(datasets)) {
  upload_dataset_to_gcs(datasets[[name]], name, "nba_dash_data")
}
