#### These functions do not work with the new sex- and age-based scoring ####
#### implemented in 2023.####

# These functions assign point values for raw scores in each of the events in the 
# U.S. Army Combat Fitness Test. The 'ACFT_points' function will calculate point values for 
# every event and provide an ACFT composite score for each participant.

# Each function requires two inputs:
# 1) a numeric vector or data frame, 

# 2) a reference table to determine point values 
# [this reference table must already be an object in your code]

# Clean ACFT -----------------------------------------------------------------------------

clean_acft <- function(x, table_ref){
  
  x <- x %>%
    acft_times() %>%
    acft_points(table_ref)

}

# SDC Time Conversion to Seconds ---------------------------------------------------------

SDC_time <- function(x){
  try(library(dplyr), 
      silent = TRUE)
  
  try(library(stringr),
      silent = TRUE)
  
  SDC_raw <- x[["SDC_raw"]]
  
  time_min <- as.numeric(str_extract(SDC_raw, "^[[:digit:]]+"))
  time_sec <- as.numeric(str_extract(SDC_raw, "\\.[[:digit:]]+"))
  
  time_tens <- as.numeric(substr(time_sec, 3, 3))
  time_ones <- as.numeric(substr(time_sec, 4, 4))
  
  time_min <- (time_min*60)
  time_tens <- (time_tens*10)
  
  sec <- cbind.data.frame(time_min, time_tens, time_ones)
  
  SDC_sec <- NULL
  
  SDC_sec <- rowSums(sec, 
                     na.rm = TRUE)
  
  SDC_sec[SDC_sec == 0] <- NA
  
  x <- cbind.data.frame(x, SDC_sec)
  
}


# X2MR Time Conversion to Seconds ---------------------------------------------------------

Run_time <- function(x){
  try(library(dplyr), 
      silent = TRUE)
  
  try(library(stringr),
      silent = TRUE)
  
  X2MR_raw <- x[["X2MR_raw"]]
  
  time_min <- as.numeric(str_extract(X2MR_raw, "^[[:digit:]]+"))
  time_sec <- as.numeric(str_extract(X2MR_raw, "\\.[[:digit:]]+"))
  
  time_tens <- as.numeric(substr(time_sec, 3, 3))
  time_ones <- as.numeric(substr(time_sec, 4, 4))
  
  time_min <- (time_min*60)
  time_tens <- (time_tens*10)
  
  sec <- cbind.data.frame(time_min, time_tens, time_ones)
  
  X2MR_sec <- NULL
  
  X2MR_sec <- rowSums(sec, 
                     na.rm = TRUE)
  
  X2MR_sec[X2MR_sec == 0] <- NA
  
  x <- cbind.data.frame(x, X2MR_sec)
  
}


# Plank Time Conversion ------------------------------------------------------------------

PLK_time <- function(x){
  try(library(dplyr), 
      silent = TRUE)
  
  try(library(stringr),
      silent = TRUE)
  
  PLK_raw <- x[["PLK_raw"]]
  
  time_min <- as.numeric(str_extract(PLK_raw, "^[[:digit:]]+"))
  time_sec <- as.numeric(str_extract(PLK_raw, "\\.[[:digit:]]+"))
  
  time_tens <- as.numeric(substr(time_sec, 3, 3))
  time_ones <- as.numeric(substr(time_sec, 4, 4))
  
  time_min <- (time_min*60)
  time_tens <- (time_tens*10)
  
  sec <- cbind.data.frame(time_min, time_tens, time_ones)
  
  PLK_sec <- NULL
  
  PLK_sec <- rowSums(sec, 
                     na.rm = TRUE)
  
  PLK_sec[PLK_sec == 0] <- NA
  
  x <- cbind.data.frame(x, PLK_sec)
  
}


# ACFT times -----------------------------------------------------------------------------

acft_times <- function(x){
  
  x <- x %>%
    SDC_time() %>%
    Run_time() %>%
    PLK_time()
  
  print(x)
}


# MDL Points ------------------------------------------------------------------

MDL_points <- function(x, table_ref){
  try(library(data.table), 
      silent = TRUE)
  
  try(library(dplyr), 
      silent = TRUE)
  
  ifelse("rowid" %in% names(x),
         df <- x,
         df <- rowid_to_column(x))
  
  points.mdl <- table_ref[, c("Points", "MDL_raw")]
  points.mdl <- points.mdl[complete.cases(points.mdl), ]
  
  dt1 <- setDT(df[, c("rowid", "MDL_raw")])
  dt1 <- dt1[complete.cases(dt1), ]
  
  dt2 <- setDT(points.mdl)
  
  dt3 <- dt2[dt1, on = "MDL_raw", roll = Inf]
  
  dt3 <- dt3 %>%
    rename("MDL_points" = "Points")
  
  df <- df %>%
    left_join(dt3,
              by = c("rowid", "MDL_raw"))
}


# Example:
#      acft <- MDL_points(acft, points)       


# SPT Points ------------------------------------------------------------------

SPT_points <- function(x, table_ref){
  try(library(data.table), 
      silent = TRUE)
  
  try(library(dplyr), 
      silent = TRUE)
  
  ifelse("rowid" %in% names(x),
         df <- x,
         df <- rowid_to_column(x))
  
  points.spt <- table_ref[, c("Points", "SPT_raw")]
  points.spt <- points.spt[complete.cases(points.spt), ]
  
  dt1 <- setDT(df[, c("rowid", "SPT_raw")])
  dt1 <- dt1[complete.cases(dt1), ]
  
  dt2 <- setDT(points.spt)
  
  dt3 <- dt2[dt1, on = "SPT_raw", roll = Inf]
  
  dt3 <- dt3 %>%
    rename("SPT_points" = "Points")
  
  df <- df %>%
    left_join(dt3,
              by = c("rowid", "SPT_raw"))
}

# Example:
#         acft <- SPT_points(acft, points)


# HRP Points ------------------------------------------------------------------

HRP_points <- function(x, table_ref){
  try(library(data.table), 
      silent = TRUE)
  
  try(library(dplyr), 
      silent = TRUE)
  
  ifelse("rowid" %in% names(x),
         df <- x,
         df <- rowid_to_column(x))
  
  points.hrp <- table_ref[, c("Points", "HRP_raw")]
  points.hrp <- points.hrp[complete.cases(points.hrp), ]
  
  dt1 <- setDT(df[, c("rowid", "HRP_raw")])
  dt1 <- dt1[complete.cases(dt1), ]
  
  dt2 <- setDT(points.hrp)
  
  dt3 <- dt2[dt1, on = "HRP_raw", roll = Inf]
  
  dt3 <- dt3 %>%
    rename("HRP_points" = "Points")
  
  df <- df %>%
    left_join(dt3,
              by = c("rowid", "HRP_raw"))
}

# Example:
#        acft <- HRP_points(acft, points)


# SDC Points ------------------------------------------------------------------

SDC_points <- function(x, table_ref){
  try(library(data.table), 
      silent = TRUE)
  
  try(library(dplyr), 
      silent = TRUE)
  
  ifelse("rowid" %in% names(x),
         df <- x,
         df <- rowid_to_column(x))
  
  points.sdc <- table_ref[, c("Points", "SDC_sec")]
  points.sdc <- points.sdc[complete.cases(points.sdc), ]
  
  dt1 <- setDT(df[, c("rowid", "SDC_sec")])
  dt1 <- dt1[complete.cases(dt1), ]
  
  dt2 <- setDT(points.sdc)
  
  dt3 <- dt2[dt1, on = "SDC_sec", roll = -Inf]
  
  dt3 <- dt3 %>%
    rename("SDC_points" = "Points")
  
  df <- df %>%
    left_join(dt3,
              by = c("rowid", "SDC_sec"))
}

# Example:
#       acft <- SDC_points(acft, points)


# LTK  Points -----------------------------------------------------------------

LTK_points <- function(x, table_ref){
  try(library(data.table), 
      silent = TRUE)
  
  try(library(dplyr), 
      silent = TRUE)
  
  ifelse("rowid" %in% names(x),
         df <- x,
         df <- rowid_to_column(x))
  
  points.ltk <- table_ref[, c("Points", "LTK_raw")]
  points.ltk <- points.ltk[complete.cases(points.ltk), ]
  
  dt1 <- setDT(df[, c("rowid", "LTK_raw")])
  dt2 <- setDT(points.ltk)
  
  dt3 <- dt2[dt1, on = "LTK_raw", roll = Inf]
  
  dt3 <- dt3 %>%
    rename("LTK_points" = "Points")
  
  df <- df %>%
    left_join(dt3,
              by = c("rowid", "LTK_raw"))
}

# acft <- LTK_points(acft, points)

# Plank Points ---------------------------------------------------------------

PLK_points <- function(x, table_ref){
  try(library(data.table), 
      silent = TRUE)
  
  try(library(dplyr), 
      silent = TRUE)
  
  ifelse("rowid" %in% names(x),
         df <- x,
         df <- rowid_to_column(x))
  
  points.plk <- table_ref[, c("Points", "PLK_sec")]
  points.plk <- points.plk[complete.cases(points.plk), ]
  
  dt1 <- setDT(df[, c("rowid", "PLK_sec")])
  dt2 <- setDT(points.plk)
  
  dt3 <- dt2[dt1, on = "PLK_sec", roll = Inf]
  
  dt3 <- dt3 %>%
    rename("PLK_points" = "Points")
  
  df <- df %>%
    left_join(dt3,
              by = c("rowid", "PLK_sec"))
}

# 2 MR Points -----------------------------------------------------------------

Run_points <- function(x, table_ref){
  try(library(data.table), 
      silent = TRUE)
  
  try(library(dplyr), 
      silent = TRUE)
  
  ifelse("rowid" %in% names(x),
         df <- x,
         df <- rowid_to_column(x))
  
  points.run <- table_ref[, c("Points", "X2MR_sec")]
  points.run <- points.run[complete.cases(points.run), ]
  
  dt1 <- setDT(df[, c("rowid", "X2MR_sec")])
  dt1 <- dt1[complete.cases(dt1), ]
  
  dt2 <- setDT(points.run)
  
  dt3 <- dt2[dt1, on = "X2MR_sec", roll = -Inf]
  
  dt3 <- dt3 %>%
    rename("X2MR_points" = "Points")
  
  df <- df %>%
    left_join(dt3,
              by = c("rowid", "X2MR_sec"))
}

# Example:
#       acft <- Run_points(acft, points)


# ACFT_points Function -----------------------------------------------------------

acft_points <- function(x, table_ref){
  try(library(data.table), 
      silent = TRUE)
  
  try(library(dplyr), 
      silent = TRUE)
  
  x <- x %>% 
    MDL_points(table_ref) %>% 
    SPT_points(table_ref) %>% 
    HRP_points(table_ref) %>% 
    SDC_points(table_ref) %>% 
    LTK_points(table_ref) %>% 
    Run_points(table_ref) %>%
    PLK_points(table_ref)
  
  # creating a combo point column by merging points from LTK and PLK
  x <- x %>%
    mutate("LTK_PLK_points" = coalesce(x$LTK_points, x$PLK_points))
  
  points_total <- cbind.data.frame(x$MDL_points, x$SPT_points, x$HRP_points, 
                                   x$SDC_points, 
                                   x$X2MR_points, x$LTK_PLK_points)
  
  x$ACFT_total <- c(x$ACFT_total, rowSums(points_total, 
                                          na.rm = TRUE))
  
  x$ACFT_total[x$ACFT_total == 0] <- NA
  
  print(x)
}

# Example:

#       acft_points(acft, points)