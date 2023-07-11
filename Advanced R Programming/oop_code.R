library(readr)
library(magrittr)
library(tidyr)
library(dplyr)

make_LD <- function(data) {
  list <- split(data, data$id)
  structure(list, class = "LongitudinalData")
}

subject <- function(data, id) UseMethod("subject")
visit <- function(data, visit_num) UseMethod("visit")
room <- function(data, room_name) UseMethod("room")

print.LongitudinalData <- function(data) {
  cat("Longitudinal dataset with", length(data), "subjects", "\n")
}

subject.LongitudinalData <- function(data, id) {
  subject <- data[[as.character(id)]]
  structure(list(id = id, dataset = subject), class = "subject")
}

print.subject <- function(data) {
  if(is.null(data[["dataset"]])) {
    NULL
  } else {
    cat("Subject ID:", data[["id"]])
  }
}

summary.subject <- function(data) {
  subject_summary <- data[["dataset"]] %>%
    group_by(visit, room) %>%
    summarize(mean_pollution = mean(value), .groups = "keep") %>%
    spread(key = room, value = mean_pollution) %>%
    as.data.frame()
  
  structure(list(id = data[["id"]],
                 dataset = subject_summary), class = "Summarize")
}

visit.subject <- function(data, visit_num) {
  visit_data <- data[["dataset"]] %>%
    filter(visit == visit_num)
  structure(list(id = data[["id"]],
                 visit = visit_num,
                 dataset = visit_data), class = "visit")
}

room.visit <- function(data, room_name) {
  room_data <- data[["dataset"]] %>%
    filter(room == room_name)
  structure(list(id = data[["id"]],
                 visit = data[["visit"]],
                 room = room_name,
                 dataset = room_data), class = "room")
}

print.room <- function(data) {
  if(is.null(data[["dataset"]])) {
    NULL
  } else {
    cat("ID: ", data[["id"]], "\n")
    cat("Visit: ", data[["visit"]], "\n")
    cat("Room: ", data[["room"]])
  }
}

summary.room <- function(data) {
  room_summary <- summary(data[["dataset"]]$value)
  structure(list(id = data[["id"]],
                 dataset = room_summary), class = "Summarize")
}

print.Summarize <- function(data) {
  cat("ID: ", data[["id"]], '\n')
  data[["dataset"]]
}