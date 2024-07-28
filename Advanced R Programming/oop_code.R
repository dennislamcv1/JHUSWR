# Define the LongitudinalData class
LongitudinalData <- function(data) {
  if (!all(c("id", "visit", "room", "value", "timepoint") %in% names(data))) {
    stop("Data must contain 'id', 'visit', 'room', 'value', and 'timepoint' columns.")
  }
  structure(list(data = data), class = "LongitudinalData")
}

# Define a constructor function
make_LD <- function(data) {
  LongitudinalData(data)
}


# Define the Subject class
Subject <- function(data, id) {
  # Ensure data is a data frame
  if (!inherits(data, "data.frame")) stop("Input data must be a data frame.")
  
  subject_data <- data[data$id == id, , drop = FALSE]
  if (nrow(subject_data) == 0) warning("No data for subject with ID ", id)
  structure(subject_data, class = c("Subject", "data.frame"))
}

# Define the Visit class
Visit <- function(data, visit) {
  # Ensure data is a data frame
  if (!inherits(data, "data.frame")) stop("Input data must be a data frame.")
  
  visit_data <- data[data$visit == visit, , drop = FALSE]
  if (nrow(visit_data) == 0) warning("No data for visit number ", visit)
  structure(visit_data, class = c("Visit", "data.frame"))
}

# Define the Room class
Room <- function(data, room) {
  # Ensure data is a data frame
  if (!inherits(data, "data.frame")) stop("Input data must be a data frame.")
  
  room_data <- data[data$room == room, , drop = FALSE]
  if (nrow(room_data) == 0) warning("No data for room '", room, "'")
  structure(room_data, class = c("Room", "data.frame"))
}

# Generic function and method for subject
subject <- function(object, id) {
  UseMethod("subject")
}

subject.LongitudinalData <- function(object, id) {
  Subject(object$data, id)
}

# Generic function and method for visit
visit <- function(object, visit) {
  UseMethod("visit")
}

visit.Subject <- function(object, visit) {
  Visit(object, visit)
}

# Generic function and method for room
room <- function(object, room) {
  UseMethod("room")
}

room.Visit <- function(object, room) {
  Room(object, room)
}

# Print method for LongitudinalData
print.LongitudinalData <- function(x, ...) {
  cat("LongitudinalData Object\n")
  cat("Number of subjects:", length(unique(x$data$id)), "\n")
  cat("Number of visits:", length(unique(x$data$visit)), "\n")
  cat("Number of rooms:", length(unique(x$data$room)), "\n")
  invisible(x)
}

# Summary method for Subject
summary.Subject <- function(object, ...) {
  cat("Summary for Subject ID:", unique(object$id), "\n")
  summary(object$value)
}


