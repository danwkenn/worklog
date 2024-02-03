#' Internal function for DB.
get_db <- function(db) {
  if (is.null(db)) {
    db <- Sys.getenv("WORKLOG_DB_PATH")
  }

  if (is.character(db)) {
    db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  }
  db
}

#' Internal function for new ID creation
get_unique_id <- function(db, table) {
  query <- paste(
    "SELECT COUNT(*) AS N FROM",
    table)
  row_count <- DBI::dbGetQuery(db, query)$N
  id <- row_count + 1
  id
}

convert_other_info <- function(other_info) {
  jsonlite::toJSON(other_info)
}

add_contact <- function(
  title,
  firstname,
  surname,
  fullname,
  other_info = NULL,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "contacts")
  other_info <- convert_other_info(other_info)
  # Insert data into the contacts table
  new_contact <- data.frame(
    id = id,
    title = title,
    firstname = firstname,
    surname = surname,
    fullname = fullname,
    other_info = as.character(other_info))

  DBI::dbWriteTable(
    db,
    "contacts",
    new_contact,
    append = TRUE,
    row.names = FALSE)

  message("Contact added successfully with id `", id, "`")
  invisible(TRUE)
}


add_client <- function(
  name,
  short_name,
  long_name,
  other_info = NULL,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "clients")
  other_info <- convert_other_info(other_info)
  # Insert data into the contacts table
  new_contact <- data.frame(
    id = id,
    name = name,
    short_name = short_name,
    long_name = long_name,
    other_info = as.character(other_info))

  DBI::dbWriteTable(
    db,
    "clients",
    new_contact,
    append = TRUE,
    row.names = FALSE)

  message("Client added successfully with id `", id, "`")
  invisible(TRUE)
}

add_position <- function(
  contact_id,
  client_id,
  name,
  other_info = NULL,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "positions")
  other_info <- convert_other_info(other_info)
  # Insert data into the contacts table
  new_position <- data.frame(
    id = id,
    contact_id = contact_id,
    client_id = client_id,
    name = name,
    other_info = as.character(other_info))

  DBI::dbWriteTable(
    db,
    "positions",
    new_position,
    append = TRUE,
    row.names = FALSE)

  message("position added successfully with id `", id, "`")
  invisible(TRUE)
}

add_address <- function(
  contact_id,
  position_id,
  type,
  value,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "addresses")
  other_info <- convert_other_info(other_info)
  # Insert data into the contacts table
  new_address <- data.frame(
    id = id,
    contact_id = contact_id,
    position_id = position_id,
    type = type,
    address = address)

  DBI::dbWriteTable(
    db,
    "address",
    new_address,
    append = TRUE,
    row.names = FALSE)

  message("address added successfully with id `", id, "`")
  invisible(TRUE)
}
