#' Internal function for DB.
#' @param db data-base
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
#' @param db data-base
#' @param table table to create ID for
get_unique_id <- function(db, table) {
  query <- paste(
    "SELECT COUNT(*) AS N FROM",
    table)
  row_count <- DBI::dbGetQuery(db, query)$N
  id <- row_count + 1
  id
}

#' Convert other info into character form
#' @param other_info List to convert to JSON
convert_other_info <- function(other_info) {
  jsonlite::toJSON(other_info)
}

#' Add a contact to the contacts table.
#' @param title Title (Mr/Ms) of contact
#' @param firstname First name
#' @param surname Last name/surname
#' @param fullname Full name including title, firstname, surname
#' @param other_info Other information.
#' @param db Database file.
#' @export
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

#' Add a client to the clients table.
#' @param name Name of the client
#' @param short_name Short name of client
#' @param long_name Long name of client
#' @param other_info Other information.
#' @param db Database file.
#' @export
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

#' Add a position to the positions table.
#' @param contact_id Id of contact
#' @param client_id id of client
#' @param name name of the position.
#' @param other_info Other information.
#' @param db Database file.
#' @export
add_position <- function(
  contact_id,
  client_id,
  name,
  start_date = NA_character_,
  end_date = NA_character_,
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
    start_date = as.character(start_date),
    end_date = as.character(end_date),
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

#' Add a address to the addresses table.
#' @param contact_id Id of contact
#' @param position_id id of client
#' @param type name of the address type.
#' @param value value of address.
#' @param db Database file.
#' @export
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
  # Insert data into the contacts table
  new_address <- data.frame(
    id = id,
    contact_id = contact_id,
    position_id = position_id,
    type = type,
    value = value)

  DBI::dbWriteTable(
    db,
    "address",
    new_address,
    append = TRUE,
    row.names = FALSE)

  message("address added successfully with id `", id, "`")
  invisible(TRUE)
}
