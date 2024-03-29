% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add.R
\name{add_task}
\alias{add_task}
\title{Add a task to the tasks table.}
\usage{
add_task(project_id, name, description, completion_time, status, db = NULL)
}
\arguments{
\item{project_id}{Project ID}

\item{name}{Name of task}

\item{description}{Description of task}

\item{completion_time}{time taken to complete}

\item{status}{Not Started, In progress, Finished}

\item{db}{Database file.}
}
\description{
Add a task to the tasks table.
}
