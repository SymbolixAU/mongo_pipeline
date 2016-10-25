#' match pipeline
#'
#' builds the \code{$match} pipeline
#' @param fields data.frame or JSON of field & values to include in the match
#' pipeline. A data.frame must be one row, and where the column names are the
#' keys and the row contains the values.
#' @return match pipline string
#' @export
match_pipeline <- function(fields){

	if(inherits(fields, "data.frame")){
		if(nrow(fields) != 1)
			stop("the fields data.frame can only be one row long")

		match <- gsub("\\[|\\]", "", jsonlite::toJSON(fields, dataframe = "rows"))
		match <- paste0('{ "$match" : ', match, ' }')
	}else{
		match <- paste0('{ "$match" : { "', fields, '" } }')
	}

	if(!jsonlite::validate(match))
		stop(json_error("match"))

	return(match)
}

#' project pipeline
#'
#' builds the \code{$project} pipeline
#' @param fields vector of fields to include in the project pipeline
#' @param exclude_id logical
#' @return project pipeline string
#' @export
project_pipeline <- function(fields, exclude_id = TRUE){

	if(!is.logical(exclude_id))
		stop("exclude_id must either be TRUE or FALSE")

	f <- paste0("\"", fields, "\"")

	project <- paste0('{ "$project" : {', ' "_id" : ', as.numeric(!exclude_id),
										" , ", paste0(f, " : 1", collapse = " , "), '} }')

	if(!jsonlite::validate(project))
		stop(json_error("project"))

	return(project)
}

json_error <- function(pipeline){
	return(paste0("There is an error in the JSON for the ", pipeline, " pipeline"))
}
