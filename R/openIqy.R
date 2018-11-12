#' Open an Informer Live Excel .iqy file
#'
#' @description This function is designed to take the path to an .iqy file used with Excel.  The function can
#' be provided a named list of predifined parameters.
#'
#' @param path Character of the path to a Live Excel .iqy file.
#' @param param_list A named list with the run time parameters for opening the data source.  If not provided,
#' these fields will be prompted for in the console.
#' @param build_only If TRUE, the function can be used as a helper app to build the param_list without executing the query.
#' @param convert If TRUE, character columns with all digit values, and dates with all fields formatted YYYY-MM-DD will be converted to the appropriate type.
#' @return
#' @export
#'
#' @examples
#' df <- openIqy("path/to/query.iqy)  #Will prompt for parameters if any expected
#' params <- openIqy("path/to/query.iqy, build_only = TRUE)  ##Will return a parameter list from prompts
#' df <- openIqy("path/to/query.iqy, params)  ##Will load using the paramters saved in list
openIqy <- function(path, param_list = NULL, build_only = FALSE, convert = TRUE) {
        qurl <- .parseIqyUrl(path)
        if (is.null(param_list)) {
                param_list <- .makeQParamList(qurl)
                param_list <- .promptQParamList(param_list)
                if(build_only == TRUE) {
                        if(length(param_list) <= 0) {
                                message("No parameters to build list for.")
                        }
                        return(param_list)
                }
        }
        qurl <- .makeIqyUrl(qurl, param_list)
        print(qurl)
        response <- httr::GET(qurl)##Get the http response
        content <- XML::htmlParse(httr::content(response, "text", encoding = "ISO-8859-1"))##Extract the content
        content <- XML::getNodeSet(content, "//table")
        content <- XML::readHTMLTable(content[[1]], stringsAsFactors = FALSE, trim = TRUE)
        content <- na.omit(content)

        ##Convert columns to numeric or date if they match these criteria
        if(convert){
                suppressWarnings(
                        nums <- colnames(content)[
                                unlist(lapply(content, function(x) all(!is.na(as.numeric(x)))))
                                ]
                )
                suppressWarnings(
                        dates <- colnames(content)[
                                unlist(lapply(content,
                                              function(x) all(grepl("^\\d{4}-\\d{2}-\\d{2}", x))))
                                ]
                )

                content <- dplyr::mutate_at(content, vars(nums), as.numeric)
                content <- dplyr::mutate_at(content, vars(dates), as.Date)

        }


        return(content)
}


##Load the url from the iqy file and parse it into a list
.parseIqyUrl <- function(path) {
        q <- readLines(path)
        qurl <- paste(q[3], q[4], sep = "?")
        qurl <- httr::parse_url(qurl)
        return(qurl)
}

##Extract the informer parameter fields for the URL and their prompts
.makeQParamList <- function(qurl) {
        param_list <- qurl$query[grepl("parameter", x = names(qurl$query))]
        return(param_list)
}

##Prompt to supply the values for the query, save as a list
.promptQParamList <- function(param_list) {
        n <- 0
        for (a in param_list) {
                n <- n + 1
                param_list[n] <- readline(paste(names(param_list[n]), a, ": ", sep = " "))
        }
        return(param_list)
}

##Merge a list of provided values with the parsed url list
.makeIqyUrl <- function(qurl, param_list) {
        a <- length(qurl$query[grepl("parameter", x = names(qurl$query))])
        b <- length(param_list)
        if (a == b) {
                qurl$query <- modifyList(qurl$query, param_list)
                qurl <- httr::build_url(qurl)
                return(qurl)
        } else {
                message("Different number of parameters provided. Correct list?")
        }
}


