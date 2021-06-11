#' Tab Title Box
#'
#' Add extra functionality to titles, including a "more info" modal. Provides a
#' title and text to appear in a well under it.  A more info button
#  opens a modal with the id of title + "_info".  The modal must be
#' added as an observe.
#' @param title The title of the tab.
#' @param details Test to appear that describes the purpose of the tab.
#' @param modal Boolean optional "more info" modal
#'
#' @return
#' @export
#'
#' @examples
tabTitleBox <- function(title, details, modal = TRUE) {

        link <- paste0(tolower(title), "_info")
        link <- gsub(" ", "_", link)

        if (modal == TRUE) {
                action <- div(class = "title_info", actionLink(link, "More info"))
        } else {
                action <- div()
        }

        tagList(
                div(class = "title",
                    action,
                    h3(title)
                ),
                div(class = "well",
                    p(details)
                )
        )
}
