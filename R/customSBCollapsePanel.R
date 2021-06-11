#' Custom Shiny Collapse Panel
#'
# 'Create a unique collapsible sidebar panel
#' @param name A name for the panel, must be unique
#' @param content_tag_list Content stored in a shiny tagList
#'
#' @return
#' @export
#'
#' @examples
customSBCollapsePanel <- function(name, content_tag_list) {

        id <- gsub("[[:punct:]]", "", name)
        id <- gsub("[[:space:]]", "-", id)
        id0 <- paste0(id, "-0")
        id0heading <- paste0(id, "-heading")
        id0collapse <- paste0(id, "-collapse")
        id0collapsehash <- paste0("#", id0collapse)
        idhash <- paste0("#", id)
        idhref <- paste0("#", id)

        panel_tags <-
                #######################Collapsible Panel Tagset
                div(id = id,
                    class = "panel-group",
                    role = "tablist",
                    'aria-multiselectable' = "true",
                    div(id = id0,
                        class = "panel panel-primary",
                        div(id = id0heading,
                            class = "panel-heading",
                            'data-toggle' = "collapse",
                            'data-target' = id0collapsehash,
                            'data-parent' = idhash,
                            'aria-expanded' = "true",
                            'aria-controls' = id0collapse,
                            style = "cursor: pointer;",
                            h4(class = "panel-title",
                               name
                            )#/h4
                        ),#/div id0heading
                        div(id = id0collapse,
                            class = "panel-collapse collapse",
                            role = "tabpanel",
                            'aria-labelledby' = id0heading,
                            div(class="panel-body",
                                content_tag_list
                            )#/div panel-body
                        )#/div id0collapse
                    )#/panel id0
                )#/id
        ######################

        return(panel_tags)
}
