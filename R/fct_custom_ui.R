clickable_value_box <- function(value, subtitle = "", icon = NULL, id = "box1",
                                color = "#FFFFFF", background = "#36486b", width = NULL) {
  
  style <- paste0("color: ", color, "; background-color: ", background, ";")
  boxContent <- div(class = "action-button small-box", style = style, id = id,
                    div(class = "inner", h3(value, style = "white-space:normal"), p(subtitle)),
                    if (!is.null(icon)) dev(class = "icon-large", icon))
  
  box <- div(class = if(!is.null(width)) paste0("col-sm-", width), boxContent)
}

watchlist_modal <- function(ticker) {
  modalDialog(
    title = strong(ticker),
    renderText({"Congratulations, we have made it to the modal"}),
    size = "s",
    easyClose = TRUE
  )
}