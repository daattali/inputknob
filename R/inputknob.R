#' Create a `<input-knob>` web component
#'
#' Binding to the `<input-knob>` web component version 1.0.0.
#'
#' @param id ID for the component. If not provided, an ID will be automatically
#' generated.
#' @param value TODO
#' @param scale TODO
#' @param min Required
#' @param max Required
#' @param slot Content to place in the default (unnamed) slot.
#' @param slot_back_side TODO
#' @param css_knob_size TODO
#' @param ... Any additional HTML attributes to add to the element tag.
#' @export
inputknob <- function(
  id = NULL,
  value = NULL,
  scale = NULL,
  min = NULL,
  max = NULL,
  slot = NULL,
  slot_back_side = NULL,
  css_knob_size = NULL,
  ...
  ) {

  dependencies <- list(
    htmltools::htmlDependency(
      name = "input-knob",
      version = "1.0.0",
      src = "webcomponent",
      package = "inputknob",
      script = list(src = "input-knob.js", type = "module")
    )
  )

  params <- as.list(environment())
  params_extra <- eval(substitute(alist(...)))

  shinywc::shinywc_ui(
    tag = "input-knob",
    params = params,
    params_extra = params_extra,
    attributes = list("value", "scale", "min", "max"),
    required = list("min", "max"),
    events = list("knob-move-change", "knob-move-start", "knob-move-end"),
    slots = list("back-side"),
    styles = list("--knob-size"),
    dependencies = dependencies
  )
}

Shinywc_inputknob <- R6::R6Class(
  "Shinywc_inputknob",
  inherit = shinywc::ShinywcProxy,

  public = list(
    initialize = function(id, session = shiny::getDefaultReactiveDomain()) {
      super$initialize(tag = "input-knob", id = id, session = session)
    },
    event_knob_move_change = function() {
      super$listen_event("knob-move-change")
    },
    event_knob_move_start = function() {
      super$listen_event("knob-move-start")
    },
    event_knob_move_end = function() {
      super$listen_event("knob-move-end")
    },

    get_value_prop = function(cb) {
      super$get_prop("value", cb)
    },
    set_value_prop = function(value) {
      super$set_prop("value", value)
    },
    get_scale_prop = function(cb) {
      super$get_prop("scale", cb)
    },
    set_scale_prop = function(value) {
      super$set_prop("scale", value)
    },
    get_min_prop = function(cb) {
      super$get_prop("min", cb)
    },
    set_min_prop = function(value) {
      super$set_prop("min", value)
    },
    get_max_prop = function(cb) {
      super$get_prop("max", cb)
    },
    set_max_prop = function(value) {
      super$set_prop("max", value)
    },

    call_rotateLeft = function(turns) {
      params <- list(turns)
      super$call_method("rotateLeft", params)
    },
    call_rotateRight = function(turns) {
      params <- list(turns)
      super$call_method("rotateRight", params)
    }
  ),

  active = list(
    value = function(value) {
      if (missing(value)) super$get_attr("value")
      else super$set_attr("value", value)
    },
    scale = function(value) {
      if (missing(value)) super$get_attr("scale")
      else super$set_attr("scale", value)
    },
    min = function(value) {
      if (missing(value)) super$get_attr("min")
      else super$set_attr("min", value)
    },
    max = function(value) {
      if (missing(value)) super$get_attr("max")
      else super$set_attr("max", value)
    }
  )
)

#' @export
inputknob_proxy <- function(id, session = shiny::getDefaultReactiveDomain()) {
  Shinywc_inputknob$new(id = id, session = session)
}
