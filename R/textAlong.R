textAlongGrob <- function(label, x = unit(0.5, 'npc'), y = unit(0.5, 'npc'),
                          just = 'centre', hjust = NULL, vjust = NULL, rot = 0,
                          check.overlap = FALSE, rot.type = 'rot', x0 = 0,
                          y0 = 0, x1 = 0, y1 = 0, force.rot = TRUE, dodge = NULL,
                          push = NULL, bg.colour="white", bg.r=0.1,
                          default.units = 'npc', name = NULL, gp = gpar(),
                          vp = NULL) {
  if (!is.unit(x)) {
    x <- unit(x, default.units)
  }
  if (!is.unit(y)) {
    y <- unit(y, default.units)
  }
  if (rot.type == 'rot') {
    shadowtext::shadowtextGrob(
      label = label, x = x, y = y, just = just, hjust = hjust,
      vjust = vjust, rot = rot, check.overlap = check.overlap,
      default.units = default.units, name = name, gp = gp, vp = vp,
      bg.colour=bg.colour, bg.r=bg.r
    )
  } else {
    if (!rot.type %in% c('along', 'across')) {
      cli::cli_abort('{.arg rot.type} must be either {.val rot}, {.val along), or {.val across}')
    }
    if (!is.unit(x0)) {
      x0 <- unit(x0, default.units)
    }
    if (!is.unit(y0)) {
      y0 <- unit(y0, default.units)
    }
    if (!is.unit(x1)) {
      x1 <- unit(x1, default.units)
    }
    if (!is.unit(y1)) {
      y1 <- unit(y1, default.units)
    }
    # x0 <- convertX(x0, 'mm', TRUE)
    # y0 <- convertY(y0, 'mm', TRUE)
    # x1 <- convertX(x1, 'mm', TRUE)
    # y1 <- convertY(y1, 'mm', TRUE)
    # xpos <- convertX(x, 'mm', TRUE)
    # ypos <- convertY(y, 'mm', TRUE)
    # angle <- edge_angle(x0, y0, x1, y1)
    # if (rot.type == 'across') {
    #   angle <- angle - 90
    # }
    # if (!is.null(dodge)) {
    #   dodge <- convertHeight(dodge, 'mm', TRUE)
    #   dodge_angle <- (angle + 90) / 360 * 2 * pi
    #   dodge_x <- cos(dodge_angle) * dodge
    #   dodge_y <- sin(dodge_angle) * dodge
    #   xpos <- xpos + dodge_x
    #   ypos <- ypos + dodge_y
    # }
    # if (!is.null(push)) {
    #   push <- convertHeight(push, 'mm', TRUE)
    #   push_angle <- angle / 360 * 2 * pi
    #   push_x <- cos(push_angle) * push
    #   push_y <- sin(push_angle) * push
    #   xpos <- xpos + push_x
    #   ypos <- ypos + push_y
    # }
    # if (force.rot) {
    #   fix <- angle > 90 & angle < 270
    #   angle[fix] <- angle[fix] + 180
    # }
    # shadowtext::shadowtextGrob(
    #  label = label, x = x, y = y,
    #  just = just, hjust = hjust,
    #   vjust = vjust, rot=angle,
    #   check.overlap = check.overlap, name = name, gp = gp, vp = vp,
    #   bg.colour=bg.colour, bg.r=bg.r 
    #   )
    gTree(
      label = label, x = x, y = y, just = just, hjust = hjust,
      vjust = vjust, rot.type = rot.type, x0 = x0, y0 = y0, x1 = x1,
      y1 = y1, force.rot = force.rot, dodge = dodge, push = push,
      check.overlap = check.overlap, name = name, gp = gp, vp = vp,
      bg.colour=bg.colour, bg.r=bg.r,
      cl = 'textalong'
    )
  }
}
#' Text angled according to line
#'
#' This function takes care of recalculating the angle of the text as the device
#' size changes
#'
#' @importFrom grid makeContent
#' @export
#' @keywords internal
makeContent.textalong <- function(x) {
  x0 <- convertX(x$x0, 'mm', TRUE)
  y0 <- convertY(x$y0, 'mm', TRUE)
  x1 <- convertX(x$x1, 'mm', TRUE)
  y1 <- convertY(x$y1, 'mm', TRUE)
  xpos <- convertX(x$x, 'mm', TRUE)
  ypos <- convertY(x$y, 'mm', TRUE)
  angle <- edge_angle(x0, y0, x1, y1)
  if (x$rot.type == 'across') {
    angle <- angle - 90
  }
  if (!is.null(x$dodge)) {
    dodge <- convertHeight(x$dodge, 'mm', TRUE)
    dodge_angle <- (angle + 90) / 360 * 2 * pi
    dodge_x <- cos(dodge_angle) * dodge
    dodge_y <- sin(dodge_angle) * dodge
    xpos <- xpos + dodge_x
    ypos <- ypos + dodge_y
  }
  if (!is.null(x$push)) {
    push <- convertHeight(x$push, 'mm', TRUE)
    push_angle <- angle / 360 * 2 * pi
    push_x <- cos(push_angle) * push
    push_y <- sin(push_angle) * push
    xpos <- xpos + push_x
    ypos <- ypos + push_y
  }
  if (x$force.rot) {
    fix <- angle > 90 & angle < 270
    angle[fix] <- angle[fix] + 180
  }
  sgrob <- shadowtext::shadowtextGrob(
    label = x$label, x = unit(xpos, 'mm'), y = unit(ypos, 'mm'),
    just = x$just, hjust = x$hjust, vjust = x$vjust, rot = angle,
    check.overlap = x$check.overlap, name = x$name, gp = x$gp, vp = x$vp,
    bg.colour=x$bg.colour, bg.r=x$bg.r
  )
  chg <- gList()
  ## Rename
  for (i in seq_along(sgrob$children)) {
    chd <- sgrob$children[[i]]
    chd$name <- paste0("textalong",i)
    chg <- gList(chg, chd)
  }
  setChildren(x, chg)
}