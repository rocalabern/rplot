#' r.plot.treemap
#' @seealso \code{\link{r.plot.new}} \code{\link{r.plot}} \code{\link{r.plot.add}}
#' \cr 2nd axis: \code{\link{r.plot.setmargins}} \code{\link{r.plot.newaxis}} \code{\link{r.plot.addaxis}}
#' \cr Other: \code{\link{r.plot.treemap}} \code{\link{r.plot.bar}} \code{\link{r.plot.histogram}} \code{\link{r.plot.distribution}}
#' \cr Colors: \code{\link{r.palette}} \code{\link{r.color}}
#' \cr Tools: \code{\link{r.plot.window}} \code{\link{r.plot.reset}} \code{\link{r.plot.close}}
#' @export
r.plot.treemap <- function (segment, segmentgroup,
                       area,
                       color, 
                       scale = NULL, 
                       lab = c(segmentgroup = TRUE, segment = FALSE),
                       main = "Treemap",
                       colorScaleLeft = rgb(0.9,0.0,0.0),
                       colorScaleCenter = rgb(1,1,1),
                       colorScaleRight = rgb(0.0,0.9,0.0),
                       colorLabelsegmentgroup = rgb(0,0,0,0.98),
                       colorLabelSegment = rgb(0,0,0,0.95),
                       colorBorder = "black",
                       cexMain = 1.5,
                       cexLabelsegmentgroup = 1,
                       cexLabelsegment = 0.1) 
{
  require(grid)
  if (any(length(segment) != length(area), length(segment) != length(segmentgroup), 
          length(segment) != length(color))) {
    stop("segment, area, segmentgroup, and color must be the same length.")
  }
  if (length(lab) == 1) {
    lab[2] <- lab[1]
  }
  if (missing(segment)) {
    segment <- seq_along(area)
    lab["segment"] <- FALSE
  }
  stopifnot(all(!is.na(segment)))
  data <- data.frame(label = segment, segmentgroup, area, color)
  data <- data[order(data$area, decreasing = TRUE), ]
  na.idx <- which(is.na(data$area) | is.na(data$segmentgroup) | is.na(data$color))
  if (length(na.idx)) {
    warning("Missing NAs for area, segmentgroup, or color will not be shown")
    data <- data[-na.idx, ]
  }
  zero.area.idx <- which(data$area == 0)
  if (length(zero.area.idx)) {
    data <- data[-zero.area.idx, ]
  }
  if (nrow(data) == 0) {
    stop("No records to display")
  }
  data$color.orig <- data$color
  if (is.null(scale)) {
    max_abs_data_color = max(abs(data$color))
    if (max_abs_data_color>0) data$color <- data$color / max_abs_data_color
  }
  else {
    data$color <- sapply(data$color, function(x) {
      if (x/scale > 1) 
        1
      else if (-1 > x/scale) 
        -1
      else x/scale
    })
  }
  data.by.segmentgroup <- split(data, data$segmentgroup, drop = TRUE)
  segmentgroup.data <- lapply(data.by.segmentgroup, function(x) {
    sum(x[, 3])
  })
  segmentgroup.data <- data.frame(area = as.numeric(segmentgroup.data), label = names(segmentgroup.data))
  segmentgroup.data <- segmentgroup.data[order(segmentgroup.data$area, decreasing = TRUE), 
                                         ]
  segmentgroup.data$color <- rep(NULL, nrow(segmentgroup.data))
  color.ramp.pos <- colorRamp(c(colorScaleCenter, colorScaleRight))
  color.ramp.neg <- colorRamp(c(colorScaleCenter, colorScaleLeft))
  color.ramp.rgb <- function(x) {
    col.mat <- mapply(function(x) {
      if (x < 0) {
        color.ramp.neg(abs(x))
      }
      else {
        color.ramp.pos(abs(x))
      }
    }, x)
    mapply(rgb, col.mat[1, ], col.mat[2, ], col.mat[3, ], 
           max = 255)
  }
  add.viewport <- function(z, label, color, x.0, y.0, x.1, 
                           y.1) {
    for (i in 1:length(label)) {
      if (is.null(color[i])) {
        filler <- gpar(col = "blue", fill = "transparent", 
                       cex = cexLabelsegmentgroup)
      }
      else {
        filler.col <- color.ramp.rgb(color[i])
        filler <- gpar(col = filler.col, fill = filler.col, 
                       cex = cexLabelsegment)
      }
      new.viewport <- viewport(x = x.0[i], y = y.0[i], 
                               width = (x.1[i] - x.0[i]), height = (y.1[i] - 
                                                                      y.0[i]), default.units = "npc", just = c("left", 
                                                                                                               "bottom"), name = as.character(label[i]), clip = "on", 
                               gp = filler)
      z <- append(z, list(new.viewport))
    }
    z
  }
  squarified.treemap <- function(z, x = 0, y = 0, w = 1, h = 1, 
                                 func = add.viewport, viewport.list) {
    cz <- cumsum(z$area)/sum(z$area)
    n <- which.min(abs(log(max(w/h, h/w) * sum(z$area) * 
                             ((cz^2)/z$area))))
    more <- n < length(z$area)
    a <- c(0, cz[1:n])/cz[n]
    if (h > w) {
      viewport.list <- func(viewport.list, z$label[1:n], 
                            z$color[1:n], x + w * a[1:(length(a) - 1)], rep(y, 
                                                                            n), x + w * a[-1], rep(y + h * cz[n], n))
      if (more) {
        viewport.list <- Recall(z[-(1:n), ], x, y + h * 
                                  cz[n], w, h * (1 - cz[n]), func, viewport.list)
      }
    }
    else {
      viewport.list <- func(viewport.list, z$label[1:n], 
                            z$color[1:n], rep(x, n), y + h * a[1:(length(a) - 
                                                                    1)], rep(x + w * cz[n], n), y + h * a[-1])
      if (more) {
        viewport.list <- Recall(z[-(1:n), ], x + w * 
                                  cz[n], y, w * (1 - cz[n]), h, func, viewport.list)
      }
    }
    viewport.list
  }
  map.viewport <- viewport(x = 0.05, y = 0.05, width = 0.9, 
                           height = 0.75, default.units = "npc", name = "MAP", just = c("left", 
                                                                                        "bottom"))
  map.tree <- gTree(vp = map.viewport, name = "MAP", children = gList(rectGrob(gp = gpar(col = "white"), 
                                                                               name = "background")))
  segmentgroup.viewports <- squarified.treemap(z = segmentgroup.data, viewport.list = list())
  for (i in 1:length(segmentgroup.viewports)) {
    this.segmentgroup <- data.by.segmentgroup[[segmentgroup.data$label[i]]]
    this.data <- data.frame(this.segmentgroup$area, this.segmentgroup$label, 
                            this.segmentgroup$color)
    names(this.data) <- c("area", "label", "color")
    segment.viewports <- squarified.treemap(z = this.data, 
                                            viewport.list = list())
    segmentgroup.tree <- gTree(vp = segmentgroup.viewports[[i]], name = segmentgroup.data$label[i])
    for (s in 1:length(segment.viewports)) {
      segment.tree <- gTree(vp = segment.viewports[[s]], name = this.data$label[s], 
                            children = gList(rectGrob(name = "color")))
      if (lab[2]) {
        segment.tree <- addGrob(segment.tree, textGrob(x = unit(1, 
                                                                "lines"), y = unit(1, "npc") - unit(1, "lines"), 
                                                       label = this.data$label[s], gp = gpar(col = colorLabelSegment), 
                                                       name = "label", just = c("left", "top")))
      }
      segmentgroup.tree <- addGrob(segmentgroup.tree, segment.tree)
    }
    segmentgroup.tree <- addGrob(segmentgroup.tree, rectGrob(gp = gpar(col = colorBorder), 
                                                             name = "border"))
    if (lab[1]) {
      segmentgroup.tree <- addGrob(segmentgroup.tree, textGrob(label = segmentgroup.data$label[i], 
                                                               name = "label", gp = gpar(col = colorLabelsegmentgroup)))
    }
    map.tree <- addGrob(map.tree, segmentgroup.tree)
  }
  op <- options(digits = 1)
  top.viewport <- viewport(x = 0.05, y = 1, width = 0.9, height = 0.2, 
                           default.units = "npc", name = "TOP", , just = c("left", 
                                                                           "top"))
  legend.ncols <- 51
  l.x <- (0:(legend.ncols - 1))/(legend.ncols)
  l.y <- unit(0.25, "npc")
  l.cols <- color.ramp.rgb(seq(-1, 1, by = 2/(legend.ncols - 
                                                1)))
  if (is.null(scale)) {
    l.end <- max(abs(data$color.orig))
  }
  else {
    l.end <- scale
  }
  top.list <- gList(textGrob(label = main, y = unit(0.7, "npc"), 
                             just = c("center", "center"), gp = gpar(cex = cexMain)), segmentsGrob(x0 = seq(0, 
                                                                                                            1, by = 0.25), y0 = unit(0.24, "npc"), x1 = seq(0, 1, 
                                                                                                                                                            by = 0.25), y1 = unit(0.2, "npc")), rectGrob(x = l.x, 
                                                                                                                                                                                                         y = l.y, width = 1/legend.ncols, height = unit(0.7, "lines"), 
                                                                                                                                                                                                         just = c("left", "bottom"), gp = gpar(col = NA, fill = l.cols), 
                                                                                                                                                                                                         default.units = "npc"), textGrob(label = format(l.end * 
                                                                                                                                                                                                                                                           seq(-1, 1, by = 0.5), trim = TRUE), x = seq(0, 1, by = 0.25), 
                                                                                                                                                                                                                                          y = 0.1, default.units = "npc", just = c("center", "center"), 
                                                                                                                                                                                                                                          gp = gpar(col = "black", cex = 0.6, fontface = "bold")))
  options(op)
  top.tree <- gTree(vp = top.viewport, name = "TOP", children = top.list)
  treemap <- gTree(name = "TREEMAP", children = gList(rectGrob(gp = gpar(col = "white", 
                                                                         fill = "white"), name = "background"), top.tree, 
                                                      map.tree))
  grid.newpage()
  grid.draw(treemap)
}