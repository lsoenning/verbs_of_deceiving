# PLOTTING
#================================================================================

## lattice plotting options
#--------------------------------------------------------------------------------

my_settings <- list()
my_settings$axis.line$col <- NA
my_settings$strip.border$col <- NA
my_settings$strip.background$col <- NA
my_settings$layout.heights$strip = 1.4
my_settings$clip$panel = "off"

my_settings$layout.heights$key.axis.padding <- 0
my_settings$layout.heights$xlab <- 0
my_settings$layout.heights$bottom.padding <- 0.5
my_settings$layout.heights$top.padding <- 1
my_settings$layout.heights$key.padding <- 0
my_settings$layout.heights$axis.top <- 0
my_settings$layout.heights$main <- 0
my_settings$layout.heights$main.key.padding <- 0

my_settings$layout.widths$ylab <- 0
my_settings$layout.widths$axis.panel <- 10
my_settings$layout.widths$right.padding <- 0
my_settings$layout.widths$left.padding <- 1
my_settings$layout.widths$key.right <- 0
my_settings$layout.widths$axis.right <- 0
my_settings$layout.widths$ylab.right <- 0

my_settings$axis.text$col <- "grey30"

my_settings$axis.components$left$pad1 <- 0.5
my_settings$axis.components$left$pad2 <- 1.4
my_settings$axis.components$bottom$pad1 <- 0.5
my_settings$axis.components$bottom$pad2 <- 1.4

# tick marks
my_settings$axis.components$bottom$tck <- .5
my_settings$axis.components$left$tck   <- .5
my_settings$axis.components$top$tck    <- .5
my_settings$axis.components$right$tck  <- .5

# space between axis label and tick mark labels
my_settings$layout.widths$ylab.axis.padding <- 0.2
my_settings$layout.heights$axis.xlab.padding <- 0.2


my_settings$box.rectangle$col = 1
my_settings$box.umbrella$col = 1
my_settings$box.dot$col = 1
my_settings$plot.symbol$col = 1


## function for text with edges
#--------------------------------------------------------------------------------

panel.text_halo <- 
	function(x, y=NULL, labels, col='black', bg='white', 
					 theta= seq(0, 2*pi, length.out=50), r=0.1, ... ) {
		
		xy <- xy.coords(x,y)
		xo <- r*strwidth('A')
		yo <- r*strheight('A')
		
		# draw background text with small shift in x and y in background colour
		for (i in theta) {
			panel.text(x= xy$x + cos(i)*xo, 
								 y= xy$y + sin(i)*yo, labels, col=bg, ... )
		}
		# draw actual text in exact xy position in foreground colour
		panel.text(xy$x, xy$y, labels, col=col, ... )
	}


## functions for axes
#--------------------------------------------------------------------------------

axis_L <-
	function(side, ..., line.col)
	{
		if (side %in% c("left", "bottom")) {
			col <- trellis.par.get("axis.text")$col
			axis.default(side, ..., line.col = col)
			if (side == "bottom")
				grid::grid.lines(y = 0)
			if (side == "left")
				grid::grid.lines(x = 0)
		}
	}

axis_C <-
  function(side, ..., line.col)
  {
    if (side %in% c("left", "bottom", "top")) {
      col <- trellis.par.get("axis.text")$col
      axis.default(side, ..., line.col = col)
      if (side == "bottom")
        grid::grid.lines(y = 0)
      if (side == "left")
        grid::grid.lines(x = 0)
      if (side == "top")
        grid::grid.lines(y = 1)
    }
  }

axis_U <-
  function(side, ..., line.col)
  {
    if (side %in% c("left", "bottom", "right")) {
      col <- trellis.par.get("axis.text")$col
      axis.default(side, ..., line.col = col)
      if (side == "bottom")
        grid::grid.lines(y = 0)
      if (side == "left")
        grid::grid.lines(x = 0)
      if (side == "right")
        grid::grid.lines(x = 1)
    }
  }

axis_all <-
  function(side, ..., line.col)
  {
    if (side %in% c("left", "bottom", "top", "right")) {
      col <- trellis.par.get("axis.text")$col
      axis.default(side, ..., line.col = col)
      if (side == "bottom")
        grid::grid.lines(y = 0)
      if (side == "left")
        grid::grid.lines(x = 0)
      if (side == "top")
        grid::grid.lines(y = 1)
      if (side == "right")
        grid::grid.lines(x = 1)
    }
  }

axis_left = function(side, ..., line.col)
{
	if (side %in% "left") {
		col <- trellis.par.get("axis.text")$col
		axis.default(side, ..., line.col = col)
		if (side == "left")
			grid::grid.lines(x = 0)
	}
}

axis_bottom = function(side, ..., line.col)
{
	if (side %in% "bottom") {
		col <- trellis.par.get("axis.text")$col
		axis.default(side, ..., line.col = col)
		if (side == "bottom")
			grid::grid.lines(y = 0)
	}
}

axis_top = function(side, ..., line.col)
{
	if (side %in% "top") {
		col <- trellis.par.get("axis.text")$col
		axis.default(side, ..., line.col = col)
		if (side == "top")
			grid::grid.lines(y = 1)
	}
}

# new panel functions
#--------------------------------------------------------------------------------

# Dot diagram

panel.dotdiagram <- function(  x, x_anchor=0, y_anchor=0, n_bins=20, seq_min=min(x), seq_max=max(x), scale_y=1, set_pch=19, set_col=1, set_cex=1, vertical=F, leftwards=F, downwards=F){
	x_steps <- diff(c(seq_min, seq_max))/(n_bins-1)
	x_breaks <- seq(seq_min-x_steps/2, seq_max+x_steps/2, length.out=n_bins+1)
	counts <- as.numeric(table(cut(x, breaks = x_breaks, right = F)))
	dot_col_matrix <- matrix("NA", nrow=n_bins, ncol=max(counts))
	for(i in 1:nrow(dot_col_matrix)) {
		if(counts[i] >= 1) dot_col_matrix[i, 1:counts[i]] <- set_col
		}
	if(vertical==T){
		if(leftwards == F){
			panel.xyplot(
				x = (rep(1:max(counts), each=n_bins)*scale_y) + x_anchor,
				y = rep(seq(seq_min, seq_max, length.out=n_bins), max(counts)), pch=set_pch, col=dot_col_matrix, cex=set_cex
			)		
			}
		if(leftwards == T){
			panel.xyplot(
				x = -((rep(1:max(counts), each=n_bins)*scale_y)) + x_anchor,
				y = rep(seq(seq_min, seq_max, length.out=n_bins), max(counts)), pch=set_pch, col=dot_col_matrix, cex=set_cex
			)		
		}
	}
	if(vertical==F){
		if(downwards == F){
			panel.xyplot(
				y = (rep(1:max(counts), each=n_bins)*scale_y) + y_anchor,
				x = rep(seq(seq_min, seq_max, length.out=n_bins), max(counts)), pch=set_pch, col=dot_col_matrix, cex=set_cex
			)		
			}
		if(downwards == T){
			panel.xyplot(
				y = -((rep(1:max(counts), each=n_bins)*scale_y)) + y_anchor,
				x = rep(seq(seq_min, seq_max, length.out=n_bins), max(counts)), pch=set_pch, col=dot_col_matrix, cex=set_cex
			)		
		}
	}
}


# Nice arrows

panel.Arrows <- function (x0, y0, x1, y1, code = 2, arr.length = 0.4, arr.width = arr.length/2, 
                          arr.adj = 0.5, arr.type = "curved", segment = TRUE, 
                          col = "black", lcol = col, lty = 1, arr.col = lcol, 
                          lwd = 1, arr.lwd = lwd, ...) 
{
  if (arr.type == "simple") {
    panel.arrows(x0, y0, x1, y1, code = code, length = arr.length/2.54, 
                 lty = lty, col = col, lwd = lwd, ...)
    return()
  }
  if (arr.type == "none") {
    return()
  }
  if (arr.type == "T") {
    panel.arrows(x0, y0, x1, y1, code = code, length = arr.length/(2 * 
                                                                     2.54), lty = lty, angle = 90, col = col, lwd = lwd, 
                 ...)
    return()
  }
  if (segment) 
    panel.segments(x0, y0, x1, y1, col = lcol, lty = lty, lwd = lwd, 
                   ...)
  user <- par("usr")
  pin <- par("pin")
  pin <- pin/max(pin)
  sy <- (user[4] - user[3])/pin[2]
  sx <- (user[2] - user[1])/pin[1]
  angle <- atan((y1 - y0)/(x1 - x0) * sx/sy)/pi * 180
  angle[is.nan(angle)] <- 0
  angle[x1 < x0] <- 180 + angle[x1 < x0]
  xx <- x1
  yy <- y1
  if (sy < 0 & sx < 0) 
    angle <- angle + 180
  else if (sx < 0) 
    angle <- angle + 180
  if (code == 3) 
    lattice.Arrowhead(x0 = xx, y0 = yy, angle = angle, lcol = lcol, 
                      arr.col = arr.col, arr.adj = arr.adj, lty = lty, 
                      arr.length = arr.length, arr.width = arr.width, arr.type = arr.type, 
                      arr.lwd = arr.lwd, ...)
  if (code != 2) {
    angle <- 180 + angle
    xx <- x0
    yy <- y0
  }
  lattice.Arrowhead(x0 = xx, y0 = yy, angle = angle, lcol = lcol, arr.col = arr.col, 
                    arr.adj = arr.adj, lty = lty, arr.length = arr.length, 
                    arr.width = arr.width, arr.type = arr.type, arr.lwd = arr.lwd, 
                    ...)
}




lattice.Arrowhead <- function (x0, y0, angle = 0, arr.length = 0.4, arr.width = arr.length/2, 
                               arr.adj = 0.5, arr.type = "curved", lcol = "black", 
                               lty = 1, arr.col = lcol, arr.lwd = 2, npoint = 5, ...) 
{
  if (arr.type == "none") {
    return()
  }
  if (arr.type == "curved") {
    rad <- 0.7
    len <- 0.25 * pi
    mid <- c(0, rad)
    x <- seq(1.5 * pi + len, 1.5 * pi, length.out = npoint)
    rr <- cbind(mid[1] - rad * cos(x), mid[2] + rad * sin(x))
    mid <- c(0, -rad)
    x <- rev(x)
    rr <- rbind(rr, cbind(mid[1] - rad * cos(x), mid[2] - 
                            rad * sin(x)))
    mid <- c(rr[nrow(rr), 1], 0)
    rd <- rr[1, 2]
    x <- seq(pi/2, 3 * pi/2, length.out = 3 * npoint)
    rr <- rbind(rr, cbind(mid[1] - rd * 0.25 * cos(x), mid[2] - 
                            rd * sin(x)))
    rr[, 1] <- rr[, 1] * 2.6
    rr[, 2] <- rr[, 2] * 3.45
  }
  else if (arr.type == "triangle") {
    x <- c(-0.2, 0, -0.2)
    y <- c(-0.1, 0, 0.1)
    rr <- 6.22 * cbind(x, y)
  }
  else if (arr.type %in% c("circle", "ellipse")) {
    if (arr.type == "circle") 
      arr.width = arr.length
    rad <- 0.1
    mid <- c(-rad, 0)
    x <- seq(0, 2 * pi, length.out = 15 * npoint)
    rr <- 6.22 * cbind(mid[1] + rad * sin(x), mid[2] + rad * 
                         cos(x))
  }
  if (arr.adj == 0.5) 
    rr[, 1] <- rr[, 1] - min(rr[, 1])/2
  if (arr.adj == 0) 
    rr[, 1] <- rr[, 1] - min(rr[, 1])
  user <- par("usr")
  pcm <- par("pin") * 2.54
  sy <- (user[4] - user[3])/pcm[2]
  sx <- (user[2] - user[1])/pcm[1]
  nr <- max(length(x0), length(y0), length(angle), length(arr.length), 
            length(arr.width), length(lcol), length(lty), length(arr.col))
  if (nr > 1) {
    x0 <- rep(x0, length.out = nr)
    y0 <- rep(y0, length.out = nr)
    angle <- rep(angle, length.out = nr)
    arr.length <- rep(arr.length, length.out = nr)
    arr.width <- rep(arr.width, length.out = nr)
    lcol <- rep(lcol, length.out = nr)
    lty <- rep(lty, length.out = nr)
    arr.col <- rep(arr.col, length.out = nr)
  }
  RR <- rr
  for (i in 1:nr) {
    dx <- rr[, 1] * arr.length[i]
    dy <- rr[, 2] * arr.width[i]
    angpi <- angle[i]/180 * pi
    cosa <- cos(angpi)
    sina <- sin(angpi)
    RR[, 1] <- cosa * dx - sina * dy
    RR[, 2] <- sina * dx + cosa * dy
    RR[, 1] <- x0[i] + RR[, 1] * sx
    RR[, 2] <- y0[i] + RR[, 2] * sy
    panel.polygon(RR, col = arr.col[i], border = lcol[i], lty = lty[i], 
                  lwd = arr.lwd, ...)
  }
}

