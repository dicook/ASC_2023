display_dist_cl <- function(method = "density", center = TRUE, half_range = NULL, rug = FALSE, col = "black", ...) {
  if (!requireNamespace("ash", quietly = TRUE)) {
    stop("Please install the ash package", call. = FALSE)
  }
  
  method <- match.arg(method, c("histogram", "density", "ash"))
  labels <- NULL
  init <- function(data) {
    half_range <<- tourr:::compute_half_range(half_range, data, center)
    labels <<- abbreviate(colnames(data), 2)
  }
  
  render_frame <- function() {
    par(pty = "m", mar = c(4, 4, 1, 1))
    plot(
      x = NA, y = NA, xlim = c(-1.2, 1.2), ylim = c(-1.1, 6),
      xaxs = "i", yaxs = "i",
      xlab = "Data Projection", ylab = "Density", yaxt = "n"
    )
    axis(2, seq(0, 6, by = 1))
  }
  render_transition <- function() {
    rect(-1.2, -1.1, 1.2, 6, col = "#FFFFFFE6", border = NA)
  }
  render_data <- function(data, proj, geodesic) {
    abline(h = seq(0.5, 5.5, by = 0.5), col = "grey80")
    lines(c(0, 0), c(-1, 0), col = "grey80")
    lines(c(-1, -1), c(-1.1, 0), col = "grey80")
    lines(c(1, 1), c(-1.1, 0), col = "grey80")
    
    x <- data %*% proj
    if (center) x <- center(x)
    x <- x / half_range
    
    # Render projection data
    if (method == "histogram") {
      bins <- hist(x, breaks = seq(-1, 1, 0.2), plot = FALSE)
      with(bins, rect(mids - 0.1, 0, mids + 0.1, density,
                      col = "black", border = "white"
      ))
    } else if (method == "density") {
      clrs <- unique(col)
      nclrs <- length(clrs)
      if (nclrs > 1) {
        for (i in 1:nclrs) {
          x_c <- x[col == clrs[i],]
          polygon(stats::density(x_c), lwd = 2, col = clrs[i])
        }
      }
      else {
        polygon(stats::density(x), lwd = 2, col = "black")
      }
    } else if (method == "ash") {
      utils::capture.output(ash <- ash::ash1(ash::bin1(x, c(-half_range, half_range))))
      lines(ash)
    }
    abline(h = 0)
    box(col = "grey70")
    
    if (rug) {
      segments(x, 0, x, 0.1, ...)
    }
    
    # Render tour axes
    ax <- seq_along(proj) / length(proj)
    segments(0, -ax, proj, -ax, col = "black", lwd = 3)
    text(1.0, -ax, labels, pos = 4)
  }
  
  list(
    init = init,
    render_frame = render_frame,
    render_transition = render_transition,
    render_data = render_data,
    render_target = tourr:::nul
  )
}

animate_dist_cl <- function(data, tour_path = grand_tour(1), ...) {
  animate(
    data = data, tour_path = tour_path,
    display = display_dist_cl(...), ...
  )
}
