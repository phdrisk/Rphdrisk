#' #' FUNCOES PHDRISk
#' 
#' DUAL PLOT FUNCTION
#' 
#' 
#' @param x1 coordenadas da primeira linha ( data )
#' @param y1 coordenadas da primeira linha ( data )
#' @param x2 coordenadas da segunda linha ( dados )
#' @param y2 coordenadas da segunda linha ( dados )
#' @param col cor padrao c("#C54E6D", "#009380")
#' @param lwd parametro do grafico (veja ?par)
#' @param mar parametro do grafido (veja ?par)
#' @param ylab1 rotulo entrada 1
#' @param ylab2 rotulo entrada2
#' @param nxbreaks numero de quebras no eixo horizontal
#' @param yleg1 rotulo do eixo
#' @param yleg2 rotulo do eixo
#' @param ylim1 tho numbers
#' @param ylim2 tho numbers
#' @param main tho numbers
#' @param legx tho numbers
#' @param legy tho numbers
#' @param colgrid xxxxxx
#' @param ylim.ref yyyyyy
#' @param xlab e main são para rotulo x e titulo principal como em plot ()
#' @param silent silent
#' @param bty bty
#' 
#' @import stats
#' @importFrom graphics abline axis grid legend mtext par
#' 
#' @export 
dualplot <- function(x1, y1, y2, x2 = x1, 
                     col = c("#C54E6D", "#009380"),
                     lwd = c(1, 1), colgrid = NULL,
                     mar = c(3, 6, 3, 6) + 0.1, 
                     ylab1 = paste(substitute(y1), collapse = ""), 
                     ylab2 = paste(substitute(y2), collapse = ""),
                     nxbreaks = 5, 
                     yleg1 = paste(gsub("\n$", "", ylab1), "(left axis)"), 
                     yleg2 = paste(ylab2, "(right axis)"),
                     ylim1 = NULL, ylim2 = NULL, ylim.ref = NULL,
                     xlab = "", main = NULL, legx = "topleft", legy = NULL, 
                     silent = FALSE, bty = "n", ...){
  
  # strip excess attributes (eg xts etc) from the two vertical axis variables
  ylab1 <- as.character(ylab1)
  ylab2 <- as.character(ylab2)
  y1 <- as.numeric(y1)
  y2 <- as.numeric(y2)
  
  # is ylim.ref is NULL, calculate a good default
  if(is.null(ylim.ref)){
    if (length(y1) == length(y2)){
      ylim.ref <- c(1, 1)
    } else {
      if (min(x1) >  min(x2)){
        ylim.ref <- c(1, which(abs(x2 - min(x1)) == min(abs(x2 - min(x1)))))
      } else {
        ylim.ref <- c(which(abs(x1 - min(x2)) == min(abs(x1 - min(x2)))), 1)
      }
    }
    
    
  }
  
  
  oldpar <- par(mar = mar)
  xbreaks <- round(seq(from = min(c(x1, x2)), to = max(c(x1, x2)), length.out = nxbreaks))
  
  # unless ylim1 or ylim2 were set, we set them to levels that make it equivalent
  # to a graphic drawn of indexed series (if all data positive), or to the mean
  # of each series +/- three standard deviations if some data are negative
  if(is.null(ylim1) & is.null(ylim2)){
    if(min(c(y1, y2), na.rm = TRUE) < 0){
      message("With negative values ylim1 or ylim2 need to be chosen by a method other than treating both series visually as though they are indexed. Defaulting to mean value +/- 3 times the standard deviations.")
      ylim1 <- c(-3, 3) * sd(y1, na.rm = TRUE) + mean(y1, na.rm = TRUE)
      ylim2 <- c(-3, 3) * sd(y2, na.rm = TRUE) + mean(y2, na.rm = TRUE)
    }
    
    
    if(ylim.ref[1] > length(y1)){
      stop("ylim.ref[1] must be a number shorter than the length of the first series.")
    }
    if(ylim.ref[2] > length(y2)){
      stop("ylim.ref[2] must be a number shorter than the length of the second series.")
    }
    
    if(!silent) message("The two series will be presented visually as though they had been converted to indexes.")
    
    # convert the variables to indexes (base value of 1 at the time specified by ylim.ref)
    ind1 <- as.numeric(y1) / y1[ylim.ref[1]]
    ind2 <- as.numeric(y2) / y2[ylim.ref[2]]
    
    # calculate y axis limits on the "index to 1" scale
    indlimits <- range(c(ind1, ind2), na.rm = TRUE)
    
    # convert that back to the original y axis scales
    ylim1 = indlimits * y1[ylim.ref[1]]
    ylim2 = indlimits * y2[ylim.ref[2]]
  } else {
    if(!silent) warning("You've chosen to set at least one of the vertical axes limits manually.  Up to you, but it is often better to leave it to the defaults.")
  }
  
  # draw first series - with no axes.
  plot(x1, y1, type = "l", axes = FALSE, lwd = lwd[1],
       xlab = xlab, ylab = "", col = col[1], main = main, 
       xlim = range(xbreaks), ylim = ylim1)
  
  # add in the gridlines if wanted:
  if(!is.null(colgrid)){
    grid(lty = 1, nx = NA, ny = NULL, col = colgrid)   
    abline(v = xbreaks, col = colgrid)
  }
  
  # add in the left hand vertical axis and its label
  axis(2, col = col[1], col.axis= col[1], las=1 )  ## las=1 makes horizontal labels
  mtext(paste0("\n", ylab1, "\n"), side = 2, col = col[1], line = 1.5) 
  
  # Allow a second plot on the same graph
  par(new=TRUE)
  
  # Plot the second series:
  plot(x2, y2,   xlab="", ylab="", axes = FALSE, type = "l", lwd = lwd[2],
       col = col[2], xlim = range(xbreaks), ylim = ylim2)
  
  ## add second vertical axis (on right) and its label
  mtext(paste0("\n", ylab2, "\n"), side = 4, col = col[2], line = 4.5) 
  axis(4,  col = col[2], col.axis = col[2], las=1)
  
  # Draw the horizontal time axis
  axis(1, at = xbreaks, labels = xbreaks)
  
  # Add Legend
  legend(x = legx, y = legy, legend=c(yleg1, yleg2),
         text.col = col, lty = c(1, 1), lwd = lwd, col = col,
         bty = bty, ...)
  
  par(oldpar)
}