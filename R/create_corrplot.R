# Define the function
create_corrplot <- function(pred.corr, cc, type = "lower", title = NULL, mar = NULL
                            # method = c("circle", "square", "ellipse", "number", "shade", "color", "pie"), type = c("full", "lower", "upper"), add = FALSE, col = NULL, bg = "white", title = "", is.corr = TRUE, diag = TRUE, outline = FALSE, mar = c(0, 0, 0, 0), addgrid.col = NULL, addCoef.col = NULL, addCoefasPercent = FALSE, order = c("original", "AOE", "FPC", "hclust", "alphabet"), hclust.method = c("complete","ward", "ward.D", "ward.D2", "single", "average", "mcquitty", "median", "centroid"), addrect = NULL, rect.col = "black", rect.lwd = 2, tl.pos = NULL, tl.cex = 1, tl.col = "red", tl.offset = 0.4, tl.srt = 90, cl.pos = NULL, cl.lim = NULL, cl.length = NULL, cl.cex = 0.8, cl.ratio = 0.15, cl.align.text = "c", cl.offset = 0.5, number.cex = 1, number.font = 2, number.digits = NULL, addshade = c("negative", "positive", "all"), shade.lwd = 1, shade.col = "white", p.mat = NULL, sig.level = 0.05, insig = c("pch", "p-value", "blank", "n", "label_sig"), pch = 4, pch.col = "black", pch.cex = 3, plotCI = c("n", "square", "circle", "rect"), lowCI.mat = NULL, uppCI.mat = NULL, na.label = "?", na.label.col = "black", win.asp = 1, ...
                            ) {

  # Create a correlation plot
  corrplot::corrplot(pred.corr, method = "color", type = type, mar = mar,
                     addCoef.col = 'black', cl.pos = 'b', diag = FALSE, title = title)

  # Add annotations
  if (type == "lower"){
    for (i in 1:nrow(pred.corr)) {
      for (j in 1:ncol(pred.corr)) {
        if ((pred.corr[i, j] > cc | pred.corr[i, j] < -cc) && pred.corr[i, j] != 1 && i < j) {
          text(i + 0.4, nrow(pred.corr) + 1.25 - j, "*", col = "black", cex = 1.5)
        }
      }
    }
  } #else {
    # for (i in 1:nrow(pred.corr)) {
      # for (j in 1:ncol(pred.corr)) {
        # if ((pred.corr[i, j] > cc | pred.corr[i, j] < -cc) && pred.corr[i, j] != 1 && i < j) {
          # text(i + 0.4, nrow(pred.corr) + 1.25 - j, "*", col = "black", cex = 1.5)
        # }
      # }
    # }
  # }
}
