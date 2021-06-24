# code mostly taken from here:
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization 

M <- cor(df[,loc$dvnames], use = "complete.obs")
corrplot( M
        , type   = "upper"
        , method = "ellipse", order = "hclust", diag = FALSE, las = 2)
