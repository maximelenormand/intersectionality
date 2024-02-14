# Load packages
library(shiny)
library(leaflet)
library(leafpop) # sudo apt-get install pkg-config libfontconfig1-dev libcairo2-dev
library(sf)
library(RColorBrewer)
library(scales)

# Load data
load("Data.Rdata")

shinyServer(function(input, output) {

  ##### Map Profile #####################################################################################

  # Reactive expression
  reacmappro <- reactive({

    # Bbox
    bboxpro <- as.numeric(bbox[bbox[, 1] == input$CSpro, -1])

    # Shp & Popup
    shppro <- shp
    popuppro <- paste0("popups/", 1:dim(shp)[1], ".png")

    if (as.character(input$CSpro) == "METRO") {
      shppro <- shppro[as.character(shp$city) != "SAINT DENIS" & as.character(shp$city) != "FORT DE FRANCE", ]
      popuppro <- popuppro[as.character(shp$city) != "SAINT DENIS" & as.character(shp$city) != "FORT DE FRANCE"]
    }

    if (as.character(input$CSpro) != "ALL" & as.character(input$CSpro) != "METRO") {
      shppro <- shppro[as.character(shp$city) == as.character(input$CSpro), ]
      popuppro <- popuppro[as.character(shp$city) == as.character(input$CSpro)]
    }

    # Color legend
    coloproleg <- c("#1B7FAD", "#6D5B96", "#B46BA2", "#FCB13B", "#F27A2E", "#D64261")

    # Color by social group
    socpro <- shppro[, names(shppro) == input$CATpro, drop = TRUE]
    colopro <- coloproleg[socpro]

    # Barplot
    propsocpro <- propsoc[[input$CSpro]]

    res <- list(bboxpro = bboxpro, shppro = shppro, coloproleg = coloproleg, colopro = colopro, popuppro = popuppro, propsocpro = propsocpro)
  })

  # Create the base map
  output$mappro <- renderLeaflet({

    # Bbox
    minlon <- reacmappro()$bboxpro[1]
    minlat <- reacmappro()$bboxpro[2]
    maxlon <- reacmappro()$bboxpro[3]
    maxlat <- reacmappro()$bboxpro[4]

    # Map
    leaflet() %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(minlon, minlat, maxlon, maxlat) %>%
      addPolygons(
        data = reacmappro()$shppro, color = reacmappro()$colopro, weight = 2, smoothFactor = 0.5,
        opacity = 0, fillOpacity = 0.5, fillColor = reacmappro()$colopro, popup = popupImage(reacmappro()$popuppro, width = 500),
        highlightOptions = highlightOptions(color = "black", opacity = 0.7, weight = 2)
      ) #  %>%
    # addLegend(position = 'bottomleft', ## choose bottomleft, bottomright, topleft or topright
    #           colors = reacmappro()$coloproleg,
    #           labels = 1:6,
    #           opacity = 0.6,      ##transparency
    #           title = "Profiles")   ## title of the legend
  })

  # Barplot of the percentage of profile according to the social group
  output$barplotpro <- renderPlot({
    par(mar = c(10, 5, 1, 1))
    barplot(100 * t(reacmappro()$propsocpro), col = alpha(reacmappro()$coloproleg, 0.6), ylim = c(0, 100), border = NA, axes = FALSE, xlab = "", ylab = "", las = 2, cex.names = 1.25, names.arg = names$cat)
    axis(2, las = 1, cex.axis = 1.25)
    mtext("Percentage", 2, line = 3, cex = 1.5)
  })

  # Plot profile used as legend
  output$plotpro <- renderPlot({
    par(mar = c(3.5, 4.5, 1, 0.5))
    matplot(1:24, t(profiles$mu[, -1]), type = "l", lty = 1, col = alpha(reacmappro()$coloproleg, 0.6), lwd = 3, axes = FALSE, xlab = "", ylab = "")
    axis(1, at = c(3, 6, 9, 12, 15, 18, 21), las = 1, cex.axis = 1)
    axis(2, las = 1, cex.axis = 1)
    mtext("Time of day", 1, line = 2.5, cex = 1.25)
    mtext("Normalized volume", 2, line = 3.5, cex = 1.25)
    box(lwd = 1.5)
  })

  ##### Map Mismatch #####################################################################################

  # Reactive expression
  reacmapmis <- reactive({

    # Bbox
    bboxcsmis <- as.numeric(bbox[bbox[, 1] == input$CSmis, -1])

    # Shp & Popup
    shpmis <- shp
    popupmis <- paste0("popups/", 1:dim(shp)[1], ".png")

    if (as.character(input$CSmis) == "METRO") {
      shpmis <- shpmis[as.character(shp$city) != "SAINT DENIS" & as.character(shp$city) != "FORT DE FRANCE", ]
      popupmis <- popupmis[as.character(shp$city) != "SAINT DENIS" & as.character(shp$city) != "FORT DE FRANCE"]
    }

    if (as.character(input$CSmis) != "ALL" & as.character(input$CSmis) != "METRO") {
      shpmis <- shpmis[as.character(shp$city) == as.character(input$CSmis), ]
      popupmis <- popupmis[as.character(shp$city) == as.character(input$CSmis)]
    }

    # Color
    colomisleg <- colorRampPalette(brewer.pal(9, "YlOrRd"))(10)

    ind <- ((names$mismatch[1, ] == input$CATmis1) & (names$mismatch[2, ] == input$CATmis2)) | ((names$mismatch[1, ] == input$CATmis2) & (names$mismatch[2, ] == input$CATmis1))
    ind <- as.numeric(which(ind))

    mis <- shpmis[, 15:69, drop = TRUE]
    if (input$CATmis1 == input$CATmis2) {
      mis[, 1] <- 0
      mis <- as.numeric(mis[, 1])
    } else {
      mis <- mis[, ind]
    }
    mic <- as.numeric(as.character(cut(mis, breaks = c(-1, seq(0.1, 0.9, 0.1), 2), labels = 1:length(colomisleg))))
    colomis <- colomisleg[mic]

    res <- list(bboxcsmis = bboxcsmis, shpmis = shpmis, colomis = colomis, colomisleg = colomisleg, popupmis = popupmis)
  })

  # Create the base map
  output$mapmis <- renderLeaflet({

    # Bbox
    minlon <- reacmapmis()$bboxcsmis[1]
    minlat <- reacmapmis()$bboxcsmis[2]
    maxlon <- reacmapmis()$bboxcsmis[3]
    maxlat <- reacmapmis()$bboxcsmis[4]

    # Map
    leaflet() %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(minlon, minlat, maxlon, maxlat) %>%
      addPolygons(
        data = reacmapmis()$shpmis, color = reacmapmis()$colomis, weight = 2, smoothFactor = 0.5,
        opacity = 0, fillOpacity = 0.5, fillColor = reacmapmis()$colomis, popup = popupImage(reacmapmis()$popupmis, width = 500),
        highlightOptions = highlightOptions(color = "black", opacity = 0.7, weight = 2)
      ) %>%
      addLegend(
        position = "bottomleft", ## choose bottomleft, bottomright, topleft or topright
        colors = reacmapmis()$colomisleg,
        labels = c("[0,0.1]", "]0.1,0.2]", "]0.2,0.3]", "]0.3,0.4]", "]0.4,0.5]", "]0.5,0.6]", "]0.6,0.7]", "]0.7,0.8]", "]0.8,0.9]", "]0.9,1]"),
        opacity = 0.6, ## transparency
        title = "Mismatch"
      ) ## title of the legend
  })

  ##### Map Cluster #####################################################################################

  # Reactive expression
  reacmapclu <- reactive({

    # Bbox
    bboxcsclu <- as.numeric(bbox[bbox[, 1] == input$CSclu, -1])

    # Shp & Popup
    shpclu <- shp
    if (as.character(input$CSclu) == "METRO") {
      shpclu <- shpclu[as.character(shp$city) != "SAINT DENIS" & as.character(shp$city) != "FORT DE FRANCE", ]
    }
    if (as.character(input$CSclu) != "ALL" & as.character(input$CSclu) != "METRO") {
      shpclu <- shpclu[as.character(shp$city) == as.character(input$CSclu), ]
    }

    # Color legend
    colocluleg <- brewer.pal(8, "Set2")

    # Color
    clu <- shpclu$cluster
    coloclu <- colocluleg[clu]

    res <- list(bboxcsclu = bboxcsclu, shpclu = shpclu, coloclu = coloclu, colocluleg = colocluleg)
  })

  # Create the base map
  output$mapclu <- renderLeaflet({

    # Bbox
    minlon <- reacmapclu()$bboxcsclu[1]
    minlat <- reacmapclu()$bboxcsclu[2]
    maxlon <- reacmapclu()$bboxcsclu[3]
    maxlat <- reacmapclu()$bboxcsclu[4]

    # Map
    leaflet() %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(minlon, minlat, maxlon, maxlat) %>%
      addPolygons(
        data = reacmapclu()$shpclu, color = reacmapclu()$coloclu, weight = 2, smoothFactor = 0.5,
        opacity = 0, fillOpacity = 0.5, fillColor = reacmapclu()$coloclu,
        highlightOptions = highlightOptions(color = "black", opacity = 0.7, weight = 2)
      ) 
    
  })

  # Plot cluster used as legend
  output$plotclu <- renderPlot({
    
    layout(matrix(c(1, 2, 3), 2, 3, byrow = TRUE), width = c(1.9, 1, 1), height = c(1))

    par(mar = c(7, 14, 0, 0.5))
    matplot(t(rho[c(1, 2, 3), c(5, 6, 7, 2, 3, 4, 1)]), 1:7, type = "b", col = alpha(reacmapclu()$colocluleg, 0.6)[1:3], pch = 16, lty = 2, cex = 3, lwd = 1, axes = FALSE, xlab = "", ylab = "", xlim = c(-30, 30))
    axis(1, at = seq(-30, 30, 10), cex.axis = 0.75, padj = 0.3)
    axis(2, at = 1:7, labels = rev(c("Women", "65 and more", "25-34 years", "16-24 years", "Middle-high educ.", "Middle-low educ.", "Low educ.")), las = 1, cex.axis = 1.8, font = 2, padj = 0.3)

    segments(0, 0.9, 0, 7.2, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
    segments(-30, 6.5, 30, 6.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
    segments(-30, 3.5, 30, 3.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)

    par(mar = c(7, 0.5, 0, 0.5))
    matplot(t(rho[c(4, 5, 6), c(5, 6, 7, 2, 3, 4, 1)]), 1:7, type = "b", col = alpha(reacmapclu()$colocluleg, 0.6)[4:6], pch = 16, lty = 2, cex = 3, lwd = 1, axes = FALSE, xlab = "", ylab = "", xlim = c(-30, 30))
    axis(1, at = seq(-30, 30, 10), cex.axis = 0.75, padj = 0.3)
    mtext("V-Test", 1, line = 5, cex = 1.75)

    segments(0, 0.9, 0, 7.2, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
    segments(-30, 6.5, 30, 6.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
    segments(-30, 3.5, 30, 3.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)

    par(mar = c(7, 0.5, 0, 1))
    matplot(t(rho[c(7, 8), c(5, 6, 7, 2, 3, 4, 1)]), 1:7, type = "b", col = alpha(reacmapclu()$colocluleg, 0.6)[7:8], pch = 16, lty = 2, cex = 3, lwd = 1, axes = FALSE, xlab = "", ylab = "", xlim = c(-30, 30))
    axis(1, at = seq(-30, 30, 10), cex.axis = 0.75, padj = 0.3)

    segments(0, 0.9, 0, 7.2, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
    segments(-30, 6.5, 30, 6.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
    segments(-30, 3.5, 30, 3.5, lwd = 2, xpd = TRUE, col = "grey", lty = 2)
    
  })
})
