// Module to create and delete hover widget (and click widget) on ggplot2 plots in Shiny
// Linked to the pointHoverWidgetServer R function defined in './utils/shiny_extensions.R'
const PointHoverWidget = {};

// Get a shiny plotOuput element by its id (CSS.escape in case of numbered ids)
PointHoverWidget.getPlot = function(id) {
    return document.querySelector(`#${CSS.escape(id)}`);
};

// Get the widget element from a shiny plotOuput
PointHoverWidget.getPlotWidget = function(id) {
    return this.getPlot(id).querySelector('.point-hover-widget');
};

// Build the widget value information element
PointHoverWidget.buildValueInfo = function(axis, name, value) {
    const valueElement = document.createElement('span');
    valueElement.className = `${axis}__value value`;
    valueElement.textContent = value;

    const nameElement = document.createElement('span');
    nameElement.className = `${axis}__name name`;
    nameElement.textContent = `${name}:`;

    const info = document.createElement('span');
    info.className = `${axis} point-info`;
    info.appendChild(nameElement);
    info.appendChild(valueElement);
    return info;
};

// Build and a widget to the plot
PointHoverWidget.addWidget = function(plotId, pointInfo, mapping, coords_css, coords_img, range, x_y_labels) {
    // Build bubble with infos
    const bubble = document.createElement('div');
    bubble.className = 'bubble';
    const idName = Object.keys(pointInfo)[0];
    const idDisplay = idName.charAt(0).toUpperCase() + idName.slice(1);
    const value = pointInfo[idName]
    bubble.appendChild(this.buildValueInfo(idName, idDisplay, value));
    bubble.appendChild(this.buildValueInfo('x', x_y_labels.x, pointInfo[mapping.x]));
    bubble.appendChild(this.buildValueInfo('y', x_y_labels.y, pointInfo[mapping.y]));

    // Get plot
    const plot = this.getPlot(plotId);

    // Build widget
    const widget = document.createElement('div');
    widget.className = 'point-hover-widget';
    // Adjust position so that the bubble arrow point the right place
    // Using the padding and position defined in CSS
    // Adjust orientation depending on the right or left position
    const middle = (range.right - range.left) / 2 + range.left;
    if (coords_img.x <= middle) {
        widget.style.left = `${coords_css.x - 25}px`;
        bubble.classList.add('left');
    } else if (coords_img.x > middle){
        widget.style.right = `${plot.clientWidth - coords_css.x - 25}px`;
        bubble.classList.add('right');
    }
    widget.style.bottom = `${400 - coords_css.y + 10}px`;
    widget.appendChild(bubble);

    // Add widget to plot
    plot.appendChild(widget);
};

// Remove widget from plot, only if a widget is present
PointHoverWidget.removeWidget = function(plotId) {
    if (this.getPlotWidget(plotId)) {
        this.getPlot(plotId).removeChild(this.getPlotWidget(plotId));
    }
};

// Check if the currently displayed widget infos are equal to the newly received ones
PointHoverWidget.inputEqualCurrentWidget = function(currentWidget, x, y, site) {
    return (currentWidget.querySelector('.x__value').textContent === x
            &&
            currentWidget.querySelector('.y__value').textContent === y.toString()
            &&
            currentWidget.querySelector('.site__value').textContent === site);
};

// Check if the widget need to be updated
PointHoverWidget.needUpdate = function(plotId, pointInfo, mapping) {
    const currentWidget = this.getPlotWidget(plotId);

    return (currentWidget !== null && this.inputEqualCurrentWidget(currentWidget, pointInfo[mapping.x], pointInfo[mapping.y], pointInfo.Site_ID))
    ? false : true;
};

// Create a callback function to for the shiny 'addHoverWidget' custom event
PointHoverWidget.addWidgetCallback = function(message) {
    // Destructure message
    const {pointInfo, mapping, coords_css, coords_img, range, x_y_labels, plotId} = message;

    // If widget need to be updated
    // Remove old one and add new one
    if (this.needUpdate(plotId, pointInfo, mapping)) {
        this.removeWidget(plotId);
        this.addWidget(plotId, pointInfo, mapping, coords_css, coords_img, range, x_y_labels);
    }
};

// Create a callback function to for the shiny 'removeHoverWidget' custom event
// Remove curretn widget
PointHoverWidget.removeWidgetCallback = function(message) {
    this.removeWidget(message.plotId);
};
