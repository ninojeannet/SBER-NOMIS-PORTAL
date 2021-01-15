// Module to enable and disable the shiny downloadButton
// Linked to the downloadTab module defined in './modules/download_tab/download_tab.R'
const DownloadButtonState = {};

// Get download button (i.e. <a> element)
DownloadButtonState.getButton = function(id) {
    return document.querySelector(`#${CSS.escape(id)}`);
};

// Enable download button (link) by removing 'onclick' attribute if present
DownloadButtonState.enable = function(id) {
    const button = this.getButton(id);
    if (button && button.hasAttribute('onclick')) {
        button.removeAttribute('onclick');
    }
};

// Disable download button (link) by adding 'onclick="return false;"' attribute if not present
DownloadButtonState.disable = function(id) {
    const button = this.getButton(id);
    if (button && !button.hasAttribute('onclick')) {
        button.setAttribute('onclick', 'return false;');
    }
};

// Toggle download button state
DownloadButtonState.toggle = function(id, disable) {
    if (disable) {
        this.disable(id);
    } else {
        this.enable(id);
    }
};

// Create callback for the shiny 'toggleDownloadButton' custom event
DownloadButtonState.toggleCallback = function(message) {
    const {id, disable} = message;
    this.toggle(id, disable);
};