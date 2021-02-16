
Shiny.addCustomMessageHandler('addHoverWidget', PointHoverWidget.addWidgetCallback.bind(PointHoverWidget));
Shiny.addCustomMessageHandler('removeHoverWidget', PointHoverWidget.removeWidgetCallback.bind(PointHoverWidget));

// This recieves messages of type "testmessage" from the server.
Shiny.addCustomMessageHandler('toggledlbutton', DownloadButtonState.toggleCallback.bind(DownloadButtonState));

      
$(document).ready(function(){
  $('.progress-bar').on("DOMSubtreeModified",function(){
    var target = document.querySelector(".progress-bar");
    if(target.textContent === "Upload complete"){
        target.textContent = 'Files selected';      
    }

  });
});
