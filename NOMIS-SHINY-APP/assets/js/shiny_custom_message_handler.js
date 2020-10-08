// This recieves messages of type "testmessage" from the server.
Shiny.addCustomMessageHandler("testmessage",
  function(message) {
    alert(JSON.stringify(message));
  }
);


$(document).ready(function(){
  $('.progress-bar').on("DOMSubtreeModified",function(){
    var target = document.querySelector(".progress-bar");
    if(target.textContent === "Upload complete"){
        target.textContent = 'Files selected';      
    }

  });
});
