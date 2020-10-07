// This recieves messages of type "testmessage" from the server.
Shiny.addCustomMessageHandler("testmessage",
  function(message) {
    alert(JSON.stringify(message));
  }
);

/*
$(document).ready(function(){
  console.log('test2')
  $('#uploaded_progress').on("DOMSubtreeModified",function(){
    console.log('test')
    var target = $('#uploaded_progress').children()[0];
    if(target.innerHTML === "Upload complete"){
        console.log('Change')
        target.innerHTML = 'YOUR TEXT HERE';      
    }

  });
});
*/