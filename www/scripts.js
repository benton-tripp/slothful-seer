$(document).ready(function() {
    // Show the loading screen when the app starts
    $('.wrapper').append('<div class="greyed-out-main"><div class="loader"></div></div>');

    // This function will be called from Shiny to hide the loading screen
    Shiny.addCustomMessageHandler('hideLoadingScreen', function(message) {
      $('.greyed-out-main').remove();
    });
  });

$(document).on('click', 'a[id^="mdl"]', function(evt) {
  var id = $(this).attr('id');
  Shiny.setInputValue("menuItemSelected", id, {priority: 'event'});
});

shinyjs.loadingPanel = function() {
    console.log("Greyed-out");
    $('.wrapper').append('<div class="greyed-out"></div>');
};

shinyjs.finishedLoadingPanel = function() {
    $('.greyed-out').remove();
};