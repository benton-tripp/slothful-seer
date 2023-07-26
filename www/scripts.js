
$(document).ready(function() {
    // Show the loading screen when the app starts
    $('.wrapper').append('<div class="greyed-out-main"><div class="loader"></div></div>');

    // This function will be called from Shiny to hide the loading screen
    Shiny.addCustomMessageHandler('hideLoadingScreen', function(message) {
      $('.greyed-out-main').remove();
    });
    
    // Adjust menu
    if (!$('#mainTitle').prev().hasClass('sidebar-toggle')) {
      $('.sidebar-toggle').insertBefore('#mainTitle');
      var bars = '<div class="hb-menu-bar"></div>'.repeat(3);
      $('.sidebar-toggle').append(bars);
    }
});


// Trigger menu items on model info page
$(document).on('click', 'a[id^="mdl"]', function(evt) {
  var id = $(this).attr('id');
  Shiny.setInputValue("menuItemSelected", id, {priority: 'event'});
});

// Greying-out function
shinyjs.loadingPanel = function() {
    $('.wrapper').append('<div class="greyed-out"></div>');
};

// Loading panel/spinner remove function
shinyjs.finishedLoadingPanel = function() {
    $('.greyed-out').remove();
};

