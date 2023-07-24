
$(document).ready(function() {
    
    // Show the loading screen when the app starts
    $('.wrapper').append('<div class="greyed-out-main"><div class="loader"></div></div>');

    // This function will be called from Shiny to hide the loading screen
    Shiny.addCustomMessageHandler('hideLoadingScreen', function(message) {
      $('.greyed-out-main').remove();
    });
    
    //Adjust menu
    if (!$('#mainTitle').prev().hasClass('sidebar-toggle')) {
      $('.sidebar-toggle').insertBefore('#mainTitle');
      var bars = '<div class="hb-menu-bar"></div>'.repeat(3);
      $('.sidebar-toggle').append(bars);
    }
});

/*
var isCollapsed = false;
var autoCollapsed = false; // Flag to track if the sidebar was automatically collapsed

$(document).ready(function() {
    // Show the loading screen when the app starts
    $('.wrapper').append('<div class="greyed-out-main"><div class="loader"></div></div>');

    // This function will be called from Shiny to hide the loading screen
    Shiny.addCustomMessageHandler('hideLoadingScreen', function(message) {
      $('.greyed-out-main').remove();
    });
    
    // Toggle sidebar
    $('#sidebarToggle').off('click'); // Remove any previously bound click events
    $('#sidebarToggle').click(function() {
      if (!isCollapsed) {
        $('.main-sidebar').css("width", 0);
        $('.content-wrapper').css("margin-left", 0);
        $('#sidebarCollapsed').css("display", "none");
        isCollapsed = true;
      } else {
        isCollapsed = false;
        $('#sidebarCollapsed').css("display", "block");
        $('.content-wrapper').css("margin-left", "230px");
        $('.main-sidebar').css("width", "230px");
      }
  });
});

$(window).resize(function() {
    if ($(window).width() <= 770) {
        if (!autoCollapsed && !isCollapsed && $('#sidebarCollapsed').css("display") != "none") {
          // Apply your custom styles and behaviors for small screens
          $('.main-sidebar').css("width", 0);
          $('.content-wrapper').css("margin-left", 0);
          $('#sidebarCollapsed').css("display", "none");
          isCollapsed = true;
        }
        autoCollapsed = true; // Set the flag to true when automatically collapsing
    } else {
        if (autoCollapsed && isCollapsed && $('#sidebarCollapsed').css("display") === "none") {
          // Revert to default styles and behaviors for larger screens
          $('.content-wrapper').css("margin-left", "230px");
          isCollapsed = false;
          $('#sidebarCollapsed').css("display", "block");
          $('.main-sidebar').css("width", "230px");
        }
        autoCollapsed = false; // Reset the flag when reverting to larger screen styles
    }
});

*/

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