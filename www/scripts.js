$(document).on('click', 'a[id^="m"]', function(evt) {
  var id = $(this).attr('id');
  Shiny.setInputValue("menuItemSelected", id, {priority: 'event'});
});

shinyjs.loadingPanel = function(){
    $('.wrapper').append('<div class="greyed-out></div>');
};

shinyjs.finishedLoadingPanel = function() {
    $('.greyed-out').remove();
};