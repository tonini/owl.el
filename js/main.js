var options = {
    valueNames: ['signature']
};

var documentationList = new List('documentation-list', options);

$(function(){
  $('.view_source_link').click(function(event) {
    event.preventDefault();
    $(this).parent('.view_source').next('.source').toggle();
  });
});
