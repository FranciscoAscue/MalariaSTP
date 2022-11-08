$(document).on("shiny:connected", function(){
  $("#map").css({
    width: window.innerWidth, 
    height: window.innerHeight
  });
  $(window).on("resize", function(e){
    if(e.target instanceof Window){
      $("#map").css({width: window.innerWidth, height: window.innerHeight});
    }
  });
})