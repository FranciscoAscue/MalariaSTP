function get_id(clicked_id) {
     Shiny.setInputValue("current_id", clicked_id, {priority: "event"});
}
$(document).keyup(function(event) {if ($("#ID").is(":focus") && (event.key == "Enter")) { $("#Guardar").click();}});
