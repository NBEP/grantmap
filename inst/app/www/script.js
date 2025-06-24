$( document ).ready(function() {
  var containerHeight = $(".navbar .container-fluid").height() + "px";
  $(".navbar .container-fluid")
  .append(
    "<a href='https://www.nbep.org'><img id = 'headerLogo' " 
    + "alt = 'Narragansett Bay Estuary Program' " 
    + "src='www/NBEP_logo_wide.png' align='right' height = '" + 
    $(".navbar .container-fluid").height() + "px'></a>"
  );
});
