$(document).on('click', '.survey-btn', function() {
  // Remove 'selected' from other buttons in the same group
  $(this).siblings().removeClass('selected');
  // Add 'selected' to the clicked button
  $(this).addClass('selected');
});