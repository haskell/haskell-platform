$(document).ready(function() {
    $('#package-filter').keyup(function() {
        var query = $(this).val().toLowerCase();
        if (query == '') {
            $('.history tbody tr').show();
        } else {
            $(".history tbody td:first-child").filter(function() {
                return $(this).text().toLowerCase().indexOf(query) == -1;
            }).parent().hide();
        }
    });
});
