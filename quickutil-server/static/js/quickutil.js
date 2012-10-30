var Quickutil = Quickutil || {};

(function ($, Quickutil) {

if (Quickutil.init) return;
Quickutil.init = $.Deferred(function () { $(this.resolve); });

Quickutil.init.done(function() {
    $(document).on('input', '.filter', function(e) {
        var word = $(this).val();
        var i = 0;
        $('.utility').each(function() {
            if ($(this).attr('data-utility-name').indexOf(word) >= 0) {
                $(this).show();
                ++i;
            }
            else {
                $(this).hide();
            }
        });
        $('.filter-result-description .count').text(i);
    });
});

})($, Quickutil);
