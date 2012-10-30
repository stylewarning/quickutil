var Quickutil = Quickutil || {};

(function ($, Quickutil) {

if (Quickutil.init) return;
Quickutil.init = $.Deferred(function () { $(this.resolve); });

Quickutil.init.done(function() {
    var updateFilter = function() {
        var words = $(this).val().split(/\s+/);
        var i = 0;
        $('.utility').each(function() {
            var name = $(this).attr('data-utility-name');
            if (_.all(words, function(word) { return name.indexOf(word) >= 0; })) {
                $(this).show();
                ++i;
            }
            else {
                $(this).hide();
            }
        });
        $('.filter-result-description .count').text(i);
    };
    $(document).on('input', '.filter', updateFilter);
    $.proxy(updateFilter, $('.filter'))();

    $('.filter').focus();
});

})($, Quickutil);
