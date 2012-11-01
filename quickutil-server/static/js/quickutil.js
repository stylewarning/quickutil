var Quickutil = Quickutil || {};

(function ($, Quickutil) {

if (Quickutil.init) return;
Quickutil.init = $.Deferred(function () { $(this.resolve); });

Quickutil.init.done(function() {
    var updateFilter = function() {
        if ($(this).length === 0) return;

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

    $('.favorite, .unfavorite').on('click', function(e) {
        var target = $(e.currentTarget);
        var action = target.hasClass('favorite') ? 'favorite' : 'unfavorite';
        var name = target.closest('.utility').attr('data-utility-name');
        $.ajax({
            type: 'POST',
            url: '/api/' + action + '.json',
            data: { utility: name }
        });

        target.text(action === 'favorite' ? 'star' : 'starempty');
    });
});

})($, Quickutil);
