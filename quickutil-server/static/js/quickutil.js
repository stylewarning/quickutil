var Quickutil = Quickutil || {};

(function ($, Quickutil) {

if (Quickutil.init) return;
Quickutil.init = $.Deferred(function () { $(this.resolve); });

Quickutil.init.done(function() {
    $.pjax.defaults.timeout = 20 * 1000;
    $.pjax.defaults.contentType = 'text/html';

    $(document).on('click', 'a[data-pjax]', function(e) {
        if (e.isDefaultPrevented()) return;
        var target = $(this);
        if (target.hasClass('disabled')) return;
        $.pjax({
            url: target.attr('href'),
            fragment: target.attr('data-pjax'),
            container: target.attr('data-pjax'),
            target: target.get(0)
        });
        e.preventDefault();
    });

    $(document).on('click', '.menu li a', function(e) {
        var target = $(e.currentTarget);
        $('.menu li').removeClass('current');
        target.closest('li').addClass('current');
    });
    $('#header h1').on('click', function() {
        $('.menu li').removeClass('current');
        $('.menu li a[href="/"]').parent().addClass('current');
    });

    $(document).on('pjax:success', function() { prettyPrint(); });

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
