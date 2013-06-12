var Quickutil = Quickutil || {};

(function ($, Quickutil) {

if (Quickutil.init) return;
Quickutil.init = $.Deferred(function () { $(this.resolve); });

Quickutil.init.done(function() {
    prettyPrint();

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
    $(document).on('click', '.utility .category', function(e) {
        var target = $(e.target);
        $('.category-filters ul li').removeClass('active');
        $('.category-filters ul li a[href="' + target.attr('href') + '"]').closest('li').addClass('active');
    });

    $(document).on('click', '.menu li a', function(e) {
        var target = $(e.currentTarget);
        $('.menu li a').removeClass('current');
        $('.menu li a[href="' + location.pathname + '"]').addClass('current');
    });
    $('#header h1').on('click', function() {
        $('.menu li a').removeClass('current');
        $('.menu li a[href="/"]').addClass('current');
    });
    $(document).on('click', '.category-filters ul li a', function(e) {
        var target = $(e.currentTarget);
        $('.category-filters ul li').removeClass('active');
        if (!target.hasClass('remove-filter')) {
            target.closest('li').addClass('active');
        }
    });

    $(document)
        .on('pjax:start', function() { $('#main').css('opacity', 0.4); })
        .on('pjax:end', function() { $('#main').animate({ opacity: 1 }, 'fast'); })
        .on('pjax:success', function() {
            prettyPrint();
            $('.category-filters').toggle(/^\/list/.test(location.pathname));
        });

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

    $(document).on('submit', '.favorite, .unfavorite', function(e) {
        e.preventDefault();
        var form = $(e.currentTarget);
        var button = form.children('input[type="submit"]');
        var isFavorite = form.hasClass('favorite');
        $.ajax({
            type: 'POST',
            url: form.attr('data-action'),
            data: form.serializeArray()
        });

        form.attr('data-action', '/api/' + (isFavorite ? 'favorite' : 'unfavorite') + '.json');
        button.attr('value', isFavorite ? 'starempty' : 'star');
        form.toggleClass('favorite').toggleClass('unfavorite');
    });
    $(document).on('click', '.show-source-code', function(e) {
        e.preventDefault();
        var target = $(e.target);
        var source = target.closest('.utility').children('.source-code');
        if (source.is(':visible')) {
            target.text('Source Code');
            source.slideUp();
        }
        else {
            target.text('Hide Code');
            source.slideDown();
        }
    });
});

})($, Quickutil);
