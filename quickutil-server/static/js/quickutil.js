var Quickutil = Quickutil || {};

(function ($, Quickutil) {

if (Quickutil.init) return;
Quickutil.init = $.Deferred(function () { $(this.resolve); });

Quickutil.smoothScrollTo = function(el, opt_speed, opt_callback) {
    var speed = opt_speed || 300;
    var $el = $(el);
    if ($el.length !== 0) {
        var position = $el.offset().top;
        $(/safari/i.test(navigator.userAgent) ? 'body' : 'html').animate({
            scrollTop: position
        }, speed, 'swing');
    }
};

Quickutil.init.done(function() {
    $.pjax.defaults.timeout = 3 * 1000;
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

    $(document).on('click', '.category-filters ul li a', function(e) {
        var target = $(e.currentTarget);
        $('.category-filters ul li').removeClass('active');
        if (!target.hasClass('remove-filter')) {
            target.closest('li').addClass('active');
        }
    });

    $(document)
        .on('pjax:start', function() {
            $('#main').css('opacity', 0.4);
            $('.menu li a').removeClass('current');
            if (/^\/list\/?/.test(location.pathname)) {
                $('.menu li a[href="/list"]').addClass('current');
            }
            else {
                $('.menu li a[href="' + location.pathname + '"]').addClass('current');
            }
        })
        .on('pjax:success pjax:end', function() {
            $('#main').animate({ opacity: 1 }, 'fast');
            var isListPage = /^\/list\/?/.test(location.pathname);
            $('.category-filters').toggle(isListPage);
            if (isListPage) {
                $('.category-filters ul li').removeClass('active');
                var category = location.pathname.replace(/^\/list\//, '');
                $('.category-filters ul li[data-category="' + category + '"]').addClass('active');
            }
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
        var utility = target.closest('.utility');
        var source = utility.children('.source-code');

        if (source.is(':visible')) {
            target.text('Source Code');
            source.slideUp();
        }
        else if (source.text()) {
            target.text('Hide Code');
            source.slideDown();
        }
        else {
            $.get(
                '/api/source-code?utility=' + encodeURI(utility.attr('data-utility-name'))
            ).done(function(data) {
                data = data.replace(/ /g, '&nbsp').replace(/\n/g, '<br/>');
                data = prettyPrintOne(data, 'lisp');
                source.html(data);
                target.text('Hide Code');
                source.slideDown();
            });
        }
    });

    $(document).on('click', 'a[href^=#]', function(e) {
        var href = $(this).attr("href");
        if (href === '#') {
            // do nothing
            return;
        }
        if (window.history && window.history.pushState) {
            window.history.pushState(null, '', $(this).attr("href"));
            e.preventDefault();
        }
        href = href.replace(/\./g, '\\.');
        var target = $(href == "#" || href == "" ? 'html' : href);
        Quickutil.smoothScrollTo(target, undefined);
    });
});

})($, Quickutil);
