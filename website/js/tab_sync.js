$(function () {
    var tab_list = {};
    var select_tab = function (title) {
        for (tab_title in tab_list) { if (tab_list.hasOwnProperty(tab_title)) {
            for (var i = 0, len = tab_list[tab_title].length; i < len; ++i) {
                if (tab_title === title) {
                    tab_list[tab_title][i].content
                        .css({ 'display': '' });
                    tab_list[tab_title][i].button
                        .addClass('tab_selected');;
                } else {
                    tab_list[tab_title][i].content
                        .css({ 'display': 'none' });
                    tab_list[tab_title][i].button
                        .removeClass('tab_selected');;
                }
            }
        }}
    }
    var first_title = '';
    $('.tab_sync').each(function (i) {
        var container = $(this);
        var tabs_el = $('<ul />')
            .css({ margin: '0px' });
        var max_height = 0;
        var tab_height = 0;
        var tabs = container.children('.tab_content');
        tabs.each(function (i) {
            var tab_content = $(this);
            // create tab button
            var title = tab_content.attr('title');
            if (first_title === '') {
                first_title = title;
            }
            var tab_button = $('<li class="tab_button"><h4>' + title + '</h4></li>');
            // save tab button with content
            if (!tab_list[title]) {
                tab_list[title] = [{button: tab_button, 'content': tab_content}];
            } else {
                tab_list[title].push({button: tab_button, content: tab_content});
            }
            // add functionality to button
            tab_button
                .appendTo(tabs_el)
                .click(function () {
                    select_tab(title);
                });
            // calc max height
            if (max_height < tab_content.height()) {
                max_height = tab_content.height()
            }
        });
        // set heights to same max value
        tabs.each(function (i) {
            var tab_content = $(this);
            tab_content.height(max_height);
        });
        // set active tabs
        select_tab(first_title);
        // set container properties

        container.prepend(tabs_el);
    });
    // do height check afterward so tab height is known
    $('.tab_sync').each(function (i) {
        var container = $(this);        
        container
            .css({ height: max_height + tab_list[first_title][0].button.height() });
    });
});