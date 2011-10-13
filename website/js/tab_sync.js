/*

tab_syn.js will look for a basic html structure and convert it to tabs.
If there are multiple sets of tab structures, it attempts to keep the selected
tab name visible accross the entire page.  If you had 2 tab controls with the
tab names "Python" and "Ruby", then clicking on the "Python" tab on either
control would bring that tab forward for all controls.  This is useful for
selecting a programming language preference and seeing all other table controls
switch to the desired preference.  The heights of tab_contents within a single
tab control are set to the maximum.

tab_sync.css should also be used.

The basic structure of a tab:

<div class="tab_sync">
    <div class="tab_content" title="Python">
        # python code
    </div>
    <div class="tab_content" title="Ruby">
        # ruby code
    </div>
</div>

An <ul> tag is created at the top of each tab_content div and the title of each
is used as the text of the tab.

<div class="tab_sync">
    <ul>
        <li class="tab_button">Python</li>
        <li class="tab_button">Ruby</li>
    </ul>
    <div class="tab_content" title="Python">
        # python code
    </div>
    <div class="tab_content" title="Ruby">
        # ruby code
    </div>
</div>
     
*/

$(function () {
    // over-engineered set logic for fun
    var set_union = function (set1, set2) {
        var new_set;
        if (set1 instanceof Array && set2 instanceof Array) {
            new_set = set1.slice();
            for (var i = 0, len = set2.length; i < len; ++i) {
                if (new_set.indexOf(set2[i]) === -1) {
                    new_set.push(set2[i]);
                }
            }
        } else if (typeof set1 === 'object' && typeof set2 === 'object') {
            new_set = {};
            var item;
            for (item in set1) { if (set1.hasOwnProperty(item)) {
                new_set[item] = set1[item];
            }}
            for (item in set2) { if (set2.hasOwnProperty(item)) {
                if (!new_set[item]) {
                    new_set[item] = set2[item];
                }
            }}
        } else {
            throw new Error("set_union given incompatible types");
        }
        return new_set;
    }
    var set_intersection = function (set1, set2) {
        var new_set;
        if (set1 instanceof Array && set2 instanceof Array) {
            new_set = [];
            for (var i = 0, len = set1.length; i < len; ++i) {
                if (set2.indexOf(set1[i]) !== -1) {
                    new_set.push(set1[i]);
                }
            }
        } else if (typeof set1 === 'object' && typeof set2 === 'object') {
            new_set = {};
            var item;
            for (item in set1) { if (set1.hasOwnProperty(item)) {
                if (set2.hasOwnProperty(item)) {
                    new_set[item] = set1[item];
                }
            }}
        } else {
            throw new Error("set_intersection given incompatible types");
        }
        return new_set;
    }
    
    // start of tab creation
    var tab_list = {};
    var tab_groups = [];
    // function invoked when clicking a tab
    var select_tab = function (title) {
        // find tab group
        for (var t = 0, tlen = tab_groups.length; t < tlen; ++t) {
            var tab_group = tab_groups[t];
            if (tab_group.indexOf(title) !== -1) {
                for (var g = 0, glen = tab_group.length; g < glen; ++g) {
                    tab_title = tab_group[g];
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
                }
                break;
            }
        }
    }
    // search for tab controls
    $('.tab_sync').each(function (i) {
        var container = $(this);
        var tabs_el = $('<ul />')
            .css({ margin: '0px' });
        var max_height = 0;
        var tabs = container.children('.tab_content');
        var group = [];
        // search for tab content
        tabs.each(function (i) {
            var tab_content = $(this);
            // create tab button
            var title = tab_content.attr('title');
            group.push(title);
            var tab_button = $('<li class="tab_button">' + title + '</li>');
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
        // set container properties
        container.prepend(tabs_el);
        // determine tab group
        var new_group = true;
        for (var t = 0, len = tab_groups.length; t < len; ++t) {
            var tab_group = tab_groups[t];
            if (set_intersection(group, tab_group).length > 0) {
                tab_groups[t] = set_union(group, tab_group);
                new_group = false;
            }
        }
        if (new_group) {
            tab_groups.push(group);
        }
        container.data('max_height', max_height);
    });
    // do height check after reflow so tab height is known
    for (var t = 0, len = tab_groups.length; t < len; ++t) {
        var tab_group = tab_groups[t];
        select_tab(tab_groups[t][0]);
    }
    $('.tab_sync').each(function (i) {
        var container = $(this);        
        container
            .css({ height: container.data('max_height') +        // content height
                           container.children().first().height() // button height
                 });
    });
});