$(function () {
    var zeroFill = function (number, width) {
        width -= number.toString().length;
        if (width > 0) {
            return new Array(width + (/\./.test(number) ? 2 : 1)).join('0') + number;
        }
        return number;
    }
    var map = $('#map');
    var turn = 0;
    var anno = 'frame';
    var mapSrc = function () {
        return '../playback/' + anno + '_' + zeroFill(turn, 5) + '.png';
    }
    map.src = mapSrc(turn);
    
    var forward = function () {
        turn += 1;
        map.attr('src', mapSrc());
        $('#turn').html(turn);
    };
    
    var backward = function () {
        turn -= 1;
        if (turn <= 0) {
            turn = 0;
        }
        map.attr('src', mapSrc());
        $('#turn').html(turn);
    };
    
    $('#back').click(backward);
    $('#forward').click(forward);
    $('#start').click(function () {
        anno = 'frame';
        map.attr('src', mapSrc());
    });
    $('#move').click(function () {
        anno = 'move';
        map.attr('src', mapSrc());
    });
    $('#player1').click(function () {
        anno = 'player1';
        map.attr('src', mapSrc());
    });
    $('#player2').click(function () {
        anno = 'player2';
        map.attr('src', mapSrc());
    });
    $('#player3').click(function () {
        anno = 'player3';
        map.attr('src', mapSrc());
    });
    $('#player0').click(function () {
        anno = 'player0';
        map.attr('src', mapSrc());
    });
    $('#death').click(function () {
        anno = 'death';
        map.attr('src', mapSrc());
    });
    $('#birth').click(function () {
        anno = 'birth';
        map.attr('src', mapSrc());
    });
    $('#food').click(function () {
        anno = 'food';
        map.attr('src', mapSrc());
    });
    $(document.documentElement).keydown(function (evt) {
        if (evt.keyCode == '37') { // Left Arrow
            backward();
            return false;
        } else if(evt.keyCode == '39') { // Right Arrow
            forward();
            return false;
        }
    }); 
});
