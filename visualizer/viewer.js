$(function () {
    var zeroFill = function (number, width) {
        width -= number.toString().length;
        if (width > 0) {
            return new Array(width + (/\./.test(number) ? 2 : 1)).join('0') + number;
        }
        return number;
    }
    var map = $('#map');
    var turn = 1;
    var anno = 'start';
    var mapSrc = function () {
        return 'playback/frame' + zeroFill(turn, 5) + anno + '.png';
    }
    map.src = mapSrc(turn);
    $('#back').click(function () {
        turn -= 1;
        if (turn === 0) {
            turn = 1;
        }
        map.attr('src', mapSrc());
        $('#turn').html(turn);
    });
    $('#forward').click(function () {
        turn += 1;
        map.attr('src', mapSrc());
        $('#turn').html(turn);
    });
    $('#start').click(function () {
        anno = 'start';
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
    $('#player4').click(function () {
        anno = 'player4';
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
});
