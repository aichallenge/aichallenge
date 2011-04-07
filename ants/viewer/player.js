$(function () {
    options = {
            'turns': 100,
            'output_stream': true,
            'map': 'rows 10\ncols 10\n' +
                   'm a....d....\n' + 
                   'm ..........\n' + 
                   'm ..........\n' + 
                   'm ..........\n' + 
                   'm ..........\n' + 
                   'm b....c....\n' + 
                   'm ..........\n' + 
                   'm ..........\n' + 
                   'm ..........\n' + 
                   'm ..........\n'
    };
    var ants_game = Object.create(AntsGame);
    ants_game.init(options);
    var num_players = 4;
    var bots = [];
    for (var b = 0; b < num_players; ++b) {
        bots.push(Object.create(HunterBot));
    }
    var engine = Object.create(Engine);
    var output = engine.run_game(ants_game, bots, options);
    $('#output').html(output);
});