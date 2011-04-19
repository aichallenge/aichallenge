var Engine = {};

Engine.run_game = function (game, bots, options) {
    var turns = options.turns || 1000;
    for (var turn = 0; turn <= turns; ++turn) {
        try {
            // start game
            if (turn === 0) {
                game.start_game();
            }
            // get moves from each bot
            var bot_moves = [];
            for (var b = 0, blen = bots.length; b < blen; ++b) {
                var bot = bots[b];
                var output = null;
                if (game.is_alive(b)) {
                    if (turn == 0) {
                        var start = game.get_player_start(b) + 'ready\n';
                        output = bot.input(start);
                    }
                } else {
                    var state = ('turn ' + turn + '\n' +
                                  game.get_player_state(b) + 'go\n');
                    output = bot.input(state);
                }
                bot_moves.push(output);
            }
            // start turn
            if (turn > 0) {
                game.start_turn();
            }
            // process moves
            var bot_alive = [];
            if (turn > 0 && !game.game_over()) {
                for (var b = 0, blen = bots.length; b < blen; ++b) {
                    if (game.is_alive(b)) {
                        game.do_moves(b, bot_moves[b]);
                    }
                }
                game.finish_turn();
            }
            // check for 1 bot left
            var bots_alive = 0;
            for (var b = 0, blen = bots.length; b < blen; ++b) {
                if (game.is_alive(b)) {
                    bots_alive += 1;
                }
            }
            if (bots_alive <= 1) {
                break;
            }
        } catch (err) {
            alert(err.name + ': ' + err.message);
            break;
        }
    }
}