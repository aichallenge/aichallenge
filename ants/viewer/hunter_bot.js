var HunterBot = Object.create(AntBot);

HunterBot.do_turn = function () {
    var destinations = [];
    var dest_clear = function (loc) {
        for (var d = 0, dlen = destinations.length; d < dlen; ++d) {
            if (destinations[d][0] === loc[0] &&
                destinations[d][1] === loc[1]) {
                return false;
            }
        }
        return true;
    }
    var ants = this.my_ants();
    var targets = this.enemy_ants().concat(this.food());
    for (var a = 0, alen = ants.length; a < alen; ++a) {
        var ant = ants[a];
        var closest_target = null;
        var closest_distance = 999999;
        for (var t = 0, tlen = targets.length; t < tlen; ++t) {
            var target = targets[t];
            var dist = this.distance(ant, target);
            if (dist < closest_distance) {
                closest_distance = dist;
                closest_target = target;
            }
        }
        if (closest_target === null) {
            destinations.push(ant);
            continue;
        }
        var directions = this.direction(ant, closest_target);
        var moved = false;
        for (var d = 0; d < directions.length; ++d) {
            var direction = directions[d];
            var destination = this.destination(ant, direction);
            if (this.unoccupied(destination) && dest_clear(destination)) {
                destinations.push(destination);
                this.issue_order([ant, direction]);
                moved = true;
                break;
            }
        }
        if (!moved) {
            destinations.push(ant);
        }
    }
};