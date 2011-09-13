local ants = require "Ants"

local bot = {}

function bot:onReady()
	ants:finishTurn()
end

function bot:onTurn()
	local myAnts = ants:myAnts()
	local directions = { "N", "E", "S", "W" }
	for _,ant in ipairs(myAnts) do
		for _,dir in ipairs(directions) do
			if ants:passable(ant.row, ant.col, dir) then
				ants:issueOrder(ant.row, ant.col, dir)
				break
			end
		end
	end
	ants:finishTurn()
end

function bot:onEnd()
end

ants:start(bot)
