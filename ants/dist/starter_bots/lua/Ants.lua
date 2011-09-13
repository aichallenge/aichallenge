
local ants = {
	bot = nil,
	currentTurn = -1,
	config = {},
	map = {},
	orders = {},
	ants = {},
	food = {},
	landTypes = {
		LAND = 0,
		DEAD = 1,
		ANT = 2,
		WATER = 3,
		FOOD = 4
	}
}

function ants:start(botInput)
	self.bot = botInput
	for line in io.stdin:lines() do
		self:processLine(line)
	end
end

function ants:processLine(line)
	local type, values = line:match("^%s*(%S+)%s*(.-)%s*$")
	
	if type == "ready" then
		for row = 0, self.config.rows - 1 do
			for col = 0, self.config.cols - 1 do
				if col == 0 then
					self.map[row] = {}
				end
				self.map[row][col] = { type = self.landTypes.LAND }
			end
		end
		self.bot:onReady()
	elseif type == "go" then
		self.bot:onTurn()
	elseif type == "end" then
		self.bot:onEnd()
	elseif type == "turn" then
		self.currentTurn = tonumber(values)
		if self.currentTurn > 0 then
			-- Reset map except for water:
			for row = 0, self.config.rows - 1 do
				for col = 0, self.config.cols - 1 do
					if self.map[row][col].type ~= self.landTypes.WATER then
						self.map[row][col] = { type = self.landTypes.LAND }
					end
				end
			end
			self.ants = {}
			self.food = {}
		end
	elseif type == "w" then
		local row, col = values:match("^(%d+) (%d+)")
		row, col = tonumber(row), tonumber(col)
		self.map[row][col] = { type = self.landTypes.WATER, data = {} }
	elseif type == "f" then
		local row, col = values:match("^(%d+) (%d+)")
		row, col = tonumber(row), tonumber(col)
		self.map[row][col] = { type = self.landTypes.FOOD, data = {} }
		table.insert(self.food, { row = row, col = col })
	elseif type == "r" then
		local row, col = values:match("^(%d+) (%d+)")
		row, col = tonumber(row), tonumber(col)
		self.map[row][col] = { type = self.landTypes.LAND, data = {} } -- Introduce new landtype?
	elseif type == "a" then
		local row, col, owner = values:match("^(%d+) (%d+) (%d+)")
		row, col, owner = tonumber(row), tonumber(col), tonumber(owner)
		self.map[row][col] = { type = self.landTypes.ANT, data = { owner = owner } }
		table.insert(self.ants, { row = row, col = col, owner = owner })
	elseif type == "d" then
		local row, col, owner = values:match("^(%d+) (%d+) (%d+)")
		row, col, owner = tonumber(row), tonumber(col), tonumber(owner)
		self.map[row][col] = { type = self.landTypes.DEAD, data = { owner = owner } }
	elseif type ~= nil and self.currentTurn == 0 then
		-- On turn 0, we receive some (integer) settings
		self.config[type] = tonumber(values)
	end
end

function ants:issueOrder(row, col, direction)
	table.insert(self.orders, { row = row, col = col, direction = direction })
end

function ants:finishTurn()
	for _,order in ipairs(self.orders) do
		print("o " .. (order.row) .. " " .. (order.col) .. " " .. order.direction)
	end
	self.orders = {}
	print("go")
	io.stdout:flush()
end

function ants:tileInDirection(row, col, direction)
	local rowd = 0
	local cold = 0
	if direction == "N" then
		rowd = -1
	elseif direction == "E" then
		cold = 1
	elseif direction == "S" then
		rowd = 1
	elseif direction == "W" then
		cold = -1
	end
	local newrow = row + rowd
	local newcol = col + cold
	if newrow < 0 then
		newrow = self.config.rows - 1
	elseif newrow > self.config.rows - 1 then
		newrow = 0
	end
	if newcol < 0 then
		newcol = self.config.cols - 1
	elseif newcol > self.config.cols - 1 then
		newcol = 0
	end
	return self.map[newrow][newcol]
end

function ants:myAnts()
	local result = {}
	for _,ant in ipairs(self.ants) do
		if ant.owner == 0 then
			table.insert(result, ant)
		end
	end
	return result
end

function ants:enemyAnts()
	local result = {}
	for _,ant in ipairs(self.ants) do
		if ant.owner ~= 0 then
			table.insert(result, ant)
		end
	end
	return result
end

function ants:passable(row, col, direction)
	return self:tileInDirection(row, col, direction).type ~= self.landTypes.WATER
end

function ants:distance(fromRow, fromCol, toRow, toCol)
	local dr = math.min(math.abs(fromRow - toRow), self.config.rows - math.abs(fromRow - toRow))
	local dc = math.min(math.abs(fromCol - toCol), self.config.cols - math.abs(fromCol - toCol))
	return math.sqrt((dr * dr) + (dc * dc))
end

function ants:direction(fromRow, fromCol, toRow, toCol)
	local d = {}
	fromRow = fromRow % self.config.rows
	toRow = toRow % self.config.rows
	fromCol = fromCol % self.config.cols
	toCol = toCol % self.config.cols

	if fromRow < toRow then
		if toRow - fromRow >= self.config.rows / 2 then
			table.insert(d, "N")
		end
		if toRow - fromRow <= self.config.rows / 2 then
			table.insert(d, "S")
		end
	elseif toRow < fromRow then
		if fromRow - toRow >= self.config.rows / 2 then
			table.insert(d, "S")
		end
		if fromRow - toRow <= self.config.rows / 2 then
			table.insert(d, "N")
		end
	end

	if fromCol < toCol then
		if toCol - fromCol >= self.config.cols / 2 then
			table.insert(d, "W")
		end
		if toCol - fromCol <= self.config.cols / 2 then
			table.insert(d, "E")
		end
	elseif toCol < fromCol then
		if fromCol - toCol >= self.config.cols / 2 then
			table.insert(d, "E")
		end
		if fromCol - toCol <= self.config.cols / 2 then
			table.insert(d, "W")
		end
	end
	return d
end

return ants
