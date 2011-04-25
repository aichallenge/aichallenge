# Ants AI Challenge framework
# by Matma Rex (matma.rex@gmail.com)
# Released under CC-BY 3.0 license

class Ant
	attr_accessor :alive, :owner, :square, :ai
	def initialize alive, owner, square, ai
		@alive, @owner, @square, @ai = alive, owner, square, ai
	end
	
	def alive?; @alive; end
	def dead?; !@alive; end
	
	def mine?; owner==0; end
	def enemy?; owner!=0; end
	
	def row; @square.row; end
	def col; @square.col; end
	
	
	def order direction
		@ai.order self, direction
	end
end

class Square
	attr_accessor :seen, :water, :food, :ant
	attr_accessor :row, :col
	def initialize water, food, ant, row, col
		@water, @food, @ant, @row, @col = water, food, ant, row, col
	end
	
	def land?; !@water; end
	def water?; @water; end
	
	def has_ant?; @ant and @ant.alive?; end;
end

class UnexpectedInput < Exception; end

class AI
	attr_accessor :map, :turn_number
	attr_accessor :loadtime, :turntime, :rows, :cols, :turns, :viewradius2, :attackradius2, :spawnradius2
	attr_accessor :players, :score
	attr_accessor :my_ants, :enemy_ants


	def initialize stdin=$stdin, stdout=$stdout
		@stdin, @stdout = stdin, stdout

		@map=nil
		@turn_number=0
		
		@my_ants=[]
		@enemy_ants=[]
		
		@did_setup=false
	end
	
	def settings
		{
			:loadtime => @loadtime,
			:turntime => @turntime,
			:rows => @rows,
			:cols => @cols,
			:turns => @turns,
			:viewradius2 => @viewradius2,
			:attackradius2 => @attackradius2,
			:spawnradius2 => @spawnradius2,
			:viewradius => @viewradius,
			:attackradius => @attackradius,
			:spawnradius => @spawnradius
		}.freeze
	end
	
	def setup # :yields: self
		read_intro
		yield self
		@stdout.puts 'go'
		
		@map=Array.new(@rows){|row| Array.new(@cols){|col| Square.new false, false, nil, row, col } }
		@did_setup=true
	end
	
	def run &b # :yields: self
		setup &b if !@did_setup
		
		loop do
			over = read_turn
			yield self
			@stdout.puts 'go'
			
			break if over
		end
	end
	
	def order a, b, c=nil
		if !c # assume two-argument form: ant, direction
			ant, direction = a, b
			@stdout.puts "o #{ant.row} #{ant.col} #{direction.to_s.upcase}"
		else # assume three-argument form: row, col, direction
			col, row, direction = a, b, c
			@stdout.puts "o #{row} #{col} #{direction.to_s.upcase}"
		end
	end

	
	def read_intro
		rd=@stdin.gets.strip
		raise UnexpectedInput, rd unless rd=='turn 0'

		until((rd=@stdin.gets.strip)=='ready')
			_, name, value = *rd.match(/\A([a-z0-9]+) (\d+)\Z/)
			
			case name
			when 'loadtime'; @loadtime=value.to_i
			when 'turntime'; @turntime=value.to_i
			when 'rows'; @rows=value.to_i
			when 'cols'; @cols=value.to_i
			when 'turns'; @turns=value.to_i
			when 'viewradius2'; @viewradius2=value.to_i
			when 'attackradius2'; @attackradius2=value.to_i
			when 'spawnradius2'; @spawnradius2=value.to_i
			else
				raise UnexpectedInput, rd
			end
		end
		
		@viewradius=Math.sqrt @viewradius2
		@attackradius=Math.sqrt @attackradius2
		@spawnradius=Math.sqrt @spawnradius2
	end
	
	def read_turn
		ret=false
		
		rd=@stdin.gets.strip
		
		if rd=='end'
			@turn_number=:game_over
			
			rd=@stdin.gets.strip
			_, players = *rd.match(/\Aplayers (\d+)\Z/)
			@players = players.to_i
			
			rd=@stdin.gets.strip
			_, score = *rd.match(/\Ascore (\d+(?: \d+)+)\Z/)
			@score = score.split(' ').map{|s| s.to_i}
			
			ret=true
		else
			_, num = *rd.match(/\Aturn (\d+)\Z/)
			@turn_number=num.to_i
		end
	
		# reset the map data
		@map.each do |row|
			row.each do |square|
				square.food=false
				square.ant=nil
			end
		end
		
		@my_ants=[]
		@enemy_ants=[]
		
		until((rd=@stdin.gets.strip)=='go')
			_, type, row, col, owner = *rd.match(/(w|f|a|d) (\d+) (\d+)(?: (\d+)|)/)
			row, col = row.to_i, col.to_i
			owner = owner.to_i if owner
			
			case type
			when 'w'
				@map[row][col].water=true
			when 'f'
				@map[row][col].food=true
			when 'a'
				a=Ant.new true, owner, @map[row][col], self
				@map[row][col].ant = a
				
				if owner==0
					my_ants.push a
				else
					enemy_ants.push a
				end
			when 'd'
				d=Ant.new false, owner, @map[row][col], self
				@map[row][col].ant = d
			else
				p type
				raise UnexpectedInput, rd
			end
		end
		
		return ret
	end
end











