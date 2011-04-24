$:.unshift File.dirname($0)
require 'ants.rb'

ai=AI.new

ai.setup do |ai|
	# your setup code here, if any
end

ai.run do |ai|
	# your turn code here
	
	ai.my_ants.each do |ant|
		ant.order [:N, :E, :S, :W][rand 4]
	end
	
	if ai.turn_number == :game_over
		$stderr.puts ai.score.zip(0...ai.players).map{|score, player| "Player #{player}: #{score}"}
	end
end