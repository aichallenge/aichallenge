%Main code file

%execute code in state_functions.m and add all its functions to the program
source('state_functions.m');

%Debug information output to debug.txt
%set DEBUG to 0 when submitting to the official servers
%if you want to debug in a function remember to include DEBUG
%as a global variable there
global DEBUG = 1;
if DEBUG
    DEBUG = fopen('debug.txt','w');
    fprintf('Debug info\n');
end

%end turn of our bot, reset the state variables and print go to the server
function [state] = endTurn(state)
    state=reset(state);
    state.turn = state.turn + 1;
    printf('go\n');
    fflush(stdout);
endfunction

%function that selects moves and prints current state of the game
%if debug is active
function [state] = makeMoves(state)
    global DEBUG;
    global TDIRECTIONS;
    if DEBUG
        fprintf(DEBUG,'turn: %d\n',state.turn);
        stateOutput(state,DEBUG);
        fflush(DEBUG);
    end
    
    %picks out moves for each ant (go NESW in this order of preference if there's no water)
    for ant=1:state.nmyAnts
        for d=1:TDIRECTIONS
            loc = getLocation(state.myAnts(ant,:), d, state);
            if ~state.grid.isWater(loc(1)+1,loc(2)+1)
                state = makeMove(state.myAnts(ant,:), d, state);
                break;
            end
        end
    end

    if DEBUG
        t1 = toc;
        fprintf(DEBUG,'time taken: %.2f ms\n',t1*1000);
        fflush(DEBUG);
    end
endfunction

%MAIN CODE
%receive details about the game
state=stateInput(state);
%setup the state variables
state=setup(state);
%end the turn
state=endTurn(state);

%start playing the game
while 1
    state=stateInput(state);
    if state.gameover
        fclose(DEBUG);
        break;
    end
    state=updateVisionInformation(state);
    state=makeMoves(state);
    state=endTurn(state);
end



