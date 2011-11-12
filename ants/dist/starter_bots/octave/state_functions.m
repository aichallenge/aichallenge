%state related functions

%define constants
global TDIRECTIONS = 4;
global CDIRECTIONS = 'NESW';
global DIRECTIONS = [[-1 0] ; [0 1] ; [1 0] ; [0 -1]];

%initialize state structure
state = struct('gameover',0,'turn',0);

%setup the grid inside state structure
function [state] = setup(state)
    z = zeros(state.rows,state.cols);
    o = ones(state.rows,state.cols);
    %grid that has info about all squares in the map, variable names are self-explanatory
    %ant has the number of the player the ant belongs to (-1 if there's no ant, 0 if the current player)
    %hillPlayer uses the same notation
    state.grid = struct('isVisible',z,'isWater',z,'isHill',z,'isFood',z,'ant',-o,'hillPlayer',-o);
endfunction

%reset the state at the end of the turn
function [state] = reset(state)
    %resetting all important locations to be matrices with sufficient size
    %such that they don't need to grow (and thus slow down performance) when the input arrives
    %these are rescaled after the input is received to the 'real' size
    
    %matrix nmyAnts x 2 of the locations of the player's ants
    state.myAnts = zeros(1000,2);
    state.nmyAnts = 0;
    %matrix nenemyAnts x 2 of the location the enemies' ants
    state.enemyAnts = zeros(1000,2);
    state.nenemyAnts = 0;
    %matrix nmyHills x 2 of the location of the player's hills
    state.myHills = zeros(100,2);
    state.nmyHills = 0;
    %matrix nenemyHills x 2 of the location of the enemies' hills
    state.enemyHills = zeros(100,2);
    state.nenemyHills = 0;
    %matrix nfood x 2 of the location of food
    state.food = zeros(2000,2);
    state.nfood = 0;

    %reset info in all grid squares that are land
    ch = (state.grid.isWater == 0);
    num = sum(sum(ch));
    o = ones(1,num);
    z = zeros(1,num);
    state.grid.isVisible(ch) = z;
    state.grid.isHill(ch) = z;
    state.grid.isFood(ch) = z;
    state.grid.ant(ch) = -o;
    state.grid.hillPlayer(ch) = -o;
endfunction
    
%output move information to the engine
function [state] = makeMove(loc, d, state)
    %location is a 2-element vector [row col]
    %d is a direction 1,2,3,4 corresponding to NESW
    global CDIRECTIONS;
    printf('o %d %d %c\n',loc(1),loc(2),CDIRECTIONS(d));
    nloc = getLocation(loc,d,state);
    state.grid.ant(nloc(1)+1,nloc(2)+1) = 0;
    state.grid.ant(loc(1)+1,loc(2)+1) = -1;
endfunction

%euclidean distance between two locations taking into account wrap around
function [dist] = distance(loc1, loc2, state)
    %loc1 and loc2 are 2-element vectors [row col]
    d1 = abs(loc1(1) - loc2(1));
    d2 = abs(loc1(2) - loc2(2));
    dr = min(d1,state.rows - d1);
    dc = min(d2,state.cols - d2);
    dist = sqrt(dr^2 + dc^2);
endfunction

%return the location obtained when moving in direction d taking into account the wrap around
function [locd] = getLocation(loc, d, state)
    %loc and d same as in makeMove
    global DIRECTIONS;
    locd = [mod(loc(1) + DIRECTIONS(d,1) + state.rows,state.rows) mod(loc(2) + DIRECTIONS(d,2) + state.cols,state.cols)];
endfunction

%cute BFS to determine visible cells in C++ - painfully slow in matlab (more than fast enough in C++)
%function [state] = updateVisionInformation(state)
%    global TDIRECTIONS;
%    queue = zeros(state.rows*state.cols,2);
%    visited = zeros(state.rows,state.cols);
%    top = 1;
%    bottom = 1;
%    
%    for a=1:state.nmyAnts
%        sloc = state.myAnts(a,:);
%        queue(bottom,:) = sloc;
%        bottom = bottom + 1;
%        state.grid.isVisible(sloc(1)+1,sloc(2)+1) = 1;
%        visited(sloc(1)+1,sloc(2)+1) = 1;
%        while top < bottom
%            cloc = queue(top,:);
%            top = top + 1;
%            t1=toc;
%            for d=1:TDIRECTIONS
%                nloc = getLocation(cloc, d, state);
%                if ~visited(nloc(1)+1,nloc(2)+1) && ~state.grid.isVisible(nloc(1)+1,nloc(2)+1) && distance(nloc, sloc, state) <= state.viewradius
%                    state.grid.isVisible(nloc(1)+1, nloc(2)+1) = 1;
%                    queue(bottom,:) = nloc;
%                    bottom = bottom + 1;
%                end
%                visited(nloc(1)+1,nloc(2)+1) = 1;
%            end
%        end
%    end
%endfunction

%matlab tricks - define mask of visible cells and apply it at each ant's position
function [state] = updateVisionInformation(state)
    %make visible = 0, invisible = 1
    state.grid.isVisible = 1 - state.grid.isVisible;
    %define a centered mask with visible cells around ant = 0 and 1 otherwise
    vr = floor(state.viewradius);
    dst = (-vr:vr).^2;
    sz = length(dst);
    dst2 = repmat(dst,sz,1) + repmat(dst,sz,1)';
    mask = dst2 > vr ^ 2;
    %process each ant
    for ant=1:state.nmyAnts;
        %get the location of the ant and position of the mask in the map
        loc = state.myAnts(ant,:);
        row = loc(1)+1; col = loc(2)+1;
        %corners of the mask in the map
        t=top = row-vr; l=left = col-vr; r=right = col+vr; b=bottom = row+vr;
        %corners of the mask
        tm = 1; lm = 1; rm = sz; bm = sz;
        %'cut corners' if the mask goes out of the map
        if top<1
            tm = 2 - top;
            t = 1;
        end
        
        if left < 1
            lm = 2 - left;
            l = 1;
        end
        
        if right > state.cols
            rm = sz - (right - state.cols);
            r = state.cols;
        end
        
        if bottom > state.rows
            bm = sz - (bottom - state.rows);
            b = state.rows;
        end
        
        %multiply element-wise such that all visible cells in the map around the ant get the value 0
        state.grid.isVisible(t:b,l:r) = state.grid.isVisible(t:b,l:r) .* mask(tm:bm,lm:rm);
        
        %take into account the 8 different cases where the mask is out of the map
        if top<1
            state.grid.isVisible(state.rows+top:state.rows,l:r) = state.grid.isVisible(state.rows+top:state.rows,l:r) .* mask(1:tm-1,lm:rm);
        end
        
        if left<1
            state.grid.isVisible(t:b,state.cols+left:state.cols) = state.grid.isVisible(t:b,state.cols+left:state.cols) .* mask(tm:bm,1:lm-1);
        end
        
        if right>state.cols
            state.grid.isVisible(t:b,1:right-state.cols) = state.grid.isVisible(t:b,1:right-state.cols) .* mask(tm:bm,rm+1:end);
        end
        
        if bottom>state.rows
            state.grid.isVisible(1:bottom-state.rows,l:r) = state.grid.isVisible(1:bottom-state.rows,l:r) .* mask(bm+1:end,lm:rm);
        end
        
        if top<1 && left<1
            state.grid.isVisible(state.rows+top:state.rows,state.cols+left:state.cols) = state.grid.isVisible(state.rows+top:state.rows,state.cols+left:state.cols) .* mask(1:tm-1,1:lm-1);
        end
        
        if top<1 && right>state.cols
            state.grid.isVisible(state.rows+top:state.rows,1:right-state.cols) = state.grid.isVisible(state.rows+top:state.rows,1:right-state.cols) .* mask(1:tm-1,rm+1:end);
        end
        
        if bottom>state.rows && left<1
            state.grid.isVisible(1:bottom-state.rows,state.cols+left:state.cols) = state.grid.isVisible(1:bottom-state.rows,state.cols+left:state.cols) .* mask(bm+1:end,1:lm-1);
        end
        
        if bottom>state.rows && right>state.cols
            state.grid.isVisible(1:bottom-state.rows,1:right-state.cols) = state.grid.isVisible(1:bottom-state.rows,1:right-state.cols) .* mask(bm+1:end,rm+1:end);
        end       
    end
    
    %return to the original visible matrix, 1-visible, 0-invisible
    state.grid.isVisible = 1 - state.grid.isVisible;
endfunction
            
            
        
    
    

%send output of the state to a debug file (basically this outputs the map seen by the ants)
function [] = stateOutput(state, f)
    out = '?'*ones(state.rows,state.cols);
    out(state.grid.isVisible == 1) = '.';
    out(state.grid.isWater == 1) = '%';
    out(state.grid.isFood == 1) = '*';
    out(state.grid.ant >= 0) = 'a' + state.grid.ant(state.grid.ant >= 0);
    out(state.grid.isHill == 1) = 'A' + state.grid.hillPlayer(state.grid.isHill == 1);    
    fdisp(f,char(out));
    fflush(f);
endfunction

%get input from the game into the state
function [state] = stateInput(state)
    while (inputType = scanf('%s',1))
        if strcmp(inputType,'end')
            state.gameover = 1;
            break;
        elseif strcmp(inputType,'turn')
                %start counting the time the bot uses on this turn (use variable=toc to measure it)
                tic;
                state.turn = scanf('%d',1);
                break;                
        else
            fgetl(stdin);
        end
    end
    if state.turn == 0 
        %read game parameters
        while (inputType = scanf('%s',1))
            if strcmp(inputType,'loadtime')
                state.loadtime = scanf('%d',1);
            elseif strcmp(inputType,'turntime')
                state.turntime = scanf('%d',1);
            elseif strcmp(inputType,'rows')
                state.rows = scanf('%d',1);
            elseif strcmp(inputType,'cols')
                state.cols = scanf('%d',1);
            elseif strcmp(inputType,'turns')
                state.turns = scanf('%d',1);
            elseif strcmp(inputType,'player_seed')
                state.seed = scanf('%d',1);
            elseif strcmp(inputType,'viewradius2')
                state.viewradius = sqrt(scanf('%d',1));
            elseif strcmp(inputType,'attackradius2')
                state.attackradius = sqrt(scanf('%d',1));
            elseif strcmp(inputType,'spawnradius2')
                state.spawnradius = sqrt(scanf('%d',1));
            elseif strcmp(inputType,'ready') %end of input
                tic;
                break;
            else
                fgetl(stdin);
            end
        end
    else 
        %read info about current turn
        while (inputType = scanf('%s',1))
            if strcmp(inputType,'w') %water square
                row = scanf('%d',1);
                col = scanf('%d',1);
                state.grid.isWater(row+1,col+1) = 1;
            elseif strcmp(inputType,'f') %food square
                row = scanf('%d',1);
                col = scanf('%d',1);
                state.grid.isFood(row+1,col+1) = 1;
                state.nfood = state.nfood + 1;
                state.food(state.nfood,:) = [row col];
            elseif strcmp(inputType,'a') %live ant square
                row = scanf('%d',1);
                col = scanf('%d',1);
                player = scanf('%d',1);
                state.grid.ant(row+1,col+1) = player;
                if player == 0
                    state.nmyAnts = state.nmyAnts + 1;
                    state.myAnts(state.nmyAnts,:) = [row col];
                else
                    state.nenemyAnts = state.nenemyAnts + 1;
                    state.enemyAnts(state.nenemyAnts,:) = [row col];
                end
            elseif strcmp(inputType,'d') %dead ant square
                row = scanf('%d',1);
                col = scanf('%d',1);
                player = scanf('%s',1);
                %ignore dead ants in this implementation of the package, 
                %write here the code to deal with them if you wish
            elseif strcmp(inputType,'h') %hill square
                row = scanf('%d',1);
                col = scanf('%d',1);
                player = scanf('%d',1);
                state.grid.isHill(row+1,col+1) = 1;
                state.grid.hillPlayer(row+1,col+1) = player;
                if player == 0
                    state.nmyHills = state.nmyHills + 1;
                    state.myHills(state.nmyHills,:) = [row col];
                else
                    state.nenemyHills = state.nenemyHills + 1;
                    state.enemyHills(state.nenemyHills,:) = [row col];
                end
            elseif strcmp(inputType,'players') %number of players when game is over
                state.noPlayers = scanf('%d',1);
            elseif strcmp(inputType,'scores') %scores of each player when game is over
                state.scores = scanf('%lf',state.noPlayers);
            elseif strcmp(inputType,'go') %end of input for this turn
                break;
            else
                fgetl(stdin);
            end
        end
        %resize arrays to real size
        state.food = state.food(1:state.nfood,:);
        state.myAnts = state.myAnts(1:state.nmyAnts,:);
        state.enemyAnts = state.enemyAnts(1:state.nenemyAnts,:);
        state.myHills = state.myHills(1:state.nmyHills,:);
        state.enemyHills = state.enemyHills(1:state.nenemyHills,:);
    end
endfunction                   

                
        
    
    
