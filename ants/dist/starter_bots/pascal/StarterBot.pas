unit StarterBot;

interface

uses
  Ants;

type

  { TStarterBot }

  TStarterBot = class(TAbstractBot)
  public
    procedure DoTurn(Game: TAntsData); override;
  end;

implementation

{ TStarterBot }

procedure TStarterBot.DoTurn(Game: TAntsData);
var
  I, D: Integer;
  AntLoc: TLocation;
const
  DIRECTIONS: array[0..3] of ^TDirection = (@NORTH, @EAST, @SOUTH, @WEST);
begin
  // for every of my ants
  for I := 0 to Length(Game.MyAnts) - 1 do
  begin
    AntLoc := Game.MyAnts[I].Loc;
    // try all directions until one leads to land
    for D := 0 to 3 do
    begin
      if Game.Passable(Game.Destination(AntLoc, DIRECTIONS[D]^)) then
      begin
        // go there and continue with the next ant
        Game.IssueOrder(antLoc, DIRECTIONS[D]^);
        Break;
      end;
    end;
  end;
end;

end.

