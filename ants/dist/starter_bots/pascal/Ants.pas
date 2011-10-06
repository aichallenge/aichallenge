unit Ants;

interface

uses
  Classes;

type

  TSquareValue = (sqWater = -4, sqFood, sqLand, sqDead,
    sqMe = 0, sqB, sqC, sqD, sqE, sqF, sqG, sqH, sqI, sqJ, sqK, sqL, sqM,
    sqN, sqO, sqP, sqQ, sqR, sqS, sqT, sqU, sqV, sqW, sqX, sqY, sqZ);

  TOwner = sqMe .. sqZ;

  TLocation = record
    Row: Integer;
    Col: Integer;
  end;
  PLocation = ^TLocation;
  TLocationArray = array of TLocation;

  TOwnedLocation = record
    Owner: TOwner;
    Loc: TLocation;
  end;
  POwnedLocation = ^TOwnedLocation;
  TOwnedLocationArray = array of TOwnedLocation;

  TDirection = record
    Rows: Integer;
    Cols: Integer;
    Key: Char;
  end;
  TDirectionArray = array of TDirection;

  TAbstractBot = class;

  { TAntsData }

  TAntsData = class
  protected
    FStdOut: Text;
    FRows: Integer;
    FCols: Integer;
    FTurns: Integer;
    FLoadTime: Integer;
    FTurnTime: Integer;
    FViewRadius2: Integer;
    FAttackRadius2: Integer;
    FSpawnRadius2: Integer;
    FPlayerSeed: Int64;
    FEndTime: TDateTime;
    FMap: array of array of TSquareValue;
    FFood: TLocationArray;
    FMyHills: TOwnedLocationArray;
    FEnemyHills: TOwnedLocationArray;
    FMyAnts: TOwnedLocationArray;
    FEnemyAnts: TOwnedLocationArray;
    FDeadAnts: TOwnedLocationArray;
  public
    // map height
    property Rows: Integer read FRows;
    // map width
    property Cols: Integer read FCols;
    // turn limit
    property Turns: Integer read FTurns;
    // time in ms, until bot must be ready for the first turn
    property LoadTime: Integer read FLoadTime;
    // time in ms per turn
    property TurnTime: Integer read FTurnTime;
    // squared view radius
    property ViewRadius2: Integer read FViewRadius2;
    // squared attack radius
    property AttackRadius2: Integer read FAttackRadius2;
    // squared spawn radius
    property SpawnRadius2: Integer read FSpawnRadius2;
    // a random number from the game engine command line (for debugging)
    property PlayerSeed: Int64 read FPlayerSeed;
    // locations with visible food items
    property Food: TLocationArray read FFood;
    // own hills
    property MyHills: TOwnedLocationArray read FMyHills;
    // visible, alive enemy hills
    property EnemyHills: TOwnedLocationArray read FEnemyHills;
    // own ants
    property MyAnts: TOwnedLocationArray read FMyAnts;
    // visible enemy ants
    property EnemyAnts: TOwnedLocationArray read FEnemyAnts;
    // ants that died in the last turn on currently visible squares
    property DeadAnts: TOwnedLocationArray read FDeadAnts;
    // squared euclidean distance (x²+y²), takes wrapping into account
    function DistEuclid2(const Loc1: TLocation; const Loc2: TLocation): Integer;
    // euclidean distance (Sqrt(x²+y²)), takes wrapping into account
    function DistEuclid(const Loc1: TLocation; const Loc2: TLocation): Extended;
    // manhatten distance (x+y), takes wrapping into account
    function Dist(const Loc1: TLocation; const Loc2: TLocation): Integer;
    // returns possible directions that go from one location to another
    function Direction(const Loc1: TLocation; const Loc2: TLocation): TDirectionArray;
    // adds a directional offset to a location and returns the result
    function Destination(const Loc: TLocation; const ADirection: TDirection): TLocation;
    // checks if a square is not filled with water
    function Passable(const Loc: TLocation): Boolean;
    // time remaining for the turn in milliseconds
    function TimeRemaining(): Integer;
    // allows the bot to issue an order from a location into any direction
    procedure IssueOrder(const Loc: TLocation; const ADirection: TDirection);
    constructor Create;
    destructor Destroy; override;
  end;

  { TAnts }

  TAnts = class(TAntsData)
  private
    function AppendTo(var AArray: TLocationArray): PLocation; overload;
    function AppendTo(var AArray: TOwnedLocationArray): POwnedLocation; overload;
    procedure Setup(Lines: TStringList);
    function Update(Lines: TStringList): Boolean;
    procedure FinishTurn();
  public
    procedure Run(Bot: TAbstractBot);
  end;

  { TAbstractBot }

  TAbstractBot = class
  public
    procedure DoTurn(Game: TAntsData); virtual; abstract;
  end;

const
  NORTH : TDirection = ( Rows: -1; Cols:  0; Key: 'n' );
  EAST  : TDirection = ( Rows:  0; Cols: +1; Key: 'e' );
  SOUTH : TDirection = ( Rows: +1; Cols:  0; Key: 's' );
  WEST  : TDirection = ( Rows:  0; Cols: -1; Key: 'w' );

implementation

uses
  SysUtils, Math;

const
  MILLISECS_PER_DAY = 24 * 60 * 60 * 1000;
  DAYS_PER_MILLISEC = 1 / MILLISECS_PER_DAY;

{ TAntsData }

constructor TAntsData.Create;
begin
  Assign(FStdOut, '');
  Rewrite(FStdOut);
end;

destructor TAntsData.Destroy;
begin
  inherited Destroy;
end;

function TAntsData.DistEuclid2(const Loc1: TLocation; const Loc2: TLocation): Integer;
var
  DR, DC: Integer;
begin
  DR := Abs(Loc1.Row - Loc2.Row);
  DR := Min(DR, FRows - DR);
  DC := Abs(Loc1.Col - Loc2.Col);
  DC := Min(DC, FCols - DC);
  Result := DR * DR + DC * DC;
end;

function TAntsData.DistEuclid(const Loc1: TLocation; const Loc2: TLocation): Extended;
begin
  Result := Sqrt(DistEuclid2(Loc1, Loc2));
end;

function TAntsData.Dist(const Loc1: TLocation; const Loc2: TLocation): Integer;
var
  DR, DC: Integer;
begin
  DR := Abs(Loc1.Row - Loc2.Row);
  DR := Min(DR, FRows - DR);
  DC := Abs(Loc1.Col - Loc2.Col);
  DC := Min(DC, FCols - DC);
  Result := DR + DC;
end;

function TAntsData.Direction(const Loc1: TLocation; const Loc2: TLocation): TDirectionArray;
var
  I, DR, DC: Integer;
begin
  SetLength(Result, 4);
  I := 0;
  DR := Abs(Loc1.Row - Loc2.Row);
  DC := Abs(Loc1.Col - Loc2.Col);
  if ((Loc1.Row > Loc2.Row) and (DR <= FRows - DR) or (Loc1.Row < Loc2.Row) and (DR >= FRows - DR)) then
  begin
    Result[I] := NORTH;
    Inc(I);
  end;
  if ((Loc1.Col < Loc2.Col) and (DC <= FCols - DC) or (Loc1.Col > Loc2.Col) and (DC >= FCols - DC)) then
  begin
    Result[I] := EAST;
    Inc(I);
  end;
  if ((Loc1.Row < Loc2.Row) and (DR <= FRows - DR) or (Loc1.Row > Loc2.Row) and (DR >= FRows - DR)) then
  begin
    Result[I] := SOUTH;
    Inc(I);
  end;
  if ((Loc1.Col > Loc2.Col) and (DC <= FCols - DC) or (Loc1.Col < Loc2.Col) and (DC >= FCols - DC)) then
  begin
    Result[I] := WEST;
    Inc(I);
  end;
  SetLength(Result, I);
end;

function TAntsData.Destination(const Loc: TLocation; const ADirection: TDirection): TLocation;
begin
  Result.Row := (Loc.Row + ADirection.Rows + FRows) mod FRows;
  Result.Col := (Loc.Col + ADirection.Cols + FCols) mod FCols;
end;

function TAntsData.Passable(const Loc: TLocation): Boolean;
begin
  Result := FMap[Loc.Row, Loc.Col] <> sqWater;
end;

function TAntsData.TimeRemaining(): Integer;
var
  LNow: TDateTime;
begin
  LNow := Now;
  if (LNow > FEndTime) then
    Result := 0
  else
    Result := Trunc((FEndTime - LNow) * MILLISECS_PER_DAY);
end;

procedure TAntsData.IssueOrder(const Loc: TLocation; const ADirection: TDirection);
begin
  WriteLn(FStdOut, 'o ', Loc.Row, ' ', Loc.Col, ' ', ADirection.Key);
  Flush(FStdOut);
end;

{ TAnts }

procedure TAnts.Setup(Lines: TStringList);
var
  I, Row, Col: Integer;
  Line: string;
  Key: string;
  Value: Int64;
  Tokens: TStringList;
begin
  Tokens := TStringList.Create;
  try
    Tokens.Delimiter := ' ';
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if Line <> '' then
      begin
        Tokens.DelimitedText := Line;
	Key := LowerCase(Tokens[0]);
        Value := StrToInt64(Tokens[1]);
             if Key = 'rows'          then FRows          := Value
        else if Key = 'cols'          then FCols          := Value
        else if Key = 'turns'         then FTurns         := Value
        else if Key = 'loadtime'      then FLoadTime      := Value
        else if Key = 'turntime'      then FTurnTime      := Value
        else if Key = 'viewradius2'   then FViewRadius2   := Value
        else if Key = 'attackradius2' then FAttackRadius2 := Value
        else if Key = 'spawnradius2'  then FSpawnRadius2  := Value
        else if Key = 'player_seed'   then FPlayerSeed    := Value
      end;
    end;
  finally
    Tokens.Free;
  end;
  SetLength(FMap, FRows, FCols);
  for Row := 0 to FRows - 1 do
  for Col := 0 to FCols - 1 do
  begin
    FMap[Row, Col] := sqLand;
  end;
end;

function TAnts.Update(Lines: TStringList): Boolean;
var
  I, Row, Col: Integer;
  Line: string;
  Key: string;
  Owner: TSquareValue;
  Tokens: TStringList;
  LocationPtr: PLocation;
  OwnedLocationPtr: POwnedLocation;
  FirstLine: Boolean;
begin
  // clear ant and food data
  for Row := 0 to Length(FMap) - 1 do
  for Col := 0 to Length(FMap[Row]) - 1 do
  begin
    if FMap[Row, Col] <> sqWater then FMap[Row, Col] := sqLand;
  end;
  SetLength(FFood, 0);
  SetLength(FMyHills, 0);
  SetLength(FEnemyHills, 0);
  SetLength(FMyAnts, 0);
  SetLength(FEnemyAnts, 0);
  SetLength(FDeadAnts, 0);
  // update map and create new ant and food lists
  Tokens := TStringList.Create;
  try
    Tokens.Delimiter := ' ';
    FirstLine := True;
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if Line <> '' then
      begin
        Tokens.DelimitedText := Line;
        Key := LowerCase(Tokens[0]);
        // check and ignore end game statistics
        if FirstLine and (Key = 'end') then
        begin
          Result := False;
          Exit;
        end;
        // all the interesting bits have at least 3 fields
        if Tokens.Count >= 3 then
        begin
          LocationPtr := nil;
          OwnedLocationPtr := nil;
          Row := StrToInt(Tokens[1]);
          Col := StrToInt(Tokens[2]);
          if Tokens.Count = 4 then Owner := TSquareValue(StrToInt(Tokens[3]));
          if Key = 'h' then
          begin
            if Owner = sqMe then OwnedLocationPtr := AppendTo(FMyHills)
                            else OwnedLocationPtr := AppendTo(FEnemyHills);
          end
          else if Key = 'a' then
          begin
            if Owner = sqMe then OwnedLocationPtr := AppendTo(FMyAnts)
                            else OwnedLocationPtr := AppendTo(FEnemyAnts);
          end
          else if Key = 'd' then
          begin
            FMap[Row, Col] := sqDead;
            OwnedLocationPtr := AppendTo(FDeadAnts);
          end
          else if Key = 'f' then
          begin
            FMap[Row, Col] := sqFood;
            LocationPtr := AppendTo(FFood);
          end
          else if Key = 'w' then
          begin
            FMap[Row, Col] := sqWater;
          end;
          // set data
          if (OwnedLocationPtr <> nil) then
          begin
            OwnedLocationPtr^.Owner := Owner;
            LocationPtr := @OwnedLocationPtr^.Loc;
          end;
          if (LocationPtr <> nil) then
          begin
            LocationPtr^.Row := Row;
            LocationPtr^.Col := Col;
          end;
        end;
      end;
      FirstLine := False;
    end;
  finally
    Tokens.Free;
  end;
  Result := True;
end;

function TAnts.AppendTo(var AArray: TLocationArray): PLocation;
begin
  SetLength(AArray, Length(AArray) + 1);
  Result := @AArray[High(AArray)];
end;

function TAnts.AppendTo(var AArray: TOwnedLocationArray): POwnedLocation;
begin
  SetLength(AArray, Length(AArray) + 1);
  Result := @AArray[High(AArray)];
end;

procedure TAnts.FinishTurn();
begin
  WriteLn(FStdOut, 'go');
  Flush(FStdOut);
end;

procedure TAnts.Run(Bot: TAbstractBot);
var
  MapData: TStringList;
  CurrentLine: string;
begin
  MapData := TStringList.Create;
  try
    while not EOF do
    begin
      ReadLn(CurrentLine);
      CurrentLine := LowerCase(CurrentLine);
      if CurrentLine = 'ready' then
      begin
        Setup(MapData);
        FinishTurn();
        MapData.Clear;
      end
      else if CurrentLine = 'go' then
      begin
        FEndTime := Now + FTurnTime * DAYS_PER_MILLISEC;
        if Update(MapData) then Bot.DoTurn(Self);
        FinishTurn();
        MapData.Clear;
      end
      else
      begin
        MapData.Append(CurrentLine);
      end;
    end;
  finally
    MapData.Free;
  end;
end;

end.

