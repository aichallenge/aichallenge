program MyBot;

uses
  Ants, StarterBot;

var
  AAnts: TAnts;
  ABot: TAbstractBot;

begin
  AAnts := TAnts.Create;
  try
    ABot := TStarterBot.Create;
    try
      AAnts.Run(ABot);
    finally
      ABot.Free;
    end;
  finally
    AAnts.Free;
  end;
end.

