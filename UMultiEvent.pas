{ @see https://www.petitmonte.com/bbs/answers?question_id=2552 }
unit UMultiEvent;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Contnrs;

type
  TMultiEventUpdateEvent = procedure(Sender, Arg: TObject) of object;
  TMultiEventReceive = class;

  TMultiEventSend = class(TComponent)
  private
    FObservers: TComponentList;
    function GetObservers(Index: integer): TMultiEventReceive;
  protected
    property Observers[Index: integer]: TMultiEventReceive read GetObservers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(Value: TMultiEventReceive);
    procedure Delete(Value: TMultiEventReceive);

    procedure Notify(Arg: TObject);
  end;

  TMultiEventReceive = class(TComponent)
  private
    { Private 宣言 }
    FObservable: TMultiEventSend;
    FOnUpdate: TMultiEventUpdateEvent;
    procedure SetObservable(const Value: TMultiEventSend);
  protected
    { Protected 宣言 }
    procedure DoUpdate(Arg: TObject);
  public
    { Public 宣言 }
    property Observable: TMultiEventSend read FObservable write SetObservable;
  published
    { Published 宣言 }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property OnUpdate: TMultiEventUpdateEvent read FOnUpdate write FOnUpdate;
  end;

implementation

{ TMultiEventSend }

procedure TMultiEventSend.Add(Value: TMultiEventReceive);
begin
  if FObservers.IndexOf(Value) < 0 then
    FObservers.Add(Value);
end;


constructor TMultiEventSend.Create(AOwner: TComponent);
begin
  inherited;
  FObservers := TComponentList.Create(False);
end;


destructor TMultiEventSend.Destroy;
begin
  FObservers.Free;
  inherited;
end;


function TMultiEventSend.GetObservers(Index: integer): TMultiEventReceive;
begin
  Result := FObservers[Index] as TMultiEventReceive;
end;


procedure TMultiEventSend.Notify(Arg: TObject);
var
  i: integer;
begin
  for i := 0 to FObservers.Count - 1 do
  begin
    (FObservers[i] as TMultiEventReceive).DoUpdate(Arg);
  end;
end;


procedure TMultiEventSend.Delete(Value: TMultiEventReceive);
begin
  FObservers.Extract(Value);
end;

{ TMultiEventReceive }

procedure TMultiEventReceive.DoUpdate(Arg: TObject);
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self, Arg);
end;


procedure TMultiEventReceive.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FObservable) then
    SetObservable(nil);
end;


procedure TMultiEventReceive.SetObservable(const Value: TMultiEventSend);
begin
  if Assigned(FObservable) then
  begin
    FObservable.Delete(Self);
  end;
  FObservable := Value;
  if Assigned(FObservable) then
  begin
    FObservable.Add(Self);
    FObservable.FreeNotification(Self);
  end;
end;

end.
