unit AppCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics;

type

  TLock = class
  private
    FLocked: Boolean;
  public
    constructor Create();

    function Lock(): Boolean;

    procedure Unlock();
  end;

  TObserver = class abstract
  public
    procedure Update(); virtual; abstract;
  end;

  TSubject = class abstract
  private
    FObservers: TList;
  protected
    procedure Notify();
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Attach(Observer: TObserver);
    procedure Detach(Observer: TObserver);
  end;

  TCustomValue = class(TSubject)
  private
    FValue: Extended;
  protected
    procedure SetValue(NewValue: Extended);
  public
    constructor Create(InitialValue: Extended);

    procedure Reset(); inline;

    property Value: Extended read FValue write SetValue;
  end;

  TValuePresenter = class abstract(TObserver)
  private
    FValue: TCustomValue;
    FView: TCustomEdit;

    FLockUpdate: TLock;
  protected
    function FloatToString(Value: Extended): string; virtual; abstract;
  public
    constructor Create(View: TCustomEdit; InitialValue: Extended);
    destructor Destroy(); override;

    procedure Update(); override;
    procedure UpdateModel();

    function GetModel(): TCustomValue;
  end;

  TFloatValuePresenter = class(TValuePresenter)
  protected
    function FloatToString(Value: Extended): string; override;
  public
    constructor Create(View: TCustomEdit; InitialValue: Extended = 0.0);
  end;

  TIntegerValuePresenter = class(TValuePresenter)
  protected
    function FloatToString(Value: Extended): string; override;
  public
    constructor Create(View: TCustomEdit; InitialValue: Extended = 0.0);
  end;

  { Overloaded operators }

  operator := (Right: TCustomValue): Extended; inline;

implementation

{ TLock }

constructor TLock.Create();
begin
  FLocked := False;
end;

function TLock.Lock(): Boolean;
begin
  if FLocked then
    Result := not FLocked
  else
  begin
    FLocked := True;
    Result := FLocked;
  end;
end;

procedure TLock.Unlock();
begin
  FLocked := False;
end;

{ TSubject }

constructor TSubject.Create();
begin
  FObservers := TList.Create();
end;

destructor TSubject.Destroy();
begin
  FObservers.Free();

  inherited Destroy();
end;

procedure TSubject.Attach(Observer: TObserver);
begin
  FObservers.Add(Observer);
end;

procedure TSubject.Detach(Observer: TObserver);
begin
  FObservers.Remove(Observer);
end;

procedure TSubject.Notify();
begin
  with FObservers.GetEnumerator() do
  begin
    while MoveNext() do
      TObserver(GetCurrent()).Update();

    Free();
  end;
end;

{ TCustomValue }

constructor TCustomValue.Create(InitialValue: Extended);
begin
  inherited Create();

  FValue := InitialValue;
end;

procedure TCustomValue.Reset();
begin
  Value := 0.0;
end;

procedure TCustomValue.SetValue(NewValue: Extended);
begin
  // We need to notify observers only if value changed really
  if not (NewValue = FValue) then
  begin
    FValue := NewValue;

    Notify();
  end;
end;

{ TValuePresenter }

constructor TValuePresenter.Create(View: TCustomEdit; InitialValue: Extended);
begin
  FValue := TCustomValue.Create(InitialValue);
  FView := View;

  FLockUpdate := TLock.Create();

  FValue.Attach(Self);
end;

destructor TValuePresenter.Destroy();
begin
  FValue.Detach(Self);
  FValue.Free();

  FLockUpdate.Free();

  inherited Destroy();
end;

procedure TValuePresenter.Update();
begin
  if FLockUpdate.Lock() then
  begin
    if not (Extended(FValue) = 0.0) then
      FView.Text := FloatToString(FValue)
    else
      FView.Clear();

    FLockUpdate.Unlock();
  end;
end;

procedure TValuePresenter.UpdateModel();
begin
  try
    if FLockUpdate.Lock() then
    begin
      if not (Length(FView.Text) = 0) then
        FValue.Value := StrToFloat(FView.Text)
      else
        FValue.Reset();

      // Restore default colors
      if not (FView.Color = clDefault) then
      begin
        FView.Color := clDefault;
        FView.Font.Color := clDefault;
      end;

      FLockUpdate.Unlock();
    end;
  except
    // We have invalid data in textbox
    FView.Color := clRed;
    FView.Font.Color := clWhite;

    FLockUpdate.Unlock();
  end;
end;

function TValuePresenter.GetModel(): TCustomValue;
begin
  Result := FValue;
end;

{ TFloatValuePresenter }

constructor TFloatValuePresenter.Create(View: TCustomEdit; InitialValue: Extended);
begin
  inherited Create(View, InitialValue);
end;

function TFloatValuePresenter.FloatToString(Value: Extended): string;
begin
  Result := FloatToStrF(Value, ffFixed, 4, 3);
end;

{ TIntegerValuePresenter }

constructor TIntegerValuePresenter.Create(View: TCustomEdit; InitialValue: Extended);
begin
  inherited Create(View, InitialValue);
end;

function TIntegerValuePresenter.FloatToString(Value: Extended): string;
begin
  Result := FloatToStrF(Value, ffFixed, 6, 0);
end;

{ Overloaded operators }

operator := (Right: TCustomValue): Extended;
begin
  Result := Right.Value;
end;

end.

