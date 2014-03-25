unit AppCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls;

type
  TObserver = class abstract(TObject)
  public
    constructor Create();

    procedure Update(); virtual; abstract;
  end;

  TSubject = class abstract(TObject)
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

  TScalar = class(TSubject)
  private
    FValue: Extended;
  protected
    procedure SetValue(NewValue: Extended);
  public
    constructor Create();

    procedure Reset();

    property Value: Extended read FValue write SetValue;
  end;

  TScalarView = class abstract(TObserver)
  private
    FScalar: TScalar;
    FControl: TEdit;
  protected
    function FloatToString(Value: Extended): string; virtual; abstract;
  public
    constructor Create(Scalar: TScalar; Control: TEdit);
    destructor Destroy(); override;

    procedure Update(); override;
  end;

  TScalarViewFactory = class abstract(TObject)
  public
    constructor Create();

    function CreateView(Scalar: TScalar; Control: TEdit): TScalarView; virtual; abstract;
  end;

  TScalarController = class(TObject)
  private
    FScalar: TScalar;
    FScalarView: TScalarView;
  public
    constructor Create(); overload;
    constructor Create(Control: TEdit; Factory: TScalarViewFactory); overload;
    destructor Destroy(); override;

    procedure Reset();

    procedure SetValue(Value: string); overload;
    procedure SetValue(Value: Extended); overload;

    property Model: TScalar read FScalar;
  end;

  TScalarIntegerView = class(TScalarView)
  protected
    function FloatToString(Value: Extended): string; override;
  public
    constructor Create(Scalar: TScalar; Control: TEdit);
  end;

  TScalarFloatView = class(TScalarView)
  protected
    function FloatToString(Value: Extended): string; override;
  public
    constructor Create(Scalar: TScalar; Control: TEdit);
  end;

  TScalarIntegerViewFactory = class(TScalarViewFactory)
  public
    constructor Create();

    function CreateView(Scalar: TScalar; Control: TEdit): TScalarView; override;
  end;

  TScalarFloatViewFactory = class(TScalarViewFactory)
  public
    constructor Create();

    function CreateView(Scalar: TScalar; Control: TEdit): TScalarView; override;
  end;

  operator > (const Left: TScalar; const Right: TScalar): Boolean;
  operator < (const Left: TScalar; const Right: TScalar): Boolean;
  operator >= (const Left: TScalar; const Right: TScalar): Boolean;
  operator <= (const Left: TScalar; const Right: TScalar): Boolean;

implementation

{ TObserver }

constructor TObserver.Create();
begin
  inherited Create();
end;

{ TSubject }

constructor TSubject.Create();
begin
  inherited Create();

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
  fObservers.Remove(Observer);
end;

procedure TSubject.Notify();
var
  Enumerator: TListEnumerator;
begin
  Enumerator := FObservers.GetEnumerator();

  while Enumerator.MoveNext() do
    TObserver(Enumerator.GetCurrent()).Update();

  Enumerator.Free();
end;

{ TScalar }

constructor TScalar.Create();
begin
  inherited Create();

  FValue := 0.0;
end;

procedure TScalar.Reset();
begin
  SetValue(0.0);
end;

procedure TScalar.SetValue(NewValue: Extended);
begin
  FValue := NewValue;

  Notify();
end;

{ TScalarView }

constructor TScalarView.Create(Scalar: TScalar; Control: TEdit);
begin
  inherited Create();

  FScalar := Scalar;
  FControl := Control;

  FScalar.Attach(Self);
end;

destructor TScalarView.Destroy();
begin
  FScalar.Detach(Self);

  inherited Destroy();
end;

procedure TScalarView.Update();
begin
  if not ((Length(FControl.Text) = 0) and (FScalar.Value = 0.0)) then
    if FScalar.Value <> 0.0 then
      FControl.Text := FloatToString(FScalar.Value)
    else
      FControl.Clear()
end;

{ TScalarIntegerView }

constructor TScalarIntegerView.Create(Scalar: TScalar; Control: TEdit);
begin
  inherited Create(Scalar, Control);
end;

function TScalarIntegerView.FloatToString(Value: Extended): string;
begin
  Result := FloatToStrF(Value, ffFixed, 6, 0);
end;

{ TScalarFloatView }

constructor TScalarFloatView.Create(Scalar: TScalar; Control: TEdit);
begin
  inherited Create(Scalar, Control);
end;

function TScalarFloatView.FloatToString(Value: Extended): string;
begin
  Result := FloatToStrF(Value, ffFixed, 1, 3);
end;

{ TScalarViewFactory }

constructor TScalarViewFactory.Create();
begin
  inherited Create();
end;

{ TScalarIntegerViewFactory }

constructor TScalarIntegerViewFactory.Create();
begin
  inherited Create();
end;

function TScalarIntegerViewFactory.CreateView(Scalar: TScalar; Control: TEdit): TScalarView;
begin
  Result := TScalarIntegerView.Create(Scalar, Control);
end;

{ TScalarFloatViewFactory }

constructor TScalarFloatViewFactory.Create();
begin
  inherited Create();
end;

function TScalarFloatViewFactory.CreateView(Scalar: TScalar; Control: TEdit): TScalarView;
begin
  Result := TScalarFloatView.Create(Scalar, Control);
end;

{ TScalarController }

constructor TScalarController.Create();
begin
  inherited Create();

  FScalar := TScalar.Create();
  FScalarView := nil;
end;

constructor TScalarController.Create(Control: TEdit; Factory: TScalarViewFactory);
begin
  inherited Create();

  FScalar := TScalar.Create();
  FScalarView := Factory.CreateView(FScalar, Control);
end;

destructor TScalarController.Destroy();
begin
  FScalarView.Free();
  FScalar.Free();

  inherited Destroy();
end;

procedure TScalarController.Reset();
begin
  FScalar.Reset();
end;

procedure TScalarController.SetValue(Value: string);
begin
  if Length(Value) <> 0 then
    FScalar.Value := StrToFloat(Value)
  else
    Reset();
end;

procedure TScalarController.SetValue(Value: Extended);
begin
  FScalar.Value := Value;
end;

{ Overloaded operators }

operator > (const Left: TScalar; const Right: TScalar): Boolean;
begin
  Result := (Left.Value > Right.Value);
end;

operator < (const Left: TScalar; const Right: TScalar): Boolean;
begin
  Result := (Left.Value < Right.Value);
end;

operator >= (const Left: TScalar; const Right: TScalar): Boolean;
begin
  Result := (Left.Value >= Right.Value);
end;

operator <= (const Left: TScalar; const Right: TScalar): Boolean;
begin
  Result := (Left.Value <= Right.Value);
end;

end.

