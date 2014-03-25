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

    function IsZero(): Boolean;

    property Value: Extended read FValue write SetValue;
  end;

  TScalarView = class abstract(TObserver)
  private
    FModel: TScalar;
    FEdit: TEdit;
  protected
    function FloatToString(Value: Extended): string; virtual; abstract;
  public
    constructor Create(Model: TScalar; Edit: TEdit);
    destructor Destroy(); override;

    procedure Update(); override;
  end;

  TScalarIntegerView = class(TScalarView)
  protected
    function FloatToString(Value: Extended): string; override;
  public
    constructor Create(Model: TScalar; Edit: TEdit);
  end;

  TScalarFloatView = class(TScalarView)
  protected
    function FloatToString(Value: Extended): string; override;
  public
    constructor Create(Model: TScalar; Edit: TEdit);
  end;

  TScalarViewAbstractFactory = class abstract(TObject)
  public
    constructor Create();

    function CreateView(Model: TScalar; Edit: TEdit): TScalarView; virtual; abstract;
  end;

  TScalarIntegerViewFactory = class(TScalarViewAbstractFactory)
  public
    constructor Create();

    function CreateView(Model: TScalar; Edit: TEdit): TScalarView; override;
  end;

  TScalarFloatViewFactory = class(TScalarViewAbstractFactory)
  public
    constructor Create();

    function CreateView(Model: TScalar; Edit: TEdit): TScalarView; override;
  end;

  TScalarController = class(TObject)
  private
    FModel: TScalar;
    FView: TScalarView;
  public
    constructor Create(Edit: TEdit; Factory: TScalarViewAbstractFactory);
    destructor Destroy(); override;

    procedure Reset();

    procedure SetValue(Value: string);

    property Model: TScalar read FModel;
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

function TScalar.IsZero(): Boolean;
begin
  Result := (FValue = 0.0);
end;

procedure TScalar.SetValue(NewValue: Extended);
begin
  FValue := NewValue;

  Notify();
end;

{ TScalarView }

constructor TScalarView.Create(Model: TScalar; Edit: TEdit);
begin
  inherited Create();

  FModel := Model;
  FEdit := Edit;

  FModel.Attach(Self);
end;

destructor TScalarView.Destroy();
begin
  FModel.Detach(Self);

  inherited Destroy();
end;

procedure TScalarView.Update();
begin
  if not ((Length(FEdit.Text) = 0) and FModel.IsZero()) then
    if FModel.IsZero() then
      FEdit.Clear()
    else
        FEdit.Text := FloatToString(FModel.Value);
end;

{ TScalarIntegerView }

constructor TScalarIntegerView.Create(Model: TScalar; Edit: TEdit);
begin
  inherited Create(Model, Edit);
end;

function TScalarIntegerView.FloatToString(Value: Extended): string;
begin
  Result := FloatToStrF(Value, ffFixed, 6, 0);
end;

{ TScalarFloatView }

constructor TScalarFloatView.Create(Model: TScalar; Edit: TEdit);
begin
  inherited Create(Model, Edit);
end;

function TScalarFloatView.FloatToString(Value: Extended): string;
begin
  Result := FloatToStrF(Value, ffFixed, 1, 3);
end;

{ TScalarViewAbstractFactory }

constructor TScalarViewAbstractFactory.Create();
begin
  inherited Create();
end;

{ TScalarIntegerViewFactory }

constructor TScalarIntegerViewFactory.Create();
begin
  inherited Create();
end;

function TScalarIntegerViewFactory.CreateView(Model: TScalar; Edit: TEdit): TScalarView;
begin
  Result := TScalarIntegerView.Create(Model, Edit);
end;

{ TScalarFloatViewFactory }

constructor TScalarFloatViewFactory.Create();
begin
  inherited Create();
end;

function TScalarFloatViewFactory.CreateView(Model: TScalar; Edit: TEdit): TScalarView;
begin
  Result := TScalarFloatView.Create(Model, Edit);
end;

{ TScalarController }

constructor TScalarController.Create(Edit: TEdit; Factory: TScalarViewAbstractFactory);
begin
  inherited Create();

  FModel := TScalar.Create();
  FView := Factory.CreateView(FModel, Edit);
end;

destructor TScalarController.Destroy();
begin
  FView.Free();
  FModel.Free();

  inherited Destroy();
end;

procedure TScalarController.Reset();
begin
  FModel.Reset();
end;

procedure TScalarController.SetValue(Value: string);
begin
  if Length(Value) <> 0 then
    FModel.Value := StrToFloat(Value)
  else
    Reset();
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

