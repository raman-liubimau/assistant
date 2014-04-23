unit AppLogic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AppCore;

type

  TLogic = class abstract(TObserver)
  private
    FTargetObject: TObject;
    FDependencies: TList;
  protected
    procedure AddDependency(Dependency: TSubject; NeedSubscribe: Boolean = True);
    procedure PerformAction(TargetObject: TObject; Dependencies: TList); virtual; abstract;
  public
    constructor Create(TargetObject: TObject);
    destructor Destroy(); override;

    procedure Update(); override;
  end;

  THeatWeightLogic = class(TLogic)
  protected
    procedure PerformAction(TargetObject: TObject; Dependencies: TList); override;
  public
    constructor Create(BilletWeight, NumOfBillets, HeatWeight: TCustomValue);
  end;

  TMetalWeightLogic = class(TLogic)
  protected
    procedure PerformAction(TargetObject: TObject; Dependencies: TList); override;
  public
    constructor Create(MetalWeight1, MetalWeight2, MetalWeight3, MetalWeight4,
                       MetalWeight5, MetalWeightT: TCustomValue);
  end;

  TActualRateLogic = class(TLogic)
  protected
    procedure PerformAction(TargetObject: TObject; Dependencies: TList); override;
  public
    constructor Create(HeatWeight, MetalWeight, ActualRate: TCustomValue);
  end;

  TDeficitLogic = class(TLogic)
  protected
    procedure PerformAction(TargetObject: TObject; Dependencies: TList); override;
  public
    constructor Create(HeatWeight, MetalWeight, ActualRate, NormalRate, Deficit: TCustomValue);
  end;

implementation

{ Helpers }

procedure SetValue(TargetObject: TObject; Value: Extended); inline;
begin
  TCustomValue(TargetObject).Value := Value;
end;

{ TLogic }

constructor TLogic.Create(TargetObject: TObject);
begin
  FTargetObject := TargetObject;
  FDependencies := TList.Create();
end;

destructor TLogic.Destroy();
begin
  with FDependencies.GetEnumerator() do
  begin
    while MoveNext() do
      TSubject(GetCurrent()).Detach(Self);

    Free();
  end;

  FDependencies.Free();

  inherited Destroy();
end;

procedure TLogic.Update();
begin
  PerformAction(FTargetObject, FDependencies);
end;

procedure TLogic.AddDependency(Dependency: TSubject; NeedSubscribe: Boolean);
begin
  if NeedSubscribe then
    Dependency.Attach(Self);

  FDependencies.Add(Dependency);
end;

{ THeatWeightLogic }

constructor THeatWeightLogic.Create(BilletWeight, NumOfBillets, HeatWeight: TCustomValue);
begin
  inherited Create(HeatWeight);

  AddDependency(BilletWeight);
  AddDependency(NumOfBillets);
end;

procedure THeatWeightLogic.PerformAction(TargetObject: TObject; Dependencies: TList);
var
  BilletWeight, NumOfBillets: Extended;
begin
  BilletWeight := TCustomValue(Dependencies.Items[0]).Value;
  NumOfBillets := TCustomValue(Dependencies.Items[1]).Value;

  SetValue(TargetObject, (BilletWeight * NumOfBillets));
end;

{ TMetalWeightLogic }

constructor TMetalWeightLogic.Create(MetalWeight1, MetalWeight2, MetalWeight3,
                                     MetalWeight4, MetalWeight5, MetalWeightT: TCustomValue);
begin
  inherited Create(MetalWeightT);

  AddDependency(MetalWeight1);
  AddDependency(MetalWeight2);
  AddDependency(MetalWeight3);
  AddDependency(MetalWeight4);
  AddDependency(MetalWeight5);
end;

procedure TMetalWeightLogic.PerformAction(TargetObject: TObject; Dependencies: TList);
var
  TotalWeight: Extended;
begin
  TotalWeight := 0.0;

  with Dependencies.GetEnumerator() do
  begin
    while MoveNext() do
      TotalWeight += TCustomValue(GetCurrent()).Value;

    Free();
  end;

  SetValue(TargetObject, TotalWeight);
end;

{ TActualRateLogic }

constructor TActualRateLogic.Create(HeatWeight, MetalWeight, ActualRate: TCustomValue);
begin
  inherited Create(ActualRate);

  AddDependency(HeatWeight);
  AddDependency(MetalWeight);
end;

procedure TActualRateLogic.PerformAction(TargetObject: TObject; Dependencies: TList);
var
  HeatWeight, MetalWeight, ActualRate: Extended;
begin
  HeatWeight := TCustomValue(Dependencies.Items[0]).Value;
  MetalWeight := TCustomValue(Dependencies.Items[1]).Value;

  if (HeatWeight > MetalWeight) and not (MetalWeight = 0.0)  then
  begin
    ActualRate := HeatWeight / MetalWeight;

    if (ActualRate < 1.1) then
    begin
      SetValue(TargetObject, ActualRate);
      Exit();
    end;
  end;

  SetValue(TargetObject, 0.0);
end;

{ TDeficitLogic }

constructor TDeficitLogic.Create(HeatWeight, MetalWeight, ActualRate, NormalRate, Deficit: TCustomValue);
begin
  inherited Create(Deficit);

  AddDependency(HeatWeight);
  AddDependency(MetalWeight);
  AddDependency(ActualRate, False);
  AddDependency(NormalRate);
end;

procedure TDeficitLogic.PerformAction(TargetObject: TObject; Dependencies: TList);
var
  HeatWeight, MetalWeight, ActualRate, NormalRate: Extended;
begin
  HeatWeight := TCustomValue(Dependencies.Items[0]).Value;
  MetalWeight := TCustomValue(Dependencies.Items[1]).Value;
  ActualRate := TCustomValue(Dependencies.Items[2]).Value;
  NormalRate := TCustomValue(Dependencies.Items[3]).Value;

  if (NormalRate > 1.0) and (NormalRate < 2.0) then
    if not ((HeatWeight = 0.0) or (HeatWeight < MetalWeight)) then
      if (ActualRate > NormalRate) or (ActualRate = 0.0) then
      begin
        SetValue(TargetObject, Round(HeatWeight / NormalRate - MetalWeight));
        Exit();
      end;

  SetValue(TargetObject, 0.0);
end;

end.

