unit AppLogic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AppCore;

type
  THeatWeightLogic = class(TObserver)
  private
    FBarWeight: TScalar;
    FBarCount: TScalar;
    FHeatWeight: TScalar;
  public
    constructor Create(BarWeight, BarCount, HeatWeight: TScalar);
    destructor Destroy(); override;

    procedure Update(); override;
  end;

  TMetalWeightLogic = class(TObserver)
  private
    FMetalWeight1: TScalar;
    FMetalWeight2: TScalar;
    FMetalWeight3: TScalar;
    FMetalWeight4: TScalar;
    FMetalWeight5: TScalar;
    FMetalWeight: TScalar;
  public
    constructor Create(MetalWeight1, MetalWeight2, MetalWeight3,
                       MetalWeight4, MetalWeight5, MetalWeight: TScalar);
    destructor Destroy(); override;

    procedure Update(); override;
  end;

  TFlowRateLogic = class(TObserver)
  private
    FHeatWeight: TScalar;
    FMetalWeight: TScalar;
    FFlowRate: TScalar;
  public
    constructor Create(HeatWeight, MetalWeight, FlowRate: TScalar);
    destructor Destroy(); override;

    procedure Update(); override;
  end;

  TDeficitLogic = class(TObserver)
  private
    FHeatWeight: TScalar;
    FMetalWeight: TScalar;
    FNormalFlowRate: TScalar;
    FActualFlowRate: TScalar;
    FDeficit: TScalar;
  public
    constructor Create(HeatWeight, MetalWeight, NormalFlowRate,
                       ActualFlowRate, Deficit: TScalar);
    destructor Destroy(); override;

    procedure Update(); override;
  end;

implementation

{ THeatWeightLogic }

constructor THeatWeightLogic.Create(BarWeight, BarCount, HeatWeight: TScalar);
begin
  inherited Create();

  FBarWeight := BarWeight;
  FBarCount := BarCount;
  FHeatWeight := HeatWeight;

  FBarWeight.Attach(Self);
  FBarCount.Attach(Self);
end;

destructor THeatWeightLogic.Destroy();
begin
  FBarCount.Detach(Self);
  FBarWeight.Detach(Self);

  inherited Destroy();
end;

procedure THeatWeightLogic.Update();
begin
  FHeatWeight.Value := FBarWeight.Value * FBarCount.Value;
end;

{ TMetalWeightLogic }

constructor TMetalWeightLogic.Create(MetalWeight1, MetalWeight2, MetalWeight3,
                                     MetalWeight4, MetalWeight5, MetalWeight: TScalar);
begin
  inherited Create();

  FMetalWeight1 := MetalWeight1;
  FMetalWeight2 := MetalWeight2;
  FMetalWeight3 := MetalWeight3;
  FMetalWeight4 := MetalWeight4;
  FMetalWeight5 := MetalWeight5;
  FMetalWeight := MetalWeight;

  FMetalWeight1.Attach(Self);
  FMetalWeight2.Attach(Self);
  FMetalWeight3.Attach(Self);
  FMetalWeight4.Attach(Self);
  FMetalWeight5.Attach(Self);
end;

destructor TMetalWeightLogic.Destroy();
begin
  FMetalWeight5.Detach(Self);
  FMetalWeight4.Detach(Self);
  FMetalWeight3.Detach(Self);
  FMetalWeight2.Detach(Self);
  FMetalWeight1.Detach(Self);

  inherited Destroy();
end;

procedure TMetalWeightLogic.Update();
begin
  FMetalWeight.Value := FMetalWeight1.Value + FMetalWeight2.Value +
                        FMetalWeight3.Value + FMetalWeight4.Value +
                        FMetalWeight5.Value;
end;

{ TFlowRateLogic }

constructor TFlowRateLogic.Create(HeatWeight, MetalWeight, FlowRate: TScalar);
begin
  inherited Create();

  FHeatWeight := HeatWeight;
  FMetalWeight := MetalWeight;
  FFlowRate := FlowRate;

  FHeatWeight.Attach(Self);
  FMetalWeight.Attach(Self);
end;

destructor TFlowRateLogic.Destroy();
begin
  FMetalWeight.Detach(Self);
  FHeatWeight.Detach(Self);

  inherited Destroy();
end;

procedure TFlowRateLogic.Update();
var
  Rate: Extended;
begin
  if not ((FMetalWeight > FHeatWeight) and (FMetalWeight.Value = 0.0)) then
  begin
    Rate := FHeatWeight.Value / FMetalWeight.Value;

    if Rate < 1.1 then
      FFlowRate.Value := Rate
    else
      FFlowRate.Reset();
  end
  else
    FFlowRate.Reset();
end;

{ TDeficitLogic }

constructor TDeficitLogic.Create(HeatWeight, MetalWeight, NormalFlowRate,
                                 ActualFlowRate, Deficit: TScalar);
begin
  inherited Create();

  FHeatWeight := HeatWeight;
  FMetalWeight := MetalWeight;
  FNormalFlowRate := NormalFlowRate;
  FActualFlowRate := ActualFlowRate;
  FDeficit := Deficit;

  FHeatWeight.Attach(Self);
  FMetalWeight.Attach(Self);
  FNormalFlowRate.Attach(Self);
  FActualFlowRate.Attach(Self);
end;

destructor TDeficitLogic.Destroy();
begin
  FActualFlowRate.Detach(Self);
  FNormalFlowrate.Detach(Self);
  FMetalWeight.Detach(Self);
  FHeatWeight.Detach(Self);

  inherited Destroy();
end;

procedure TDeficitLogic.Update();
begin
  if FNormalFlowRate.Value <> 0.0 then
    if (FActualFlowRate > FNormalFlowRate) or (FActualFlowRate.Value = 0.0) then
      FDeficit.Value := FHeatWeight.Value / FNormalFlowRate.Value - FMetalWeight.Value
    else
      FDeficit.Reset()
  else
    FDeficit.Reset();
end;

end.

