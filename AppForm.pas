unit AppForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, EditBtn, AppCore, AppLogic;

type

  { TMainForm }

  TMainForm = class(TForm)
    BarWeight: TEdit;
    BarCount: TEdit;
    NormalFlowRate: TFloatSpinEdit;
    ResetWeight: TButton;
    MetalWeight1: TEdit;
    MetalWeight2: TEdit;
    MetalWeight3: TEdit;
    MetalWeight4: TEdit;
    MetalWeight5: TEdit;
    MetalWeight: TEdit;
    ActualFlowRate: TEdit;
    Deficit: TEdit;
    HeatWeight: TEdit;
    HeatSettings: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MetalSettings: TGroupBox;
    FlowRateSettings: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure BarCountChange(Sender: TObject);
    procedure BarWeightChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MetalWeight1Change(Sender: TObject);
    procedure MetalWeight2Change(Sender: TObject);
    procedure MetalWeight3Change(Sender: TObject);
    procedure MetalWeight4Change(Sender: TObject);
    procedure MetalWeight5Change(Sender: TObject);
    procedure NormalFlowRateChange(Sender: TObject);
    procedure ResetWeightClick(Sender: TObject);
  private
    FBarWeightController: TScalarController;
    FBarCountController: TScalarController;
    FHeatWeightController: TScalarController;

    FMetalWeight1Controller: TScalarController;
    FMetalWeight2Controller: TScalarController;
    FMetalWeight3Controller: TScalarController;
    FMetalWeight4Controller: TScalarController;
    FMetalWeight5Controller: TScalarController;
    FMetalWeightController: TScalarController;

    FNormalFlowRateController: TScalarController;
    FActualFlowRateController: TScalarController;
    FDeficitController: TScalarController;

    FHeatWeightLogic: THeatWeightLogic;
    FMetalWeightLogic: TMetalWeightLogic;
    FFlowRateLogic: TFlowRateLogic;
    FDeficitLogic: TDeficitLogic;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  ScalarIntegerViewFactory: TScalarIntegerViewFactory;
  ScalarFloatViewFactory: TScalarFloatViewFactory;
begin
  ScalarIntegerViewFactory := TScalarIntegerViewFactory.Create();
  ScalarFloatViewFactory := TScalarFloatViewFactory.Create();

  FBarWeightController := TScalarController.Create(BarWeight, ScalarIntegerViewFactory);
  FBarCountController := TScalarController.Create(BarCount, ScalarIntegerViewFactory);
  FHeatWeightController := TScalarController.Create(HeatWeight, ScalarIntegerViewFactory);

  FMetalWeight1Controller := TScalarController.Create(MetalWeight1, ScalarIntegerViewFactory);
  FMetalWeight2Controller := TScalarController.Create(MetalWeight2, ScalarIntegerViewFactory);
  FMetalWeight3Controller := TScalarController.Create(MetalWeight3, ScalarIntegerViewFactory);
  FMetalWeight4Controller := TScalarController.Create(MetalWeight4, ScalarIntegerViewFactory);
  FMetalWeight5Controller := TScalarController.Create(MetalWeight5, ScalarIntegerViewFactory);
  FMetalWeightController := TScalarController.Create(MetalWeight, ScalarIntegerViewFactory);

  FNormalFlowRateController := TScalarController.Create();
  FActualFlowRateController := TScalarController.Create(ActualFlowRate, ScalarFloatViewFactory);
  FDeficitController := TScalarController.Create(Deficit, ScalarIntegerViewFactory);

  FHeatWeightLogic := THeatWeightLogic.Create(FBarWeightController.Model,
                                              FBarCountController.Model,
                                              FHeatWeightController.Model);
  FMetalWeightLogic := TMetalWeightLogic.Create(FMetalWeight1Controller.Model,
                                                FMetalWeight2Controller.Model,
                                                FMetalWeight3Controller.Model,
                                                FMetalWeight4Controller.Model,
                                                FMetalWeight5Controller.Model,
                                                FMetalWeightController.Model);
  FFlowRateLogic := TFlowRateLogic.Create(FHeatWeightController.Model,
                                          FMetalWeightController.Model,
                                          FActualFlowRateController.Model);
  FDeficitLogic := TDeficitLogic.Create(FHeatWeightController.Model,
                                        FMetalWeightController.Model,
                                        FNormalFlowRateController.Model,
                                        FActualFlowRateController.Model,
                                        FDeficitController.Model);

  ScalarFloatViewFactory.Free();
  ScalarIntegerViewFactory.Free();
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FHeatWeightLogic.Free();
  FMetalWeightLogic.Free();
  FFlowRateLogic.Free();
  FDeficitLogic.Free();

  FHeatWeightController.Free();
  FBarCountController.Free();
  FBarWeightController.Free();

  FMetalWeightController.Free();
  FMetalWeight5Controller.Free();
  FMetalWeight4Controller.Free();
  FMetalWeight3Controller.Free();
  FMetalWeight2Controller.Free();
  FMetalWeight1Controller.Free();

  FDeficitController.Free();
  FActualFlowRateController.Free();
  FNormalFlowRateController.Free();
end;

procedure TMainForm.BarWeightChange(Sender: TObject);
begin
  FBarWeightController.SetValue(TEdit(Sender).Text);
end;

procedure TMainForm.BarCountChange(Sender: TObject);
begin
  FBarCountController.SetValue(TEdit(Sender).Text);
end;

procedure TMainForm.MetalWeight1Change(Sender: TObject);
begin
  FMetalWeight1Controller.SetValue(TEdit(Sender).Text);
end;

procedure TMainForm.MetalWeight2Change(Sender: TObject);
begin
  FMetalWeight2Controller.SetValue(TEdit(Sender).Text);
end;

procedure TMainForm.MetalWeight3Change(Sender: TObject);
begin
  FMetalWeight3Controller.SetValue(TEdit(Sender).Text);
end;

procedure TMainForm.MetalWeight4Change(Sender: TObject);
begin
  FMetalWeight4Controller.SetValue(TEdit(Sender).Text);
end;

procedure TMainForm.MetalWeight5Change(Sender: TObject);
begin
  FMetalWeight5Controller.SetValue(TEdit(Sender).Text);
end;

procedure TMainForm.NormalFlowRateChange(Sender: TObject);
begin
  FNormalFlowRateController.SetValue(TFloatSpinEdit(Sender).Value);
end;

procedure TMainForm.ResetWeightClick(Sender: TObject);
begin
  FMetalWeight1Controller.Reset();
  FMetalWeight2Controller.Reset();
  FMetalWeight3Controller.Reset();
  FMetalWeight4Controller.Reset();
  FMetalWeight5Controller.Reset();
end;

end.

