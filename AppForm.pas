unit AppForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, AppCore, AppLogic;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
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
    procedure Button1Click(Sender: TObject);
    procedure ChangeMode(Sender: TObject);
    procedure ClearOnClick(Sender: TObject);
    procedure Edit10Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure Edit8Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBilletWeight: TValuePresenter;
    FNumOfBillets: TValuePresenter;
    FHeatWeight: TValuePresenter;

    FMetalWeight1: TValuePresenter;
    FMetalWeight2: TValuePresenter;
    FMetalWeight3: TValuePresenter;
    FMetalWeight4: TValuePresenter;
    FMetalWeight5: TValuePresenter;
    FMetalWeightT: TValuePresenter;

    FNormalRate: TValuePresenter;
    FActualRate: TValuePresenter;
    FDeficit: TValuePresenter;

    FHeatWeightLogic: TLogic;
    FMetalWeightLogic: TLogic;
    FActualRateLogic: TLogic;
    FDeficitLogic: TLogic;

  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBilletWeight := TIntegerValuePresenter.Create(Edit1);
  FNumOfBillets := TIntegerValuePresenter.Create(Edit2);
  FHeatWeight := TIntegerValuePresenter.Create(Edit3);
  FHeatWeightLogic := THeatWeightLogic.Create(FBilletWeight.GetModel(),
                                              FNumOfBillets.GetModel(),
                                              FHeatWeight.GetModel());

  FMetalWeight1 := TIntegerValuePresenter.Create(Edit4);
  FMetalWeight2 := TIntegerValuePresenter.Create(Edit5);
  FMetalWeight3 := TIntegerValuePresenter.Create(Edit6);
  FMetalWeight4 := TIntegerValuePresenter.Create(Edit7);
  FMetalWeight5 := TIntegerValuePresenter.Create(Edit8);
  FMetalWeightT := TIntegerValuePresenter.Create(Edit9);
  FMetalWeightLogic := TMetalWeightLogic.Create(FMetalWeight1.GetModel(),
                                                FMetalWeight2.GetModel(),
                                                FMetalWeight3.GetModel(),
                                                FMetalWeight4.GetModel(),
                                                FMetalWeight5.GetModel(),
                                                FMetalWeightT.GetModel());

  FNormalRate := TFloatValuePresenter.Create(Edit10, 1.0);
  FActualRate := TFloatValuePresenter.Create(Edit11);
  FActualRateLogic := TActualRateLogic.Create(FHeatWeight.GetModel(),
                                              FMetalWeightT.GetModel(),
                                              FActualRate.GetModel());
  FDeficit := TIntegerValuePresenter.Create(Edit12);
  FDeficitLogic := TDeficitLogic.Create(FHeatWeight.GetModel(),
                                        FMetalWeightT.GetModel(),
                                        FActualRate.GetModel(),
                                        FNormalRate.GetModel(),
                                        FDeficit.GetModel());
end;

procedure TMainForm.Edit1Change(Sender: TObject);
begin
  FBilletWeight.UpdateModel();
end;

procedure TMainForm.Edit10Change(Sender: TObject);
begin
  FNormalRate.UpdateModel();
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Edit4.Clear();
  Edit5.Clear();
  Edit6.Clear();
  Edit7.Clear();
  Edit8.Clear();
end;

procedure TMainForm.ChangeMode(Sender: TObject);
begin
  Edit2.Clear();
end;

procedure TMainForm.ClearOnClick(Sender: TObject);
var
  TextBox: TEdit;
begin
  TextBox := TEdit(Sender);

  if not TextBox.ReadOnly then
    TextBox.Clear();
end;

procedure TMainForm.Edit2Change(Sender: TObject);
begin
  FNumOfBillets.UpdateModel();
end;

procedure TMainForm.Edit3Change(Sender: TObject);
begin
  FHeatWeight.UpdateModel();
end;

procedure TMainForm.Edit4Change(Sender: TObject);
begin
  FMetalWeight1.UpdateModel();
end;

procedure TMainForm.Edit5Change(Sender: TObject);
begin
  FMetalWeight2.UpdateModel();
end;

procedure TMainForm.Edit6Change(Sender: TObject);
begin
  FMetalWeight3.UpdateModel();
end;

procedure TMainForm.Edit7Change(Sender: TObject);
begin
  FMetalWeight4.UpdateModel();
end;

procedure TMainForm.Edit8Change(Sender: TObject);
begin
  FMetalWeight5.UpdateModel();
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FHeatWeightLogic.Free();
  FMetalWeightLogic.Free();
  FActualRateLogic.Free();
  FDeficitLogic.Free();

  FBilletWeight.Free();
  FNumOfBillets.Free();
  FHeatWeight.Free();

  FMetalWeight1.Free();
  FMetalWeight2.Free();
  FMetalWeight3.Free();
  FMetalWeight4.Free();
  FMetalWeight5.Free();
  FMetalWeightT.Free();

  FNormalRate.Free();
  FActualRate.Free();
  FDeficit.Free();
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FBilletWeight.Update();
  FNumOfBillets.Update();
  FHeatWeight.Update();

  FMetalWeight1.Update();
  FMetalWeight2.Update();
  FMetalWeight3.Update();
  FMetalWeight4.Update();
  FMetalWeight5.Update();
  FMetalWeightT.Update();

  FNormalRate.Update();
  FActualRate.Update();
  FDeficit.Update();
end;

end.

