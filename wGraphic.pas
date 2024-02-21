unit wGraphic;

interface

uses wRead, Classes, Types, IniFiles, Graphics, ExtCtrls, SysUtils;

function CRectToGRect(pRect :TRect) :TRect;

type

  TGraphicsDirection = (gdNone, gdLeft, gdTop, gdRight, gdBottom);

  TGraphics = class( TModule)
   private
    GraphicsRect: TRect;
    function GetIndex(const pName: String): Integer;
   public
    constructor Create(const pFileName: String);
    destructor Destroy; override;
    procedure CreateGraphicsItem(const pIni: TIniFile; const pName: String);
    procedure Save;
    procedure Paint(const pCanvas: TCanvas);
    procedure ChangeLanguage;
    procedure Refresh;
    procedure SetImageProperties( pImage: TImage);
    procedure RefreshImage( pImage: TImage);
    procedure Execute;
   published
    property _Rect: TRect read GraphicsRect;
  end;

  TGraphicsItem = class
   private
    GraphicsName: String;
    GraphicsRect: TRect;
    GraphicsHiColor, GraphicsLowColor, GraphicsColor, GraphicsAlertColor: TColor;
    GraphicsDirection: TGraphicsDirection;
    GraphicsCaptionRect: TRect;
    GraphicsCaption: String;
    GraphicsFont: String;
    GraphicsValue: String;
    GraphicsAlertValue: String;
    GraphicsAlertStatus: Boolean;
    GraphicsChange: Boolean;
    procedure Paint(const pCanvas: TCanvas); virtual;
    procedure PaintCaption(const pCanvas: TCanvas);
    function GetValue: Boolean;
    function GetAlertValue: Boolean;
    function GetChange: Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetFont: TFont;
    function GetEnabledCaption: Boolean;
    function StrToDirection(const pStr: String): TGraphicsDirection;
   public
    constructor Create(const pIni: TIniFile; pName: String);
    procedure ChangeLanguage(const pIni: TIniFile);
   published
    property Name: String read GraphicsName;
    property Rect: TRect read GraphicsRect;

    property HiColor: TColor read GraphicsHiColor;
    property LowColor: TColor read GraphicsLowColor;
    property Color: TColor read GraphicsColor;
    property AlertColor: TColor read GraphicsAlertColor;
    property Font: TFont read GetFont;
    property Value: Boolean read GetValue;
    property Change: Boolean read GetChange write GraphicsChange;
    property Direction: TGraphicsDirection read GraphicsDirection;
    property Enabled_Caption: Boolean read GetEnabledCaption;
    property Caption: String read GraphicsCaption;
    property CaptionRect: TRect read GraphicsCaptionRect;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

  TGrTextItem = class( TGraphicsItem)
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrRectTextItem = class( TGraphicsItem)
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrNodeItem = class( TGraphicsItem)
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrPipeItem = class( TGraphicsItem)
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrLampItem = class( TGraphicsItem)
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrValveItem = class( TGraphicsItem)
    private
     procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrPumpItem = class( TGraphicsItem)
    private
     procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrDataItem = class( TGraphicsItem)
    GraphicsText: String;
   private
     procedure Paint(const pCanvas: TCanvas); override;
   public
     constructor Create(const pIni: TIniFile; pName: String);
   published
     property Text: String read GraphicsText;
  end;

  TGrDriveItem = class( TGraphicsItem) // Двигатель
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrTurnerItem = class( TGraphicsItem) // Кантователь
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrBunkerItem = class( TGraphicsItem) // Бункер
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrConveyorItem = class( TGraphicsItem) // Транспортер
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrFanItem = class( TGraphicsItem) // Вентилятор
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrTankItem = class( TGraphicsItem) // Бак
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrCoilItem = class( TGraphicsItem) // Змеевик
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrCoil1Item = class( TGraphicsItem) // Змеевик1
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrCoolItem = class( TGraphicsItem) // Холодильник
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrScrubberItem = class( TGraphicsItem) // Скруббер
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrMillItem = class( TGraphicsItem) // Мельница
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrReactorItem = class( TGraphicsItem) // Реактор
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrTank1Item = class( TGraphicsItem) // Бак1
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrRotorItem = class( TGraphicsItem) // Мешалка
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrBoxItem = class( TGraphicsItem) // Бокс
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

  TGrBallonItem = class( TGraphicsItem) // Баллон
   private
    procedure Paint(const pCanvas: TCanvas); override;
  end;

var _Graphics: TGraphics;

implementation

uses wMessage, wParameter, wProgramm, wCalc;

{ Public graphics function }
function CRectToGRect(pRect :TRect) :TRect;
begin
 Result := Rect(pRect.left,pRect.Top,pRect.left+pRect.Right,pRect.Top+pRect.Bottom);
end;

{ TGraphics }

constructor TGraphics.Create(const pFileName: String);
var fIni: TIniFile;
    TempList: TStringList;
    i: Integer;
begin
  inherited Create( pFileName, mtGraphic);
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  GraphicsRect := ReadRect( fIni, 'MAIN', 'RECT', GraphicsRect);
  TempList := TStringList.Create;
  ReadSections( fIni, TempList);
  for i := 0 to TempList.Count - 1 do
    if GetIndex( TempList.Strings[ i]) < 0 then CreateGraphicsItem( fIni, TempList.Strings[ i])
    else Msg('0000', 'Graphics object with name '+TempList.Strings[ i]+'. Already exist.');
  TempList.Free;
  ReadClose( fIni);
end;

destructor TGraphics.Destroy;
begin
  inherited Destroy;
end;

procedure TGraphics.ChangeLanguage;
var fIni: TIniFile;
    i: Integer;
begin
  fIni := ReadOpen( FileName);
  if fIni = nil then Exit;
  inherited ChangeLanguage( fIni);
  for i := 0 to List.Count - 1 do TGraphicsItem(List.Items[ i]).ChangeLanguage( fIni);
  ReadClose( fIni);
  Refresh;
end;

procedure TGraphics.CreateGraphicsItem(const pIni: TIniFile; const pName: String);
var GraphicsItem: TGraphicsItem;
  GrType :String;
begin
  GrType := UpperCase(Trim(ReadStr( pIni, pName, 'TYPE', '--NONE--')));
  if (GrType = '--NONE--') then Exit;
       if GrType = 'TEXT' then GraphicsItem := TGrTextItem.Create( pIni, pName)
  else if GrType = 'RECTTEXT' then GraphicsItem := TGrRectTextItem.Create( pIni, pName)
  else if GrType = 'NODE' then GraphicsItem := TGrNodeItem.Create( pIni, pName) // Узел
  else if GrType = 'PIPE' then GraphicsItem := TGrPipeItem.Create( pIni, pName) // Трубопровод
  else if GrType = 'LAMP' then GraphicsItem := TGrLampItem.Create( pIni, pName) // Лампа
  else if GrType = 'VALVE' then GraphicsItem := TGrValveItem.Create( pIni, pName) // Клапан
  else if GrType = 'PUMP' then GraphicsItem := TGrPumpItem.Create( pIni, pName) // Насос
  else if GrType = 'DATA' then GraphicsItem := TGrDataItem.Create( pIni, pName) // Данные
  else if GrType = 'DRIVE' then GraphicsItem := TGrDriveItem.Create( pIni, pName) // Двигатель
  else if GrType = 'TURNER' then GraphicsItem := TGrTurnerItem.Create( pIni, pName) // Кантователь
  else if GrType = 'BUNKER' then GraphicsItem := TGrBunkerItem.Create( pIni, pName) // Бункер
  else if GrType = 'CONVEYOR' then GraphicsItem := TGrConveyorItem.Create( pIni, pName) // Транспортер
  else if GrType = 'FAN' then GraphicsItem := TGrFanItem.Create( pIni, pName) // Вентилятор
  else if GrType = 'TANK' then GraphicsItem := TGrTankItem.Create( pIni, pName) // Бак
  else if GrType = 'COIL' then GraphicsItem := TGrCoilItem.Create( pIni, pName) // Змеевик
  else if GrType = 'COIL1' then GraphicsItem := TGrCoil1Item.Create( pIni, pName) // Змеевик1
  else if GrType = 'COOL' then GraphicsItem := TGrCoolItem.Create( pIni, pName) // Холодильник
  else if GrType = 'SCRUBBER' then GraphicsItem := TGrScrubberItem.Create( pIni, pName) // Скруббер
  else if GrType = 'MILL' then GraphicsItem := TGrMillItem.Create( pIni, pName) // Мельница
  else if GrType = 'REACTOR' then GraphicsItem := TGrReactorItem.Create( pIni, pName) // Реактор
  else if GrType = 'TANK1' then GraphicsItem := TGrTank1Item.Create( pIni, pName) // Бак1
  else if GrType = 'BALLON' then GraphicsItem := TGrBallonItem.Create( pIni, pName) // Баллон
  else if GrType = 'ROTOR' then GraphicsItem := TGrRotorItem.Create( pIni, pName) // Мешалка
  else if GrType = 'BOX' then GraphicsItem := TGrBoxItem.Create( pIni, pName) // Бокс
  else begin
    Msg('0000', 'Unknown graphics type '+GrType+'. The Loading break.');
    Exit;
  end;
  List.Add( GraphicsItem);
end;

procedure TGraphics.Save;
begin

end;

procedure TGraphics.Paint(const pCanvas: TCanvas);
var i: Integer;
begin
  if Change then begin
    pCanvas.Pen.Color := Color;
    pCanvas.Brush.Color := Color;
    pCanvas.Brush.Style := bsSolid;
    pCanvas.Rectangle( GraphicsRect);
    Change := FALSE;
  end;
  for i := 0 to List.Count - 1 do TGraphicsItem( List.Items[ i]).Paint( pCanvas);
end;

function TGraphics.GetIndex(const pName: String): Integer;
begin
  for Result := 0 to List.Count - 1 do if TGraphicsItem(List.Items[ Result]).Name = pName then Exit;
  Result := -1;
end;

procedure TGraphics.SetImageProperties(pImage: TImage);
begin
  pImage.Left := GraphicsRect.Left;
  pImage.Top := GraphicsRect.Top;
  pImage.Width := GraphicsRect.Right-GraphicsRect.Left;
  pImage.Height := GraphicsRect.Bottom-GraphicsRect.Top;
end;

procedure TGraphics.RefreshImage(pImage: TImage);
begin
  Paint( pImage.Canvas);
end;

procedure TGraphics.Execute;
begin
//
end;

procedure TGraphics.Refresh;
var i: Integer;
begin
  Change := TRUE;
  for i := 0 to List.Count - 1 do TGraphicsItem(List.Items[ i]).Change := TRUE;
end;

{ TGraphicsItem }

constructor TGraphicsItem.Create(const pIni: TIniFile; pName: String);
begin
  inherited Create;
  GraphicsName := pName;
  GraphicsRect := ReadRect( pIni, pName, 'RECT', GraphicsRect);
  GraphicsHiColor := ReadColor( pIni, pName, 'HICOLOR', GraphicsHiColor);
  GraphicsLowColor := ReadColor( pIni, pName, 'LOWCOLOR', GraphicsLowColor);
  GraphicsAlertColor := ReadColor( pIni, pName, 'ALERTCOLOR', GraphicsAlertColor);
  GraphicsColor := ReadColor( pIni, pName, 'COLOR', GraphicsColor);
  GraphicsDirection := StrToDirection( ReadStr( pIni, pName, 'DIRECTION', 'RIGHT'));
  GraphicsCaptionRect := ReadRect( pIni, pName, 'CAPTION_RECT', GraphicsCaptionRect);
  GraphicsCaption := ReadLangStr( pIni, pName, 'CAPTION', GraphicsCaption);
  GraphicsFont := ReadLangStr( pIni, pName, 'FONT', GraphicsFont);
  GraphicsValue := ReadStr( pIni, pName, 'VALUE', GraphicsValue);
  GraphicsAlertValue := ReadStr( pIni, pName, 'ALERTVALUE', GraphicsAlertValue);
end;

function TGraphicsItem.StrToDirection(const pStr: String): TGraphicsDirection;
begin
       if Pos( 'LEFT', pStr) > 0 then Result := gdLeft
  else if Pos( 'RIGHT', pStr) > 0 then Result := gdRight
  else if Pos( 'TOP', pStr) > 0 then Result := gdTop
  else if Pos( 'BOTTOM', pStr) > 0 then Result := gdBottom
  else Result := gdNone;
end;

procedure TGraphicsItem.Paint(const pCanvas: TCanvas);
begin
  pCanvas.Pen.Color := Color;
  if Enabled_Caption then PaintCaption( pCanvas);
end;

function TGraphicsItem.GetValue: Boolean;
begin
  if Length( GraphicsValue) > 0 then Result := CalcBool( GraphicsValue) else Result := FALSE;
end;

function TGraphicsItem.GetAlertValue: Boolean;
begin
  if Length( GraphicsAlertValue) > 0 then Result := CalcBool( GraphicsAlertValue) else Result := FALSE;
end;

function TGraphicsItem.GetChange: Boolean;
begin
  if not GraphicsChange and (Length( GraphicsValue) > 0) then GraphicsChange := Parameter.GetChange( GraphicsValue);
  Result := GraphicsChange;
end;

function TGraphicsItem.GetHeight: Integer;
begin
 Result := GraphicsRect.Bottom - GraphicsRect.Top;
end;

function TGraphicsItem.GetWidth: Integer;
begin
 Result := GraphicsRect.Right - GraphicsRect.Left;
end;

procedure TGraphicsItem.ChangeLanguage(const pIni: TIniFile);
begin
  GraphicsCaption := ReadLangStr( pIni, GraphicsName, 'CAPTION', GraphicsCaption);
  GraphicsFont := ReadLangStr( pIni, GraphicsName, 'FONT', GraphicsFont);
end;

function TGraphicsItem.GetEnabledCaption: Boolean;
begin
  if Length(GraphicsCaption) > 0 then Result := TRUE else Result := FALSE;
end;


procedure TGraphicsItem.PaintCaption(const pCanvas: TCanvas);
begin
//  if GetValue then pCanvas.Font.Color := HiColor else pCanvas.Font.Color := LowColor;
  pCanvas.Font.Assign( Font);
  pCanvas.Brush.Style := bsClear;
  pCanvas.TextOut(CaptionRect.Left, CaptionRect.Top, Caption);
  pCanvas.Brush.Style := bsSolid;
end;

function TGraphicsItem.GetFont: TFont;
begin
  Result := FontScheme.GetFont( GraphicsFont);
end;

{ TGrTextItem }

procedure TGrTextItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  if GetValue then pCanvas.Font.Color := HiColor else pCanvas.Font.Color := LowColor;
  pCanvas.Brush.Style := bsClear;
  pCanvas.TextOut(Rect.Left+round((Width-pCanvas.TextWidth(Caption))/2), Rect.Top+round((Height-pCanvas.TextHeight(Caption))/2), Caption);
  Change := FALSE;
end;

{ TGrRectTextItem }

procedure TGrRectTextItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
//  inherited Paint( pCanvas);
  pCanvas.Brush.Color := HiColor; pCanvas.Pen.Color := LowColor;
  pCanvas.RoundRect( Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, 10, 10);
  pCanvas.Font.Color := Color;
  pCanvas.TextOut(Rect.Left+round((Width-pCanvas.TextWidth( Caption))/2), Rect.Top+round((Height-pCanvas.TextHeight(Caption))/2), Caption);
  Change := FALSE;
end;

{ TGrNodeItem }

procedure TGrNodeItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.RoundRect( Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, 10, 10);
  pCanvas.Font.Color := clBlack;
  pCanvas.TextOut(Rect.Left+round((Width-pCanvas.TextWidth(Caption))/2), Rect.Top+1, Caption);
  pCanvas.MoveTo( Rect.Left+round(Width*0.02), Rect.Top+pCanvas.TextHeight(Caption)+3);
  pCanvas.LineTo( Rect.Right-round(Width*0.02), Rect.Top+pCanvas.TextHeight(Caption)+3);
  Change := FALSE;
end;

{ TGrPipeItem }

procedure TGrPipeItem.Paint(const pCanvas: TCanvas);
var t :Integer;
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Pen.Color := HiColor;
  if (Direction = gdLeft) or (Direction = gdRight) then begin
    pCanvas.Polyline([Point( Rect.Left, Rect.Top), Point( Rect.Right, Rect.Top)]);
    pCanvas.Pen.Color := LowColor;
    pCanvas.Polyline([Point( Rect.Left, Rect.Top -1), Point( Rect.Right, Rect.Top -1)]);
    pCanvas.Polyline([Point( Rect.Left, Rect.Top +1), Point( Rect.Right, Rect.Top +1)]);
    t := Width div 2;
    if t > 20 then begin
      t := t + Rect.Left;
      pCanvas.Pen.Color := LowColor;
      pCanvas.Brush.Color := HiColor;
      if Direction = gdLeft then pCanvas.Polygon([ Point(t, Rect.Top-3), Point(t-5,Rect.Top-3), Point(t-5,Rect.Top-6), Point(t-11,Rect.Top), Point(t-5,Rect.Top+6), Point(t-5,Rect.Top+3), Point(t,Rect.Top+3)])
      else pCanvas.Polygon([ Point(t, Rect.Top-3), Point(t+5,Rect.Top-3), Point(t+5,Rect.Top-6), Point(t+11,Rect.Top), Point(t+5,Rect.Top+6), Point(t+5,Rect.Top+3), Point(t,Rect.Top+3)]);
    end;
  end else if (Direction = gdTop) or (Direction = gdBottom) then begin
    pCanvas.Polyline([Point( Rect.Left, Rect.Top), Point( Rect.Left,Rect.Bottom )]);
    pCanvas.Pen.Color := LowColor;
    pCanvas.Polyline([Point( Rect.Left-1, Rect.Top), Point( Rect.Left-1, Rect.Bottom)]);
    pCanvas.Polyline([Point( Rect.Left+1, Rect.Top), Point( Rect.Left+1, Rect.Bottom)]);
    t := Height div 2;
    if t > 20 then begin
      t := t + Rect.Top;
      pCanvas.Pen.Color := LowColor;
      pCanvas.Brush.Color := HiColor;
      if Direction = gdTop then pCanvas.Polygon([ Point(Rect.Left-3, t), Point(Rect.Left-3,t-6), Point(Rect.Left-6,t-6), Point(Rect.Left,t-11), Point(Rect.Left+6,t-6), Point(Rect.Left+3,t-6), Point(Rect.Left+3,t)])
      else pCanvas.Polygon([ Point(Rect.Left-3, t), Point(Rect.Left-3,t+6), Point(Rect.Left-6,t+6), Point(Rect.Left,t+11), Point(Rect.Left+6,t+6), Point(Rect.Left+3,t+6), Point(Rect.Left+3,t)]);
    end;
  end;
  Change := FALSE;
end;

{ TGrLampItem }

procedure TGrLampItem.Paint( const pCanvas : TCanvas);
begin
  inherited Paint( pCanvas);
  if GetAlertValue then begin
    if GraphicsAlertStatus then pCanvas.Brush.Color := AlertColor else
        if GetValue then pCanvas.Brush.Color := HiColor else pCanvas.Brush.Color := LowColor;
    GraphicsAlertStatus := not GraphicsAlertStatus;
  end else
  if GetValue then pCanvas.Brush.Color := HiColor else pCanvas.Brush.Color := LowColor;
  pCanvas.Rectangle(Rect);
//  pCanvas.Ellipse(Rect);
end;

{ TValveItem }

procedure TGrValveItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  if GetValue then pCanvas.Brush.Color := HiColor else pCanvas.Brush.Color := LowColor;
  if Direction in [gdLeft, gdRight] then
    pCanvas.Polygon([Point( Rect.Left, Rect.Top), Point( Rect.Right, Rect.Bottom), Point( Rect.Right, Rect.Top), Point( Rect.Left, Rect.Bottom)])
  else if Direction in [gdTop, gdBottom] then
    pCanvas.Polygon([Point( Rect.Left, Rect.Top), Point( Rect.Right, Rect.Bottom), Point( Rect.Left, Rect.Bottom), Point( Rect.Right, Rect.Top)]);
  Change := FALSE;
end;

{ TGrPumpItem }

procedure TGrPumpItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  if GetValue then pCanvas.Brush.Color := HiColor else pCanvas.Brush.Color := LowColor;
  pCanvas.Ellipse( Rect);
  pCanvas.Brush.Color := pCanvas.Pen.Color;
  if Direction = gdLeft then
    pCanvas.Polygon([Point( Rect.Left,round(Rect.Top+Height*0.5)), Point( round(Rect.Left+Width*0.4), round(Rect.Top+Height*0.25)), Point(round(Rect.Left+Width*0.4),round(Rect.Top+Height*0.75))])
  else if Direction = gdTop then
    pCanvas.Polygon([Point( round(Rect.Left+Width*0.5),Rect.Top), Point( round(Rect.Left+Width*0.25), round(Rect.Top+Height*0.4)), Point(round(Rect.Left+Width*0.75),round(Rect.Top+Height*0.4))])
  else if Direction = gdRight then
    pCanvas.Polygon([Point( Rect.Right,round(Rect.Top+Height*0.5)), Point(round(Rect.Right-Width*0.4),round(Rect.Top+Height*0.25)), Point(round(Rect.Right-Width*0.4),round(Rect.Top+Height*0.75))])
  else if Direction = gdBottom then
    pCanvas.Polygon([Point( round(Rect.Left+Width*0.5),Rect.Bottom), Point(round(Rect.Left+Width*0.25),round(Rect.Bottom-Height*0.4)), Point(round(Rect.Left+Width*0.75),round(Rect.Bottom-Height*0.4))]);
  Change := FALSE;
end;

{ TGrDataItem }

constructor TGrDataItem.Create(const pIni: TIniFile; pName: String);
begin
  inherited Create( pIni, pName);
  GraphicsText := ReadStr( pIni, pName, 'TEXT', '');
end;

procedure TGrDataItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Brush.Color := LowColor;
  pCanvas.Rectangle(Rect);
  if GetValue then pCanvas.Font.Color := HiColor else pCanvas.Font.Color := clWhite;
  pCanvas.TextOut(Rect.Left+1, Rect.Top+1, Parameter.GetStr( GraphicsValue));
  pCanvas.Font.Color := clYellow;
  pCanvas.TextOut( Rect.Right-pCanvas.TextWidth(Text)-1, Rect.Top+1, Text);
  Change := FALSE;
end;

{ TGrDriveItem }

procedure TGrDriveItem.Paint(const pCanvas: TCanvas);
var t : Integer;
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  if GetValue then pCanvas.Brush.Color := HiColor else pCanvas.Brush.Color := LowColor;
  if Direction = gdLeft then begin
    t := Height div 2;
    pCanvas.Polyline([Point( Rect.Left, Rect.Top+t), Point( Rect.Right-Height, Rect.Top+t)]);
    pCanvas.Ellipse( Rect.Right-Height, Rect.Top, Rect.Right, Rect.Bottom);
  end else if Direction = gdRight then begin
    t := Height div 2;
    pCanvas.Polyline([Point( Rect.Left+Height, Rect.Top+t), Point( Rect.Right, Rect.Top+t)]);
    pCanvas.Ellipse( Rect.Left, Rect.Top, Rect.Left+Height, Rect.Bottom);
  end else if Direction = gdTop then begin
    pCanvas.Ellipse( Rect);
  end else if Direction = gdBottom then begin
    pCanvas.Ellipse( Rect);
  end;
  Change := FALSE;
end;

{ TGrTurnerItem }

procedure TGrTurnerItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Pen.Color := LowColor;
  pCanvas.Brush.Style := bsClear;
  pCanvas.Rectangle(Rect);
  Change := FALSE;
end;

{ TGrBunkerItem }

procedure TGrBunkerItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Brush.Color := LowColor; pCanvas.Pen.Color := LowColor;
  pCanvas.Polygon([ Point(Rect.Left, Rect.Top), Point(Rect.Right, Rect.Top), Point(Rect.Left+Width div 2, Rect.Bottom)]);
  Change := FALSE;
end;

{ TGrConveyorItem }

procedure TGrConveyorItem.Paint(const pCanvas: TCanvas);
var i,d: Integer;
  u: Boolean;
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Brush.Color := HiColor; pCanvas.Pen.Color := LowColor;
  pCanvas.Brush.Style := bsClear;
  pCanvas.Rectangle(Rect);
  d := Height div 3;
  i := Width-d;
  u := TRUE;
  pCanvas.MoveTo(Rect.Right, Rect.Top);
  while i > 0 do begin
    if u then begin
      pCanvas.LineTo(Rect.Left+i, Rect.Bottom);
      u := FALSE;
    end else begin
      pCanvas.LineTo(Rect.Left+i, Rect.Top);
      u := TRUE;
    end;
    i := i - d;
  end;
  Change := FALSE;
end;

{ TGrFanItem }

procedure TGrFanItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  if GetValue then pCanvas.Brush.Color := HiColor else pCanvas.Brush.Color := LowColor;
  pCanvas.Polygon([ Point( Round(Rect.Left+Width*0.4), Rect.Top),
                    Point( Round(Rect.Left+Width*0.6), Rect.Bottom),
                    Point( Round(Rect.Left+Width*0.4), Rect.Bottom),
                    Point( Round(Rect.Left+Width*0.6), Rect.Top)]);
  pCanvas.Ellipse(Round(Rect.Left+Width*0.4), Round(Rect.Top+Height*0.4), Round(Rect.Left+Width*0.6), Round(Rect.Top+Height*0.6));
  pCanvas.Brush.Style := bsClear;
  pCanvas.Ellipse(Rect);
  Change := FALSE;
end;

{ TGrTankItem }

procedure TGrTankItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Brush.Color := HiColor; pCanvas.Pen.Color := LowColor;
  pCanvas.Brush.Style := bsBDiagonal;
  pCanvas.Rectangle(Rect.Left,Round(Rect.Top+Height*0.5), Rect.Right, Rect.Bottom);
  pCanvas.Brush.Style := bsClear;
  pCanvas.Rectangle(Rect);
  pCanvas.Brush.Style := bsSolid;
  Change := FALSE;
end;

{ TGrTank1Item }

procedure TGrTank1Item.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Brush.Color := HiColor; pCanvas.Pen.Color := LowColor;
  pCanvas.Brush.Style := bsClear;
  pCanvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Round(Rect.Bottom-Height*0.4));
  pCanvas.Brush.Style := bsBDiagonal;
  pCanvas.Polygon([ Point( Round(Rect.Left), Round(Rect.Bottom-Height*0.4)),
                    Point( Round(Rect.Left+Width*0.2), Rect.Bottom),
                    Point( Round(Rect.Right-Width*0.2), Rect.Bottom),
                    Point( Round(Rect.Right), Round(Rect.Bottom-Height*0.4))]);
  pCanvas.Brush.Style := bsSolid;
  Change := FALSE;
end;

{ TGrCoilItem }

procedure TGrCoilItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Pen.Color := HiColor;
  pCanvas.Polyline([Point( Rect.Left, Rect.Top),
                    Point( Round(Rect.Left+Width*0.9), Rect.Top),
                    Point( Round(Rect.Left+Width*0.1), Round(Rect.Top+Height*0.25)),
                    Point( Round(Rect.Left+Width*0.9), Round(Rect.Top+Height*0.5)),
                    Point( Round(Rect.Left+Width*0.1), Round(Rect.Top+Height*0.75)),
                    Point( Round(Rect.Left+Width*0.9), Rect.Bottom),
                    Point( Rect.Left, Rect.Bottom)]);
  pCanvas.Pen.Color := LowColor;
  pCanvas.Polyline([Point( Rect.Left, Rect.Top-1),
                    Point( Round(Rect.Left+Width*0.9), Rect.Top-1),
                    Point( Round(Rect.Left+Width*0.1), Round(Rect.Top+Height*0.25)-1),
                    Point( Round(Rect.Left+Width*0.9), Round(Rect.Top+Height*0.5)-1),
                    Point( Round(Rect.Left+Width*0.1), Round(Rect.Top+Height*0.75)-1),
                    Point( Round(Rect.Left+Width*0.9), Rect.Bottom-1),
                    Point( Rect.Left, Rect.Bottom-1)]);
  pCanvas.Polyline([Point( Rect.Left, Rect.Top+1),
                    Point( Round(Rect.Left+Width*0.9), Rect.Top+1),
                    Point( Round(Rect.Left+Width*0.1), Round(Rect.Top+Height*0.25)+1),
                    Point( Round(Rect.Left+Width*0.9), Round(Rect.Top+Height*0.5)+1),
                    Point( Round(Rect.Left+Width*0.1), Round(Rect.Top+Height*0.75)+1),
                    Point( Round(Rect.Left+Width*0.9), Rect.Bottom+1),
                    Point( Rect.Left, Rect.Bottom+1)]);
  Change := FALSE;
end;

{ TGrCoil1Item }

procedure TGrCoil1Item.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Pen.Color := HiColor;
  pCanvas.Polyline([Point( Rect.Left, Rect.Top),
                    Point( Rect.Left, Round(Rect.Bottom-Height*0.1)),
                    Point( Round(Rect.Left+Width*0.5), Rect.Bottom),
                    Point( Rect.Right, Round(Rect.Bottom-Height*0.1)),
                    Point( Rect.Right, Rect.Top)]);
  pCanvas.Pen.Color := LowColor;
  pCanvas.Polyline([Point( Rect.Left-1, Rect.Top),
                    Point( Rect.Left-1, Round(Rect.Bottom-Height*0.1)),
                    Point( Round(Rect.Left+Width*0.5), Rect.Bottom+1),
                    Point( Rect.Right+1, Round(Rect.Bottom-Height*0.1)),
                    Point( Rect.Right+1, Rect.Top)]);
  pCanvas.Polyline([Point( Rect.Left+1, Rect.Top),
                    Point( Rect.Left+1, Round(Rect.Bottom-Height*0.1)),
                    Point( Round(Rect.Left+Width*0.5), Rect.Bottom-1),
                    Point( Rect.Right-1, Round(Rect.Bottom-Height*0.1)),
                    Point( Rect.Right-1, Rect.Top)]);
  Change := FALSE;
end;

{ TGrCoolItem }

procedure TGrCoolItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Pen.Color := Color;
  pCanvas.Rectangle( Rect);
  pCanvas.Polyline([Point(Round(Rect.Left+Width*0.2), Round(Rect.Top+Width*0.2)),
                    Point(Round(Rect.Left+Width*0.2), Round(Rect.Bottom-Width*0.2))]);
  pCanvas.Polyline([Point(Round(Rect.Left+Width*0.4), Round(Rect.Top+Width*0.2)),
                    Point(Round(Rect.Left+Width*0.4), Round(Rect.Bottom-Width*0.2))]);
  pCanvas.Polyline([Point(Round(Rect.Left+Width*0.6), Round(Rect.Top+Width*0.2)),
                    Point(Round(Rect.Left+Width*0.6), Round(Rect.Bottom-Width*0.2))]);
  pCanvas.Polyline([Point(Round(Rect.Left+Width*0.8), Round(Rect.Top+Width*0.2)),
                    Point(Round(Rect.Left+Width*0.8), Round(Rect.Bottom-Width*0.2))]);

  pCanvas.Polyline([Point(Round(Rect.Left+Width*0.2), Rect.Top),
                    Point(Round(Rect.Left+Width*0.8), Rect.Bottom)]);
  pCanvas.Pen.Color := LowColor;
  pCanvas.Polyline([Point(Round(Rect.Left+Width*0.2)-1, Rect.Top),
                    Point(Round(Rect.Left+Width*0.8)-1, Rect.Bottom)]);
  pCanvas.Polyline([Point(Round(Rect.Left+Width*0.2)+1, Rect.Top),
                    Point(Round(Rect.Left+Width*0.8)+1, Rect.Bottom)]);
  Change := FALSE;
end;

{ TGrScrubberItem }

procedure TGrScrubberItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Brush.Color := HiColor; pCanvas.Pen.Color := LowColor;
  pCanvas.Brush.Style := bsDiagCross;
  pCanvas.Rectangle(Rect.Left,Round(Rect.Top+Height*0.1), Rect.Right, Rect.Bottom);
  pCanvas.Brush.Style := bsClear;
  pCanvas.Rectangle(Rect);
  Change := FALSE;
end;

{ TGrMillItem }

procedure TGrMillItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Brush.Color := HiColor; pCanvas.Pen.Color := LowColor;
  pCanvas.Rectangle( Rect.Left, Rect.Top, Round(Rect.Left+Width*0.1), Rect.Bottom);
  pCanvas.Rectangle( Round(Rect.Left+Width*0.9), Rect.Top, Rect.Right, Rect.Bottom);
  pCanvas.Brush.Style := bsClear;
  pCanvas.Rectangle(Rect);
  pCanvas.Brush.Style := bsDiagCross;
  pCanvas.Rectangle( Round(Rect.Left+Width*0.2),Round(Rect.Top+Height*0.1), Round(Rect.Left+Width*0.8), Round(Rect.Top+Height*0.9));
  pCanvas.Polyline([Point(Round(Rect.Left+Width*0.5), Rect.Bottom),
                    Point(Round(Rect.Left+Width*0.5), Rect.Bottom+Round(Height*0.3))]);
  Change := FALSE;
end;

{ TGrReactorItem }

procedure TGrReactorItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Brush.Color := HiColor; pCanvas.Pen.Color := LowColor;
  pCanvas.Rectangle( Rect.Left, Rect.Top, Round(Rect.Left+Width*0.1), Rect.Bottom);
  pCanvas.Rectangle( Round(Rect.Left+Width*0.9), Rect.Top, Rect.Right, Rect.Bottom);
  pCanvas.Rectangle( Round(Rect.Left-Width*0.2),Round(Rect.Top+Height*0.7), Rect.Left, Rect.Bottom);
  pCanvas.Rectangle( Rect.Right, Round(Rect.Top+Height*0.7), Round(Rect.Right+Width*0.2),Rect.Bottom );
  pCanvas.Brush.Style := bsClear;
  pCanvas.Rectangle(Rect);
  pCanvas.Brush.Style := bsDiagCross;
  pCanvas.Rectangle( Round(Rect.Left+Width*0.2),Round(Rect.Top+Height*0.1), Round(Rect.Left+Width*0.8), Round(Rect.Top+Height*0.9));
  Change := FALSE;
end;

{ TGrRotorItem }

procedure TGrRotorItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Brush.Color := HiColor; pCanvas.Pen.Color := LowColor;
  pCanvas.Rectangle(Round(Rect.Left+Width*0.49),Rect.Top,Round(Rect.Left+Width*0.51),Rect.Bottom);
  pCanvas.Rectangle(Rect.Left,Round(Rect.Bottom-Height*0.05), Rect.Right, Rect.Bottom);
  Change := FALSE;
end;

{ TGrBoxItem }

procedure TGrBoxItem.Paint(const pCanvas: TCanvas);
begin
  if not Change then Exit;
  inherited Paint( pCanvas);
  pCanvas.Brush.Style := bsClear;
  pCanvas.Pen.Color := HiColor;
  pCanvas.Rectangle(Rect);
  Change := FALSE;
end;

{ TGrBallonItem }

procedure TGrBallonItem.Paint(const pCanvas: TCanvas);
begin
  inherited;
  pCanvas.Brush.Style := bsClear;
//  pCanvas.Brush.Color := HiColor;
  pCanvas.Pen.Color := LowColor;
  pCanvas.RoundRect(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom,Round(Height/3),Round(Height/3));
end;

end.
