unit ClockForm;

interface

uses
  Vcl.Forms, Vcl.Controls, Vcl.ExtCtrls, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.StdCtrls, System.Skia, Vcl.Skia, System.Types, Math, System.UITypes;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    SkPaintBox: TSkPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SkPaintBoxDraw(Sender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
  private
    procedure DrawClockFace(const ACanvas: ISkCanvas; const ADest: TRectF);
    procedure DrawClockHands(const ACanvas: ISkCanvas; const ADest: TRectF);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1.Interval := 1000; // 1 second interval
  Timer1.Enabled := True;
//  DoubleBuffered := True; // Reduce flickering
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  SkPaintBox.DrawCacheKind := TSkDrawCacheKind.Always;
  SkPaintBox.Invalidate;
  SkPaintBox.Redraw; // Redraw the Skia paintbox
end;

procedure TForm1.SkPaintBoxDraw(Sender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  DrawClockFace(ACanvas, ADest);
  DrawClockHands(ACanvas, ADest);
end;

procedure TForm1.DrawClockFace(const ACanvas: ISkCanvas; const ADest: TRectF);
var
  i: Integer;
  Angle: Double;
  X, Y: Single;
  Radius: Single;
  NumberX, NumberY: Single;
  NumberString: String;
  Paint: ISkPaint;
  Font: ISkFont;
  TextBounds: TRectF;
begin
  Radius := Min(ADest.Width, ADest.Height) / 2 - 20;

  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.Color := TAlphaColors.Black;
  Paint.StrokeWidth := 3;

  ACanvas.DrawCircle(ADest.CenterPoint.X, ADest.CenterPoint.Y, Radius, Paint);

  Paint.StrokeWidth := 1;
  for i := 0 to 59 do
  begin
    Angle := i * Pi / 30;
    X := ADest.CenterPoint.X + Cos(Angle) * Radius;
    Y := ADest.CenterPoint.Y - Sin(Angle) * Radius;
    if i mod 5 = 0 then
    begin
      Paint.StrokeWidth := 3;
      ACanvas.DrawLine(TPointF.Create(X, Y), TPointF.Create(ADest.CenterPoint.X + Cos(Angle) * (Radius - 10), ADest.CenterPoint.Y - Sin(Angle) * (Radius - 10)), Paint);
    end
    else
    begin
      Paint.StrokeWidth := 1;
      ACanvas.DrawLine(TPointF.Create(X, Y), TPointF.Create(ADest.CenterPoint.X + Cos(Angle) * (Radius - 5), ADest.CenterPoint.Y - Sin(Angle) * (Radius - 5)), Paint);
    end;
  end;

  Paint.Style := TSkPaintStyle.Fill;
  Font := TSkFont.Create(TSkTypeface.MakeDefault, 24);
  for i := 1 to 12 do
  begin
    Angle := (i mod 12) * (Pi / 6);
    NumberString := IntToStr(i);
    Font.MeasureText(NumberString, TextBounds, Paint);
    NumberX := ADest.CenterPoint.X + Cos(Angle - Pi / 2) * (Radius - 30) - (TextBounds.Width / 2);
    NumberY := ADest.CenterPoint.Y + Sin(Angle - Pi / 2) * (Radius - 30) + (TextBounds.Height / 2);
    ACanvas.DrawSimpleText(NumberString, NumberX, NumberY, Font, Paint);
  end;
end;

procedure TForm1.DrawClockHands(const ACanvas: ISkCanvas; const ADest: TRectF);
var
  Hour, Minute, Sec, MSec: Word;
  Angle: Double;
  X, Y: Single;
  Radius: Single;
  Paint: ISkPaint;
begin
  DecodeTime(Now, Hour, Minute, Sec, MSec);
  Radius := Min(ADest.Width, ADest.Height) / 2 - 20;

  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeCap := TSkStrokeCap.Round;

  // Draw hour hand
  Angle := ((Hour mod 12) + Minute / 60) * (Pi / 6);
  X := ADest.CenterPoint.X + Cos(Angle - Pi / 2) * (Radius * 0.5);
  Y := ADest.CenterPoint.Y + Sin(Angle - Pi / 2) * (Radius * 0.5);
  Paint.StrokeWidth := 6;
  ACanvas.DrawLine(TPointF.Create(ADest.CenterPoint.X, ADest.CenterPoint.Y), TPointF.Create(X, Y), Paint);

  // Draw minute hand
  Angle := (Minute + Sec / 60) * (Pi / 30);
  X := ADest.CenterPoint.X + Cos(Angle - Pi / 2) * (Radius * 0.8);
  Y := ADest.CenterPoint.Y + Sin(Angle - Pi / 2) * (Radius * 0.8);
  Paint.StrokeWidth := 4;
  ACanvas.DrawLine(TPointF.Create(ADest.CenterPoint.X, ADest.CenterPoint.Y), TPointF.Create(X, Y), Paint);

  // Draw second hand
  Angle := Sec * (Pi / 30);
  X := ADest.CenterPoint.X + Cos(Angle - Pi / 2) * (Radius * 0.9);
  Y := ADest.CenterPoint.Y + Sin(Angle - Pi / 2) * (Radius * 0.9);
  Paint.Color := TAlphaColors.Red;
  Paint.StrokeWidth := 2;
  ACanvas.DrawLine(TPointF.Create(ADest.CenterPoint.X, ADest.CenterPoint.Y), TPointF.Create(X, Y), Paint);
end;

end.
