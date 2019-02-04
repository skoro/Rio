unit profiler_graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TAGraph, TASources, TAMultiSeries, Controls,
  thread_http_client;

type

  { TProfilerGraph }

  TProfilerGraph = class
  private
    FChart: TChart;
    FListChartSource: TListChartSource;
    FListChartSourceLABELS: TListChartSource;
    FListChartSourceVALUES: TListChartSource;
    FTimeSeries: TBoxAndWhiskerSeries;
    FTimeCheckPoints: TTimeCheckPointList;
    procedure SetTimeCheckPoints(AValue: TTimeCheckPointList);
  protected
    procedure CreateChart(AParent: TWinControl);
    procedure SetProfilerData;
  public
    constructor Create(AParent: TWinControl);
    destructor Destroy; override;
    property TimeCheckPoints: TTimeCheckPointList
      read FTimeCheckPoints write SetTimeCheckPoints;
    property Chart: TChart read FChart;
  end;

implementation

uses dateutils, TAChartUtils, TAChartAxisUtils;


{ TProfilerGraph }

procedure TProfilerGraph.SetTimeCheckPoints(AValue: TTimeCheckPointList);
begin
  if FTimeCheckPoints = AValue then
    Exit;
  FTimeCheckPoints := AValue;
  SetProfilerData;
end;

procedure TProfilerGraph.CreateChart(AParent: TWinControl);
begin
  FChart := TChart.Create(AParent);
  FChart.Parent := AParent;
  FChart.Align := alClient;
  FChart.Frame.Visible := False;
  FChart.AllowZoom := False;
  FListChartSource := TListChartSource.Create(FChart);
  FListChartSource.YCount := 5;
  FListChartSourceLABELS := TListChartSource.Create(FChart);
  FListChartSourceVALUES := TListChartSource.Create(FChart);
  FTimeSeries := TBoxAndWhiskerSeries.Create(FChart);
  FTimeSeries.Source := FListChartSource;
  FTimeSeries.AxisIndexX := 0;
  FTimeSeries.AxisIndexY := 1;
  FChart.AddSeries(FTimeSeries);
  FChart.LeftAxis.Marks.Source := FListChartSourceLABELS;
  FChart.LeftAxis.Marks.Style := smsLabel;
  with FChart.AxisList.Add do begin
    Alignment := calRight;
    Marks.Source := FListChartSourceVALUES;
    Marks.Style := smsLabel;
  end;
end;

procedure TProfilerGraph.SetProfilerData;
var
  cp, total, hdr: TTimeCheckPoint;
  s, f: comp;
  function GetOffset(t: TTimeCheckPoint): Comp;
  begin
    Result := TimeStampToMSecs(DateTimeToTimeStamp(t.Start)) - s;
  end;

begin
  FListChartSource.Clear;
  FListChartSourceLABELS.Clear;
  FListChartSourceVALUES.Clear;
  if FTimeCheckPoints.Count = 0 then
    Exit;

  s := 0;
  total := FTimeCheckPoints.KeyData['Total'];
  s := GetOffset(total);

  cp := FTimeCheckPoints.KeyData['Connect'];
  f := GetOffset(cp);
  FListChartSource.AddXYList(0, [0, f, 0, f + cp.Duration, total.Duration]);
  FListChartSourceLABELS.Add(0, 0, 'Connect');
  FListChartSourceVALUES.Add(0, 0, Format('%d ms', [cp.Duration]));

  cp := FTimeCheckPoints.KeyData['Send request'];
  f := GetOffset(cp);
  FListChartSource.AddXYList(1, [0, f, 0, f + cp.Duration, total.Duration]);
  FListChartSourceLABELS.Add(0, 1, 'Send request');
  FListChartSourceVALUES.Add(0, 1, Format('%d ms', [cp.Duration]));

  hdr := FTimeCheckPoints.KeyData['Response headers'];
  f := GetOffset(hdr);
  FListChartSource.AddXYList(2, [0, f, 0, f + hdr.Duration, total.Duration]);
  FListChartSourceLABELS.Add(0, 2, 'Headers');
  FListChartSourceVALUES.Add(0, 2, Format('%d ms', [hdr.Duration]));

  // Response is a response body + the response headers.
  cp := FTimeCheckPoints.KeyData['Read response'];
  FListChartSource.AddXYList(3, [0, f + hdr.Duration, 0, f + hdr.Duration + (cp.Duration - hdr.Duration), total.Duration]);
  FListChartSourceLABELS.Add(0, 3, 'Response');
  FListChartSourceVALUES.Add(0, 3, Format('%d ms', [cp.Duration - hdr.Duration]));
end;

constructor TProfilerGraph.Create(AParent: TWinControl);
begin
  inherited Create;
  CreateChart(AParent);
end;

destructor TProfilerGraph.Destroy;
begin
  FreeAndNil(FChart);
  inherited;
end;

end.

