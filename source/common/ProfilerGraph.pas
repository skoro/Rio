unit ProfilerGraph;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TAGraph, TASources, TAMultiSeries, Controls;

const
  // Profile's check point names.
  PROFILER_CONNECT = 'connect';
  PROFILER_REQUEST = 'request';
  PROFILER_HEADERS = 'headers';
  PROFILER_BODY = 'body';
  PROFILER_TOTAL = 'total';

type

  TTimeMSec = int64;

  { TTimeCheckPoint }

  TTimeCheckPoint = class(TCollectionItem)
  private
    FStart: TDateTime;
    FFinish: TDateTime;
    FName: string;
    function GetDuration: TTimeMSec;
  public
    procedure Reset;
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write FName;
    property Start: TDateTime read FStart write FStart;
    property Finish: TDateTime read FFinish write FFinish;
    property Duration: TTimeMSec read GetDuration;
  end;

  { TTimeCheckPointEnumerator }

  TTimeCheckPointEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TTimeCheckPoint;
    property Current: TTimeCheckPoint read GetCurrent;
  end;

  { TTimeCheckPointList }

  TTimeCheckPointList = class(TCollection)
  private
    function GetTimeCheckPoint(const Name: string): TTimeCheckPoint;
  public
    constructor Create;
    function Add: TTimeCheckPoint;
    function Add(const Name: string): TTimeCheckPoint; overload;
    function GetEnumerator: TTimeCheckPointEnumerator;
    property CheckPoint[const Name: string]: TTimeCheckPoint read GetTimeCheckPoint;
      default;
  end;

  { TTimeProfiler }

  TTimeProfiler = class
  private
  protected
    FCheckPoints: TTimeCheckPointList;
  public
    constructor Create;
    destructor Destory; virtual;
    procedure Reset;
    procedure Start(const AName: string);
    procedure Stop(const AName: string);
    property CheckPoints: TTimeCheckPointList read FCheckPoints;
  end;

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
    procedure CreateChart(AParent: TWinControl); virtual;
    procedure SetProfilerData; virtual;
  public
    constructor Create(AParent: TWinControl);
    destructor Destroy; override;
    procedure Clear; virtual;
    property TimeCheckPoints: TTimeCheckPointList
      read FTimeCheckPoints write SetTimeCheckPoints;
    property Chart: TChart read FChart;
  end;

implementation

uses TAChartUtils, TAChartAxisUtils, dateutils;

{ TTimeCheckPointEnumerator }

function TTimeCheckPointEnumerator.GetCurrent: TTimeCheckPoint;
begin
  Result := inherited GetCurrent as TTimeCheckPoint;
end;

{ TTimeCheckPoint }

function TTimeCheckPoint.GetDuration: TTimeMSec;
begin
  Result := MilliSecondsBetween(FFinish, FStart);
end;

procedure TTimeCheckPoint.Reset;
begin
  FStart := 0;
  FFinish := 0;
end;

procedure TTimeCheckPoint.Assign(Source: TPersistent);
var
  TCP: TTimeCheckPoint;
begin
  TCP := TTimeCheckPoint(Source);
  FStart := TCP.Start;
  FFinish := TCP.Finish;
  FName := TCP.Name;
end;

{ TTimeProfiler }

constructor TTimeProfiler.Create;
begin
  inherited;
  FCheckPoints := TTimeCheckPointList.Create;
end;

destructor TTimeProfiler.Destory;
begin
  FCheckPoints.Destroy;
  inherited;
end;

procedure TTimeProfiler.Reset;
begin
  FCheckPoints.Clear;
end;

procedure TTimeProfiler.Start(const AName: string);
begin
  with FCheckPoints.Add do
  begin
    Name := AName;
    Start := Now;
  end;
end;

procedure TTimeProfiler.Stop(const AName: string);
var
  TCP: TTimeCheckPoint;
  TmStop: TDateTime;
begin
  TmStop := Now;
  TCP := FCheckPoints[AName];
  if not Assigned(TCP) then
    Exit; // =>
  TCP.Finish := TmStop;
end;

{ TTimeCheckPointList }

function TTimeCheckPointList.GetTimeCheckPoint(const Name: string): TTimeCheckPoint;
var
  iter: TTimeCheckPoint;
begin
  for iter in Self do
    if iter.Name = Name then
      Exit(iter); // =>
  raise Exception.CreateFmt('Check point "%s" not found.', [Name]);
end;

constructor TTimeCheckPointList.Create;
begin
  inherited Create(TTimeCheckPoint);
end;

function TTimeCheckPointList.Add: TTimeCheckPoint;
begin
  Result := inherited Add as TTimeCheckPoint;
end;

function TTimeCheckPointList.Add(const Name: string): TTimeCheckPoint;
begin
  Result := inherited Add as TTimeCheckPoint;
  Result.Name := Name;
end;

function TTimeCheckPointList.GetEnumerator: TTimeCheckPointEnumerator;
begin
  Result := TTimeCheckPointEnumerator.Create(Self);
end;

{ TProfilerGraph }

procedure TProfilerGraph.SetTimeCheckPoints(AValue: TTimeCheckPointList);
var
  CP: TTimeCheckPoint;
begin
  if FTimeCheckPoints = AValue then
    Exit;
  FTimeCheckPoints.Clear;
  for CP in AValue do
    with FTimeCheckPoints.Add do
    begin
      Name   := CP.Name;
      Start  := CP.Start;
      Finish := CP.Finish;
    end;
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
  with FChart.AxisList.Add do
  begin
    Alignment := calRight;
    Marks.Source := FListChartSourceVALUES;
    Marks.Style := smsLabel;
  end;
end;

procedure TProfilerGraph.SetProfilerData;
var
  cp, total, hdr: TTimeCheckPoint;
  s, f: comp;

  function GetOffset(t: TTimeCheckPoint): comp;
  begin
    Result := TimeStampToMSecs(DateTimeToTimeStamp(t.Start)) - s;
  end;

begin
  Clear;
  if FTimeCheckPoints.Count = 0 then
    Exit;

  s := 0;
  total := FTimeCheckPoints[PROFILER_TOTAL];
  s := GetOffset(total);

  cp := FTimeCheckPoints[PROFILER_CONNECT];
  f := GetOffset(cp);
  FListChartSource.AddXYList(0, [0, f, 0, f + cp.Duration, total.Duration]);
  FListChartSourceLABELS.Add(0, 0, 'Connect');
  FListChartSourceVALUES.Add(0, 0, Format('%d ms', [cp.Duration]));

  cp := FTimeCheckPoints[PROFILER_REQUEST];
  f := GetOffset(cp);
  FListChartSource.AddXYList(1, [0, f, 0, f + cp.Duration, total.Duration]);
  FListChartSourceLABELS.Add(0, 1, 'Send request');
  FListChartSourceVALUES.Add(0, 1, Format('%d ms', [cp.Duration]));

  hdr := FTimeCheckPoints[PROFILER_HEADERS];
  f := GetOffset(hdr);
  FListChartSource.AddXYList(2, [0, f, 0, f + hdr.Duration, total.Duration]);
  FListChartSourceLABELS.Add(0, 2, 'Headers');
  FListChartSourceVALUES.Add(0, 2, Format('%d ms', [hdr.Duration]));

  // Response is a response body + the response headers.
  cp := FTimeCheckPoints[PROFILER_BODY];
  FListChartSource.AddXYList(3, [0, f + hdr.Duration, 0, f + hdr.Duration +
    (cp.Duration - hdr.Duration), total.Duration]);
  FListChartSourceLABELS.Add(0, 3, 'Response');
  FListChartSourceVALUES.Add(0, 3, Format('%d ms', [cp.Duration - hdr.Duration]));
end;

procedure TProfilerGraph.Clear;
begin
  FListChartSource.Clear;
  FListChartSourceLABELS.Clear;
  FListChartSourceVALUES.Clear;
end;

constructor TProfilerGraph.Create(AParent: TWinControl);
begin
  inherited Create;
  FTimeCheckPoints := TTimeCheckPointList.Create;
  CreateChart(AParent);
end;

destructor TProfilerGraph.Destroy;
begin
  FreeAndNil(FChart);
  FreeAndNil(FTimeCheckPoints);
  inherited;
end;

end.
