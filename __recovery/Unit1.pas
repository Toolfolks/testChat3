unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.REST,
  Vcl.Controls, WEBLib.JSON;

type
  TForm1 = class(TWebForm)
    WebButton1: TWebButton;
    WebHttpRequest1: TWebHttpRequest;
    WebMemo1: TWebMemo;
    procedure WebButton1Click(Sender: TObject);
    procedure HandleVoiceInput(const Transcript: string);
    procedure ExecuteJavaScript(const script: string);
    procedure PlayAudioStream(Sender: TObject; AResponse: TJSXMLHttpRequest);
    procedure WebHttpRequest1Error(Sender: TObject;
      ARequest: TJSXMLHttpRequestRecord; Event: TJSEventRecord;
      var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.WebButton1Click(Sender: TObject);
begin
  HandleVoiceInput('hello');
end;

procedure TForm1.WebHttpRequest1Error(Sender: TObject;
  ARequest: TJSXMLHttpRequestRecord; Event: TJSEventRecord;
  var Handled: Boolean);
begin
  ShowMessage('HTTP request failed. Status: ' + ARequest.req.Status.ToString + ' ' + ARequest.req.StatusText);
  Handled := True;
end;

procedure TForm1.HandleVoiceInput(const Transcript: string);
var
  JSONObj: TJSONObject;
begin
  WebHttpRequest1.URL := 'https://testchat3.onrender.com/stream';
  WebHttpRequest1.Command := httpPOST;
  WebHttpRequest1.Headers.Values['Content-Type'] := 'application/json';
  WebHttpRequest1.ResponseType := rtBlob;  // Set response type to blob

  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('text', Transcript);
    WebHttpRequest1.PostData := JSONObj.ToJSON;
  finally
    JSONObj.Free;
  end;

  //WebHttpRequest1.OnResponse := PlayAudioStream;
  WebHttpRequest1.OnError := WebHttpRequest1Error;
  WebHttpRequest1.Execute;
end;

procedure TForm1.ExecuteJavaScript(const script: string);
begin
  asm
    eval(script);
  end;
end;

procedure TForm1.PlayAudioStream(Sender: TObject; AResponse: TJSXMLHttpRequest);
var
  audioUrl: string;
begin
  asm
    var audioBlob = AResponse.response;
    audioUrl = URL.createObjectURL(audioBlob);
    var audio = new Audio(audioUrl);
    audio.play();
  end;
end;

end.

