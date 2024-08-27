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
    procedure WebHttpRequest1Response(Sender: TObject; AResponse: string);

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



procedure TForm1.WebHttpRequest1Response(Sender: TObject; AResponse: string);
begin
  asm
    // Directly create a Blob from the response since it is already in binary format
    var audioBlob = AResponse.response;  // Correctly handle the Blob response
    if (audioBlob) {
      var audioUrl = URL.createObjectURL(audioBlob);  // Create URL for the Blob
      var audio = new Audio(audioUrl);  // Create an audio object with the Blob URL
      audio.play().catch(function(error) {
        console.error('Error playing audio:', error);  // Catch and log any errors
      });
    } else {
      console.error('No audio blob found in response');
    }
  end;
end;






//procedure TForm1.WebHttpRequest1Response(Sender: TObject; AResponse: TJSXMLHttpRequest);
//begin
//  showmessage('response');
//  PlayAudioStream(Sender, AResponse); // Corrected: Pass the TJSXMLHttpRequest object
//end;

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

  WebHttpRequest1.OnResponse := WebHttpRequest1Response;  // Correctly set the OnResponse handler
  WebHttpRequest1.OnError := WebHttpRequest1Error;
  WebHttpRequest1.Execute;
end;

procedure TForm1.PlayAudioStream(Sender: TObject; AResponse: TJSXMLHttpRequest);
var
  audioUrl: string;
begin
  asm
    var audioBlob = AResponse.response;  // Access the response as a Blob
    audioUrl = URL.createObjectURL(audioBlob);  // Create a URL for the blob
    var audio = new Audio(audioUrl);  // Create an audio object
    audio.play();  // Play the audio
  end;
end;

procedure TForm1.ExecuteJavaScript(const script: string);
begin
  asm
    eval(script);
  end;
end;

end.

