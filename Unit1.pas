unit Unit1;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls, WEBLib.REST,
  Vcl.Controls, WEBLib.JSON, WEBLib.ExtCtrls,TMSWebCore;

type
  TForm1 = class(TWebForm)
    WebButton1: TWebButton;
    WebHttpRequest1: TWebHttpRequest;
    WebMemo1: TWebMemo;
    WebMultimediaPlayer1: TWebMultimediaPlayer;
    WebButton2: TWebButton;
    procedure WebButton1Click(Sender: TObject);
    procedure HandleVoiceInput(const Transcript: string);
    //procedure ExecuteJavaScript(const script: string);
    //procedure PlayAudioStream(Sender: TObject; AResponse: TJSXMLHttpRequest);
    //procedure WebHttpRequest1Response(Sender: TObject; AResponse: string);
  //  procedure WebMultimediaPlayer1Ended(Sender: TObject);

    procedure WebButton2Click(Sender: TObject);
    procedure WebHttpRequest1RequestResponse(Sender: TObject;
      ARequest: TJSXMLHttpRequestRecord; AResponse: string);

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
  HandleVoiceInput('Hello, world! This is some test audio I need you to say peeps. Thankyou.');


end;









procedure TForm1.WebButton2Click(Sender: TObject);
begin

  WebMultimediaPlayer1.URL := 'audio.mp3';
  WebMultimediaPlayer1.Play;
end;

procedure TForm1.WebHttpRequest1RequestResponse(Sender: TObject;
  ARequest: TJSXMLHttpRequestRecord; AResponse: string);
var
  BlobURL: string;
begin
  asm
    var blob = ARequest.req.response;  // Access the response as a Blob
    if (blob) {
      var url = window.URL.createObjectURL(blob);  // Create a URL for the Blob
      BlobURL = url;  // Pass the Blob URL back to Pascal
    }
  end;

  // Now use the Blob URL in Pascal to set the multimedia player URL and play the audio
  if BlobURL <> '' then
  begin
    WebMultimediaPlayer1.URL := BlobURL;  // Set the Blob URL to the multimedia player
    WebMultimediaPlayer1.Play;  // Play the audio
  end
  else
  begin
    ShowMessage('Failed to create audio URL from Blob.');
  end;
end;







procedure TForm1.HandleVoiceInput(const Transcript: string);
var
  JSONObj: TJSONObject;
begin
  WebHttpRequest1.URL := 'https://testchat3.onrender.com/stream';  // Use your server URL
  WebHttpRequest1.Command := httpPOST;
  WebHttpRequest1.Headers.Values['Content-Type'] := 'application/json';
  WebHttpRequest1.ResponseType := rtBlob;  // Set response type to Blob to handle binary data

  // Prepare JSON data for the POST request
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('text', Transcript);
    WebHttpRequest1.PostData := JSONObj.ToJSON;  // Set the POST data
  finally
    JSONObj.Free;
  end;

  // Assign the response handling event
 // WebHttpRequest1.OnResponse := WebHttpRequest1RequestResponse;

  // Execute the request
  WebHttpRequest1.Execute;
end;






















//procedure TForm1.PlayAudioStream(Sender: TObject; AResponse: TJSXMLHttpRequest);
//var
//  audioUrl: string;
//begin
//  asm
//    var audioBlob = AResponse.response;  // Access the response as a Blob
//    audioUrl = URL.createObjectURL(audioBlob);  // Create a URL for the blob
//    var audio = new Audio(audioUrl);  // Create an audio object
//    audio.play();  // Play the audio
//  end;
//end;

//procedure TForm1.ExecuteJavaScript(const script: string);
//begin
//  asm
//    eval(script);
//  end;
//end;


end.

