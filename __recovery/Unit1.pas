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
    procedure WebHttpRequest1Response2(Sender: TObject; AResponse: TJSXMLHttpRequest);
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
  HandleVoiceInput('Hello, world! This is some test audio I need you to say peeps. Thankyou.');
end;

procedure TForm1.WebHttpRequest1Error(Sender: TObject;
  ARequest: TJSXMLHttpRequestRecord; Event: TJSEventRecord;
  var Handled: Boolean);
begin
  ShowMessage('HTTP request failed. Status: ' + ARequest.req.Status.ToString + ' ' + ARequest.req.StatusText);
  Handled := True;
end;







procedure TForm1.WebHttpRequest1Response2(Sender: TObject; AResponse: TJSXMLHttpRequest);
var
  audioUrl: string;
begin
  asm
    var audioBlob = AResponse.response;  // Access the Blob response directly
    if (audioBlob) {
      var audioUrl = URL.createObjectURL(audioBlob);  // Create URL for the Blob
      var audio = new Audio(audioUrl);  // Create an audio object with the Blob URL
      audio.play().catch(function(error) {
        console.error('Error playing audio:', error);  // Log any errors if playback fails
      });
    } else {
      console.error('No audio blob found in response');  // This should not happen if the response type is correctly set to blob
    }
  end;
end;




procedure TForm1.HandleVoiceInput(const Transcript: string);
var
  JSONObj: TJSONObject;
begin
  WebHttpRequest1.URL := 'https://testchat3.onrender.com/stream';  // Use your server URL
  WebHttpRequest1.Command := httpPOST;
  WebHttpRequest1.Headers.Values['Content-Type'] := 'application/json';
  WebHttpRequest1.ResponseType := rtText;  // Set response type to text

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

procedure TForm1.WebHttpRequest1Response(Sender: TObject; AResponse: string);
begin
  asm
    try {
      if (AResponse) {
        // Convert binary string to a Uint8Array
        var binaryString = atob(AResponse);  // Decode the base64 encoded string
        var len = binaryString.length;
        var bytes = new Uint8Array(len);
        for (var i = 0; i < len; i++) {
          bytes[i] = binaryString.charCodeAt(i);
        }

        // Create a Blob from the binary data
        var blob = new Blob([bytes.buffer], {type: 'audio/mpeg'});  // Correct MIME type for MP3
        var url = window.URL.createObjectURL(blob);  // Create URL for the Blob
        var a = document.createElement('a');  // Create a link element
        a.href = url;  // Set the href attribute to the Blob URL
        a.download = 'audio.mp3';  // Set the download attribute to specify the filename
        document.body.appendChild(a);  // Append the link to the body
        a.click();  // Trigger a click to start the download
        document.body.removeChild(a);  // Remove the link from the document
        window.URL.revokeObjectURL(url);  // Revoke the Blob URL to free up memory
      } else {
        console.error('No audio blob found in response');
      }
    } catch (e) {
      console.error('Error processing audio:', e);  // Log any errors
    }
  end;
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

