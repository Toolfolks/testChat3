object Form1: TForm1
  Width = 640
  Height = 480
  CSSLibrary = cssBootstrap
  ElementFont = efCSS
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  object WebButton1: TWebButton
    Left = 32
    Top = 113
    Width = 96
    Height = 25
    Caption = 'WebButton1'
    ElementClassName = 'btn btn-light'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = WebButton1Click
  end
  object WebMemo1: TWebMemo
    Left = 296
    Top = 40
    Width = 281
    Height = 169
    ElementClassName = 'form-control'
    ElementFont = efCSS
    HeightPercent = 100.000000000000000000
    Lines.Strings = (
      'WebMemo1')
    SelLength = 0
    SelStart = 0
    WidthPercent = 100.000000000000000000
  end
  object WebMultimediaPlayer1: TWebMultimediaPlayer
    Left = 120
    Top = 280
    Width = 100
    Height = 41
    AutoPlay = True
    MultimediaType = mtAudio
    PlaybackRate = 1.000000000000000000
  end
  object WebButton2: TWebButton
    Left = 72
    Top = 192
    Width = 96
    Height = 25
    Caption = 'WebButton2'
    ChildOrder = 3
    ElementClassName = 'btn btn-light'
    ElementFont = efCSS
    HeightStyle = ssAuto
    HeightPercent = 100.000000000000000000
    WidthPercent = 100.000000000000000000
    OnClick = WebButton2Click
  end
  object WebHttpRequest1: TWebHttpRequest
    OnRequestResponse = WebHttpRequest1RequestResponse
    Left = 304
    Top = 280
  end
end
