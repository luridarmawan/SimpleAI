# SimpleAI

Simple AI with Pascal


Data intent dan entitiest menggunakan file text biasa, tidak menggunakan RDBMS.
Disarankan untuk menggunakan Redis atau sejenisnya.

**Dependency**

- kesabaran dan ketekunan


### Instalasi

Gunakan Lazarus, buka file "simpleai_package.lpk" dan install file tersebut.


**SimpleAI USAGE**

```delphi
SimpleAI := TSimpleAI.Create;
SimpleAI.AddEntitiesFromFile( 'entities.txt');
SimpleAI.AddEntitiesFromFile( 'entities-pulsa.txt');
SimpleAI.AddEntitiesFromFile( 'entities-hotel.txt');
SimpleAI.AddIntentFromFile( 'intents.txt');
SimpleAI.AddIntentFromFile( 'intents-pulsa.txt');
SimpleAI.AddIntentFromFile( 'intents-hotel.txt');
SimpleAI.AddIntentFromFile( 'intents-danlainlain.txt');
SimpleAI.AddResponFromFile( 'response.txt');

.
.
.

Text := 'hi apa kabar?';

if SimpleAI.Exec(Text) then
begin

  // output dalam format text (result saja)
  ResponseString := SimpleAPI.SimpleAI.ResponseText;
  
  // output dalam json format
  json := SimpleAPI.SimpleAI.ResponseJson;

  //
  Action := SimpleAI.Action;
  IntentName := SimpleAI.IntentName;
  Params := SimpleAI.Parameters;
  
  // do something
  .
  .
  .

end;
```


***Format JSON Output***

```
{
	"code": 0,
	"request": {
		"text": ""
	},
	"response": {
		"intents": {
			"action": "",
			"name": "",
			"parameters": {}
		},
		"text": []
	}
}
```


# SimpleBOT

SimpleBOT merupakan salah satu contoh penggunaan SimpleAI yang dipergunakan untuk membuat BOT.
Memiliki fitur menjawab otomatis, dan belajar suatu definisi kata sederhana.
Kecerdasan Bot ini tergantung dari data entities dan intent yang Anda miliki, serta logic handler yang Anda buat.

Contoh penggunaan bot sederhana dengan SimpleBOT ini bisa anda coba dari situs [ai.fastplaz.com](http://ai.fastplaz.com) atau bisa melalu aplikasi chat **Telegram**, silahkan hubungi contact *'Fastplaz Bot'*.

**Dependency**

- FastPlaz_runtime
- SimpleAI package

### Instalasi

Gunakan Lazarus, buka file "simplebot_package.lpk" dan install file tersebut.
Jangan lupa, instalasi ini membutuhkan SimpleAI package.

**SimpleBOT USAGE**

```
  SimpleBOT := TSimpleBotModule.Create;
  SimpleBOT.OnError := @OnErrorHandler;  // Your Custom Message
  text_response := SimpleBOT.Exec(Text);
  SimpleBOT.Free;

```

Fungsi 'OnErrorHandler' bisa digunakan untuk melakukan trapping terhadap kata/kalimat yang belum diakomodir oleh data SimpleAI

```delphi
function TMainModule.OnErrorHandler(const Message: string): string;
begin
  .
  .
  .
  // save to log file
  LogUtil.Add(Message, 'AI');
  
  // or save to database
  .
  .
  Result := 'Your custom messages';
end;
```





![Format](img/format_01.png "Format")
