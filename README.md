# SimpleAI

Simple AI with Pascal


Data intent dan entitiest menggunakan file text biasa, tidak menggunakan RDBMS.
Disarankan untuk menggunakan Redis atau sejenisnya.

***Dependency***

- kesabaran dan ketekunan


### Instalasi

Gunakan Lazarus, buka file "simpleai_package.lpk" dan install file tersebut.


***SimpleAI USAGE***

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


***Format Output***

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

***Dependency***

- FastPlaz_runtime
- SimpleAI package

### Instalasi

Gunakan Lazarus, buka file "simplebot_package.lpk" dan install file tersebut.
Jangan lupa, instalasi ini membutuhkan SimpleAI package.


***Input***

method: Post

data disematkan di dalam body post, dengan format berikut

```
{"message":{"message_id":0,"text":"Your Message","chat":{"id":0}}}
```





![Format](img/format_01.png "Format")
