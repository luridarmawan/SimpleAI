# SimpleAI

Simple AI with Pascal


Data intent dan entitiest menggunakan file text biasa, tidak menggunakan RDBMS.
Disarankan untuk menggunakan Redis atau sejenisnya.

***Dependency***

- FastPlaz_runtime
- SimpleAI_package

### Instalasi

....


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
  str := SimpleAPI.SimpleAI.ResponseText;
  
  // output dalam json format
  json := SimpleAPI.SimpleAI.ResponseJson;

  //
  Action := SimpleAI.Action;
  IntentName := SimpleAI.IntentName;
  Params := SimpleAI.Parameters;

end;
```


***Input***

method: Post

```
{"message":{"message_id":0,"text":"Your Message","chat":{"id":0}}}
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
			"pattern": "",
			"parameters": {}
		},
		"text": []
	}
}
```


# SimpleBOT

***Dependency***

- FastPlaz_runtime
- SimpleBot_package






![Format](img/format_01.png "Format")
