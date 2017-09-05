# SimpleAI

## What is it?

SimpleAI adalah _package code_ yang bisa digunakan untuk menerapkan AI (_Artificial Intelligence_) secara sederhana, khususnya untuk implementasi NLP (_Natural Language Processing_).

SimpleAI ini dibangun dalam bahasa Pascal, khususnya dengan [freepascal](http://www.freepascal.org/). Untuk memudahkan dalam proses development, telah disediakan juga _package_ untuk digunakan di dalam IDE [Lazarus](http://www.lazarus-ide.org/)

SimpleAI dibangun secara sederhana, data diambil dari *file* biasa, tanpa menggunakan RDBMS yang sudah ada.

Untuk meningkatkan performat, telah disediakan juga fitur untuk koneksi ke [Redis](https://redis.io/). Untuk Production atau penggunaan trafik yang tinggi, disarankan untuk menggunakan Redis sebagai pilihan _data storage_-nya.


## Why use it?

**Ringan Tanpa Beban**

SimpleAI dibuat dengan sederhana, simple dan ringan. SimpleAI adalah _binary application_ sehingga diharapkan akan lebih cepat dan ringan. 

## How to use it


### Requirements

- Kesabaran dan Ketekunan
- [Free Pascal](http://www.freepascal.org/)

### Instalation

Jika menggunakan Lazarus, buka file "simpleai_package.lpk" dan install file tersebut.


### SimpleAI USAGE

```delphi
SimpleAI := TSimpleAI.Create;
SimpleAI.AddEntitiesFromFile( 'entities-pulsa.txt');
SimpleAI.AddEntitiesFromFile( 'entities-hotel.txt');
SimpleAI.AddEntitiesFromFile( 'entities.txt');
SimpleAI.AddIntentFromFile( 'intents-pulsa.txt');
SimpleAI.AddIntentFromFile( 'intents-hotel.txt');
SimpleAI.AddIntentFromFile( 'intents-danlainlain.txt');
SimpleAI.AddIntentFromFile( 'intents.txt');
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


### Format JSON Output

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


## Documentation

Take a look at the repo [Wiki](https://github.com/luridarmawan/SimpleAI/wiki) for further information and tutorials!
Feel free to improve!

## Projects with this library

Here's a list of projects that feats this library, feel free to add yours!

- [SimpleBOT example](https://github.com/luridarmawan/SimpleBOT/) 



## Troubleshooting

If you like living on the edge, please report any bugs you find on the
[SimpleAI issues](https://github.com/luridarmawan/SimpleAI/issues) page.

## Contributing

See [CONTRIBUTING](CONTRIBUTING.md) for more information.

## License

Please see the [LICENSE](LICENSE.txt) included in this repository,
which this project is licensed under.

## Credits

Credit list in [CREDITS](CREDITS)

[Carik Bot](http://www.carik.id/)
