
## What is it?

SimpleAI adalah _package code_ yang bisa digunakan untuk menerapkan AI (_Artificial Intelligence_) secara sederhana, khususnya untuk implementasi NLP (_Natural Language Processing_).

SimpleAI ini dibangun dalam bahasa Pascal, khususnya dengan [freepascal](http://www.freepascal.org/). Untuk memudahkan dalam proses development, telah disediakan juga _package_ untuk digunakan di dalam IDE [Lazarus](http://www.lazarus-ide.org/)

SimpleAI dibangun secara sederhana, data diambil dari *file* biasa, tanpa menggunakan RDBMS yang sudah ada.

Untuk meningkatkan performat, telah disediakan juga fitur untuk koneksi ke [Redis](https://redis.io/). Untuk Production atau penggunaan trafik yang tinggi, disarankan untuk menggunakan Redis sebagai pilihan _data storage_-nya.


## Why use it?

**Ringan Tanpa Beban**

SimpleAI dibuat dengan sederhana, simple dan ringan. SimpleAI adalah _binary application_ sehingga diharapkan akan lebih cepat dan ringan. 


## How to use it

SimpleAI terbagi dalam 2 (dua) _package_ utama :

1. SimpleAI Package
2. SimpleBOT Package

### SimpleAI Package

Merupakan **package utama** dari SimpleAI ini. Berisi pustaka _entities_ dan _intents_ yang digunakan untuk kebutuhan AI secara sederhana.

Package ini bisa digunakan baik untuk _desktop application_ maupun _web application_ yang berbasis [FastPlaz](http://www.fastplaz.com)

Informasi penggunaan secara lengkap bisa dibaca dari dokumen [README SimpleAI](README-simpleai.md)

### SimpleBOT Package

Merupakan _package_ pendukung yang siap digunakan untuk membuat AI sederhana berbasis web. Package ini dikhususkan untuk aplikasi-aplikasi berbasis web yang menggunakan [FastPlaz](http://www.fastplaz.com).

SimpleBOT merupakan salah satu contoh penggunaan SimpleAI yang dipergunakan untuk membuat BOT.
Memiliki fitur menjawab otomatis, dan belajar suatu definisi kata sederhana.
Kecerdasan Bot ini tergantung dari data entities dan intent yang Anda miliki, serta logic handler yang bisa dibuat _custom_ sesuai kebutuhan.

SimpleBOT package sudah mendukung juga untuk digunakan sebagai Telegram BOT.

Contoh penggunaan bot sederhana dengan SimpleBOT ini bisa anda coba dari situs [ai.fastplaz.com](http://ai.fastplaz.com) atau bisa melalu aplikasi chat **Telegram**, silahkan hubungi contact *'Fastplaz Bot'*.

### Requirements

- FastPlaz_runtime
- SimpleAI package

Informasi penggunaan secara lengkap bisa dibaca dari dokumen [README SimpleBOT](README-simplebot.md)


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
=======
[![Stories in Ready](https://badge.waffle.io/luridarmawan/SimpleAI.png?label=ready&title=Ready)](https://waffle.io/luridarmawan/SimpleAI)
# SimpleAI
Simple AI with Pascal

