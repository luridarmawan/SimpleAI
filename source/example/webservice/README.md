# WebService example




## How to use it

Langkah-langkah umum dalam deploy webservice NLP ini :

1. Copy file-file nlp data ke folder `public_html\files\nlp`
2. Compile NLP source yang ada di folder `source\nlp`
   akan terbentuk file `nlp.bin` di folder `public_html`
3. Buat virtual hosting di apache yang mengarah ke folder public_html
4. Akses nlp service ini dengan format 
   http://your-url/?text=hi%20apa%20kabar



### Requirements

- FastPlaz_runtime
- SimpleAI package


