# Text language identification using Wikipedia data

The aim of this project is to provide high-quality language detection over all the web's languages. The proxy for all web's languages is Wikipedia. Currently, we support 156 languages that have their Wikipedia entries.


## Usage

The main function is `text-langs` that returns 2 values:

- a lang - probability alist (languages are represented by their ISO-639-1 codes)
- a vector of tokens with their inferred langs

```
WILD> (text-langs "це тест")
((:UK . 0.5000003) (:RU . 0.4999998))
#(<це - UK:1.00> <тест - RU:1.00>)
```


## Running as a service

### Installation

1. Install SBCL
2. Get Quicklisp
3. Git clone project
4. `$ cd wiki-lang-detect; sbcl --load run.lisp`


## Running as a Docker

```
docker build -t wiki-lang-detect:latest .
docker run -it -p 5000:5000 wiki-lang-detect:latest

curl -X POST -H "Content-Type: application/json" -d "{'text': 'Несе Галя'}"  http://localhost:5000/detect | jq '.'
```

Or you can use prebuilt Docker image maintained outside of this repository.

```
docker run -it -p 5000:5000 chaliy/wiki-lang-detect:latest
```

### API

See [swagger definition](http://wild.lisp.kiev.ua/swagger)


## Helpful links:

- [Index of languages by writing system](http://www.omniglot.com/writing/langalph.htm)
- [Evaluating language identification performance](https://blog.twitter.com/2015/evaluating-language-identification-performance)
