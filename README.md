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
3. In project directory run: `$ sbcl --load run.lisp`


## Helpful links:

- [Index of languages by writing system](http://www.omniglot.com/writing/langalph.htm)
- [Evaluating language identification performance](https://blog.twitter.com/2015/evaluating-language-identification-performance)
