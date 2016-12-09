FROM ubuntu:latest

MAINTAINER Mike Chaliy <mike@chaliy.name>

RUN set -ex \
  && apt-get update \
  && apt-get upgrade -y \
  && apt-get install -y wget openssl libev-dev build-essential make wget bzip2

RUN  set -ex \
  && wget http://prdownloads.sourceforge.net/sbcl/sbcl-1.3.6-x86-64-linux-binary.tar.bz2 -O /tmp/sbcl.tar.bz2 \
  && mkdir /tmp/sbcl \
  && tar jxvf /tmp/sbcl.tar.bz2 --strip-components=1 -C /tmp/sbcl/ \
  && cd /tmp/sbcl \
  && sh install.sh \
  && cd /tmp \
  && rm -rf /tmp/sbcl/

RUN  set -ex \
 && wget https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --non-interactive --load /quicklisp.lisp --eval "(quicklisp-quickstart:install)" \
 && sbcl --non-interactive --eval '(with-open-file (out "/root/.sbclrc" :direction :output)(format out "(load \"/root/quicklisp\/setup.lisp\")"))'

ADD . /wiki-lang-detect

RUN  set -ex \
 && sbcl --non-interactive --load /wiki-lang-detect/init.lisp

EXPOSE 5000
CMD sbcl --non-interactive --load /wiki-lang-detect/run.lisp
