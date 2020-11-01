FROM ciceron/ada-servlet:latest

MAINTAINER Stephane Carrez <Stephane.Carrez@gmail.com>

RUN mkdir -p /usr/src \
   && cd /usr/src \
   && git clone https://github.com/stcarrez/swagger-ada.git openapi-ada \
   && cd /usr/src/openapi-ada \
   && ./configure --disable-shared --enable-server --prefix=/usr \
   && make -s \
   && make -s install

WORKDIR /usr/src
