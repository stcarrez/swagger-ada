FROM ciceron/ada-servlet:latest

MAINTAINER Stephane Carrez <Stephane.Carrez@gmail.com>

# We have to setup a timezone otherwise installation of tzdata will hang.
ENV TZ=Europe/Paris
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get update \
   && apt-get install -y openjdk-17-jre-headless

RUN mkdir -p /usr/src \
   && cd /usr/src \
   && git clone https://github.com/stcarrez/swagger-ada.git openapi-ada \
   && cd /usr/src/openapi-ada \
   && ./configure --disable-shared --enable-server --prefix=/usr \
   && make -s build \
   && make -s install

WORKDIR /usr/src
