FROM buildpack-deps:latest

ENV STACK_DOWNLOAD_URL https://github.com/commercialhaskell/stack/releases/download/v0.1.6.0/stack-0.1.6.0-linux-x86_64.tar.gz
ENV DEBIAN_FRONTEND noninteractive
ENV PATH $PATH:/root/.local/bin
ENV LANG C.UTF-8

RUN apt-get update -q && \
    apt-get install -qy libgmp-dev libpq-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /root/.local/bin && \
    wget -q $STACK_DOWNLOAD_URL && \
    tar xvf stack-0.1.6.0-linux-x86_64.tar.gz && \
    cd stack-0.1.6.0-linux-x86_64 && \
    chmod +x stack && \
    mv stack /root/.local/bin

RUN mkdir /app
WORKDIR /app
COPY . /app/
RUN stack setup
RUN stack build
