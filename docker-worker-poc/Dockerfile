FROM ubuntu:18.04

RUN apt-get update

RUN apt-get install -y sudo
RUN apt-get install -y curl

RUN apt-get install -y libstdc++6
RUN apt-get install -y gcc
RUN apt-get install -y g++
RUN apt-get install -y python3 python3-pip python3-dev python3-numpy
RUN curl -sSf https://static.rust-lang.org/rustup.sh | sh

RUN apt-get install -y software-properties-common
RUN apt-get install -y openjdk-8-jdk
RUN apt-get install -y scala

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/mono-xamarin.list
RUN apt-get update
RUN apt-get install -y mono-devel

RUN apt-get install -y ruby
RUN gem install bundler 

RUN apt-get install -y golang

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get install -y php

RUN apt-get install -y build-essential
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN apt-get install -y nodejs

RUN apt-get install -y ocaml

RUN sh -c 'echo "$(curl -fsSL https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein)" > /usr/bin/lein'
RUN chmod a+x /usr/bin/lein
RUN lein

RUN apt-get install -y git

RUN pip3 install numpy scipy scikit-learn pillow h5py