FROM debian:buster

RUN apt-get update && apt-get install -yq curl npm
RUN curl -sSL https://get.haskellstack.org/ | sh
COPY . /boggle
COPY ./frontend/index.html /
WORKDIR /boggle/server
RUN stack build
RUN apt-get install -y wamerican
RUN stack install
EXPOSE 8080
CMD ["/root/.local/bin/boggle-solver","/usr/share/dict/words","/index.html"]