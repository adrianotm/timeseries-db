FROM haskell:8

WORKDIR /app

COPY stack.yaml /app
COPY package.yaml /app
RUN stack setup && \
  stack build --only-dependencies

COPY . /app
RUN stack build --copy-bins --local-bin-path /usr/local/bin

FROM ubuntu:20.04

COPY --from=0 /usr/local/bin /usr/local/bin

EXPOSE 8081
CMD /usr/local/bin/server
