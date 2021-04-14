FROM haskell:8

WORKDIR /app
COPY . /app
RUN stack setup && \
  stack exec -- ghc --version && \
  stack build --copy-bins --local-bin-path /usr/local/bin

EXPOSE 8081
CMD /usr/local/bin/server
