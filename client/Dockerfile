FROM codesimple/elm:0.19

WORKDIR /app
COPY . /app

RUN mkdir dest && elm make src/Main.elm --output dest/index.html

FROM nginx:alpine

COPY --from=0 /app/dest /usr/share/nginx/html
