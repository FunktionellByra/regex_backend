FROM haskell:9.10.3 

WORKDIR /app

COPY . . 

RUN cabal update && cabal build

EXPOSE 8080

CMD ["cabal", "run", "server"]
