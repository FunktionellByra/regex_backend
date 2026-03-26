FROM haskell:9.10.3 AS builder

WORKDIR /app

COPY . . 

RUN cabal update && cabal build

EXPOSE 8080

CMD ["cabal", "run", "server"]