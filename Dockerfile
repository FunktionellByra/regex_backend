# ---- Build stage ----
FROM haskell:9.10.3 AS builder

WORKDIR /app
COPY . .

RUN cabal update && \
    cabal build exe:server

RUN cp $(cabal list-bin exe:server) /server

# ---- Runtime stage ----
FROM debian:bookworm-slim

WORKDIR /app

COPY --from=builder /server /server

EXPOSE 8080

CMD ["/server"]