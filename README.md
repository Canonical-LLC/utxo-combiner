# `utxo-combiner`

`utxo-combiner` is a simple utility for combining UTxOs at an address. Start the utility by passing in an address, signing key, batch size and optional testnet magic:

```
cabal run utxo-combiner -- -a addr_test1vzal5wcx2cnfvux84kf8clcj65ej2qklhtahecr5caytrdqr0jl5z -s ~/testnet/marketplace.skey -c 8 -t 1097911063
```

It will then start to poll the blockchain and create a transaction to combine the UTxOs when it detects there are over the batch size amount (specified by `-c 8` here, so it needs at least 8 UTxOs before it will combine them).

See `cabal run utxo-combiner -- --help` for more options.

The utility needs the `cardano-cli` on path. `cardano-cli` requires the `CARDANO_NODE_SOCKET_PATH` is set as well.

## Building

To build run:

```
cabal build
```

The `cabal run` command above will also build the utility.
