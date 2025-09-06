# Tagless-Final Key-Value Store in Haskell

This project is a demonstration of the "tagless-final" pattern in Haskell. It defines a simple key-value store interface and provides three distinct backends (interpreters) for it:

1.  An **HTTP backend** that communicates with a Python server.
2.  A low-level **RPC backend** that communicates with a C server.
3.  An **in-memory test backend** for fast, pure testing.

The primary goal is to showcase how a single, polymorphic piece of logic can be executed against completely different implementations without changing a single line of its code.

## The Tagless-Final Approach

### The Interface (`KVStore.hs`)

The core of the pattern in this project is the `KeyValueStore` typeclass defined in `src/KVStore.hs`:

```haskell
class Monad m => KeyValueStore m where
  requestPost :: Int -> String -> m ()
  requestGet  :: Int -> m String
```

This typeclass defines the "language" for interacting with our key-value store. It has two operations: `requestPost` and `requestGet`. The `m` is a type parameter representing the context (the monad) in which these operations will be executed.

### Polymorphic Business Logic (`Main.hs`)

Because the interface is abstract, we can write functions that are polymorphic over the implementation. The `exampleInteraction` function in `src/Main.hs` is an example. It can work with *any* monad `m` that has an instance of `KeyValueStore`.

```haskell
exampleInteraction :: KeyValueStore m => m ()
exampleInteraction = do
  requestPost 1 "First value"
  requestPost 2 "Second value"
  -- ... and so on
```

This function describes the *what* (the sequence of operations) without committing to the *how* (HTTP, RPC, or in-memory).

## Implementations (The "Interpreters")

We provide three implementations of the `KeyValueStore` typeclass. Each interpreter is a monad with a corresponding `instance KeyValueStore`.

### 1. HTTP Backend

This implementation interprets the `KeyValueStore` operations as HTTP requests to a simple Python server (`python/server.py`).

*   **`src/Http/Client.hs`**: Defines the `HttpM` monad (a `newtype` wrapper around `IO`) and its `KeyValueStore` instance. `requestPost` is translated into a raw HTTP `POST` request string, and `requestGet` into a `GET` request string.
*   **`src/Http/Lib.hs`**: Contains the `executeHttp` helper function, which handles the low-level socket logic to send the request and receive the response.

### 2. RPC Backend

This is a more complex backend that communicates with a C-based RPC server (`rpcc/kvstore_server.c`), which is generated using `rpcgen`.

*   **`src/Rpc/Client.hs`**: Defines the `RpcM` monad and its `KeyValueStore` instance. This is a wrapper that calls the functions from the RPC library.
*   **`src/Rpc/Lib.hs`**: This file contains a Haskell client for the RPC protocol. It manually constructs binary RPC call packets according to the XDR standard. It performs two main tasks:
    1.  **Port Discovery**: It first contacts the system's `rpcbind` daemon to find the correct port for the `KVSTORE_PROG` service.
    2.  **Call Execution**: It then opens a TCP socket to that port and sends a binary payload representing the `GET` or `POST` procedure call. It also parses the binary response.

### 3. In-Memory Test Backend

This implementation is for testing and does not perform any network I/O, making it fast and deterministic.

*   **`src/Test/Client.hs`**: Defines the `TestM` monad. It uses an `IORef` holding a `Data.Map Int String` to simulate the mutable key-value store. This is an example how tagless-final cn be used to create unit tests.

## Building and Running

### Prerequisites

You will need GHC, Cabal, Make, a C compiler, and `rpcbind`. On a Debian-based system, you can install them with:
```bash
sudo apt-get update
sudo apt-get install -y ghc cabal-install make gcc build-essential libtirpc-dev rpcbind
cabal update
```

### Build

Run the build script. This will compile the Haskell code with Cabal and the C RPC server with Make. All binaries will be placed in the `dist/` directory.
```bash
bash build.sh
```

### Test

The test script automates the entire process: it starts the `rpcbind` daemon, runs the Python HTTP server and the C RPC server in the background, and then executes the Haskell client to verify that all backends work correctly.
```bash
bash test.sh
```

### Manual Usage

You can also run the client directly from the command line.

```bash
# Post a key-value via HTTP
dist/main http post 1 "hello http"

# Get a value via RPC
dist/main rpc get 1

# Run the polymorphic example interaction against the in-memory test backend
dist/main run test
```