/* kvstore.x - Key-Value store RPC interface */

const KV_MAXLEN = 1024;

/* Argument structure for POST */
struct kvpair {
    int key;
    string value<KV_MAXLEN>;
};

program KVSTORE_PROG {
    version KVSTORE_VERS {
        void POST(kvpair) = 1;
        string GET(int) = 2;
    } = 1;
} = 0x20000001;   /* matches rpcProgram in Haskell */
