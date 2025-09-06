#!/bin/bash

set -e

kill_servers() {
    pkill -f "python3 dist/http_server.py" || true
    pkill -f "dist/rpc_server" || true
}

verify_interaction() {
    local mode=$1
    echo "Testing ${mode^^} interaction..."
    ${MAIN} "$mode" post 1 "V_1" 2>> test.log
    ${MAIN} "$mode" post 2 "V_2" 2>> test.log
    ${MAIN} "$mode" post 3 "V_3" 2>> test.log

    V_1=$(${MAIN} "$mode" get 1 2>> test.log)
    V_2=$(${MAIN} "$mode" get 2 2>> test.log)
    V_3=$(${MAIN} "$mode" get 3 2>> test.log)

    echo "Retrieved via $mode:"
    echo "Key 1: $V_1"
    echo "Key 2: $V_2"
    echo "Key 3: $V_3"
    if [[ "$V_1" != "V_1" || "$V_2" != "V_2" || "$V_3" != "V_3" ]]; then
        echo "${mode^^} interaction failed!"
        exit 1
    else
        echo "${mode^^} interaction succeeded!"
    fi
}

MAIN="dist/main"

rm -f test.log

# Kill servers if running
kill_servers

# Ensure rpcbind is running
systemctl is-active --quiet rpcbind || sudo systemctl start rpcbind

echo "rpcbind daemon"
ps -aux | grep rpcbind

# Start HTTP server
python3 dist/http_server.py &>> test.log &

echo "HTTP server"
ps -aux | grep http_server.py

# Start RPC server
dist/rpc_server &>> test.log &

echo "RPC server"
ps -aux | grep rpc_server
rpcinfo -p

# Wait a while
sleep 5

# Verify that the servers are running
if ! pgrep -f "python3 dist/http_server.py" > /dev/null;
then
    echo "HTTP server failed to start!"
    exit 1
fi

if ! pgrep -f "dist/rpc_server" > /dev/null;
then
    echo "RPC server failed to start!"
    exit 1
fi

# Verify HTTP interaction
verify_interaction http
# Verify RPC interaction
verify_interaction rpc

# Testing polymorphic interaction
echo "Testing polymorphic interaction..." >> test.log
dist/main run rpc 2>> test.log
dist/main run http 2>> test.log
dist/main run test 2>> test.log

# Clean up running servers
kill_servers