#!/bin/bash

NODES_COUNT=10


NODE_NAMES=""
for (( i=0; i<$NODES_COUNT; i++ ))
do
    NODE_NAMES+=" bully$i@`hostname`"
done

echo -e "Spawning erlang nodes...\n"
for (( i=0; i<$NODES_COUNT; i++ ))
do
    erl -pa out/production/bully-algorithm -sname "bully$i" -s coordinator_server start ${NODE_NAMES} -s init stop -noshell &
done

echo -e "Nodes were spawned. Press any key to exit.\n"
read

echo -e "Killing erlang nodes..."

#warning: this will kill all beam instances
pkill beam.smp

echo -e "Bye!\n"