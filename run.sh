#!/bin/bash

NODES_COUNT=10


NODE_NAMES=""
for (( i=0; i<$NODES_COUNT; i++ ))
do
    NODE_NAMES+=" bully$i@`hostname`"
done

echo "Spawning erlang nodes..."
for (( i=0; i<$NODES_COUNT; i++ ))
do
    erl -pa out/production/bully-algorithm -sname "bully$i" -s coordinator_server start ${NODE_NAMES} -s init stop -noshell &
done

sleep 1

echo -e "\nErlang nodes have been spawned."
echo "You can now try to kill a leader, say with \"kill -9 <LEADER_PID>\". Have fun."
echo "Press any key to exit."
echo
read

echo "Killing erlang nodes..."

#warning: this will kill all beam instances
pkill beam.smp

echo "Bye!"