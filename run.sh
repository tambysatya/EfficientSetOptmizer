#!/bin/sh

EXEC="/home/sat/.local/bin/EfficientSetOptimizer-exe"

run_kp (){
    p=$1
    n=$2
    for i in {1..10}
    do
        $EXEC KP Instances/SatKP_random/SatKP_p-${p}_n-${n}_i-${i}.ins finalKP.log
    done
}

run_kp 3 100
run_kp 4 100
run_kp 5 100
