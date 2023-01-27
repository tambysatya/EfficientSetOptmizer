#!/bin/sh

EXEC="/home/sat/.local/bin/EfficientSetOptimizer-exe"

run_kp (){
    p=$1
    n=$2
    for i in {1..10}
    do
        timeout 7200 $EXEC KP Instances/SatKP_random/SatKP_p-${p}_n-${n}_i-${i}.ins finalKP.log
    done
}
run_ap (){
    p=$1
    n=$2
    for i in {1..10}
    do
        timeout 7200 $EXEC AP Instances/SatAP_random/SatAP_p-${p}_n-${n}_i-${i}.ins finalAP.log
    done
}



#run_kp 3 100
#run_kp 4 100
#run_kp 5 100
run_ap 3 30
run_ap 4 30
run_ap 5 30

