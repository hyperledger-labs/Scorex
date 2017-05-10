#!/usr/bin/env bash

my_dir="$(dirname "$0")"
source "$my_dir/ips.sh"

for ip in "${ips[@]}"
do
   echo "ubuntu@$ip"
   ssh ubuntu@$ip "pkill -f twinsChain"
   ssh ubuntu@$ip "rm -f /tmp/l.log"
   ssh ubuntu@$ip "rm -rf /home/ubuntu/data/data"
   ssh ubuntu@$ip "rm -f /home/ubuntu/data/l.log"
   ssh ubuntu@$ip "nohup java -jar -XX:-UseGCOverheadLimit /home/ubuntu/data/twinsChain.jar /home/ubuntu/data/settings.json > /tmp/l.log 2>&1 &"
done


