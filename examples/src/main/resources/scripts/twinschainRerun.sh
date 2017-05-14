#!/usr/bin/env bash

my_dir="$(dirname "$0")"
source "$my_dir/ips.sh"

for ip in "${ips[@]}"
do
   echo "ubuntu@$ip"
   ssh ubuntu@$ip "pkill -f twinsChain"
   ssh ubuntu@$ip "rm -f /tmp/l.log && rm -rf /home/ubuntu/data/data && rm -f /home/ubuntu/data/l.log"

#   ssh ubuntu@$ip "sed -i '/\"RparamX10\"/c\  \"RparamX10\": 8,' /home/ubuntu/data/settings.json"
#   ssh ubuntu@$ip "sed -i '/\"addedMaxDelay\"/c\    \"addedMaxDelay\": 0,' /home/ubuntu/data/settings.json"
   ssh ubuntu@$ip "sed -i '/\"targetBlockDelayMillis\"/c\    \"targetBlockDelayMillis\": 60000,' /home/ubuntu/data/settings.json"

   ssh ubuntu@$ip "nohup java -jar -XX:-UseGCOverheadLimit /home/ubuntu/data/twinsChain.jar /home/ubuntu/data/settings.json > /tmp/l.log 2>&1 &"
   sleep 30
done


