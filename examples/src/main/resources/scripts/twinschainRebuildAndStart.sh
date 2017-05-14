#!/usr/bin/env bash

my_dir="$(dirname "$0")"
source "$my_dir/ips.sh"

for ip in "${ips[@]}"
do
  echo "$ip"
  ssh ubuntu@$ip "pkill -f twinsChain"
  ssh ubuntu@$ip "rm -f /tmp/l.log && rm -rf /home/ubuntu/data/data && rm -f /home/ubuntu/data/l.log && rm -rf /home/ubuntu/Scorex/examples/target/scala-2.12/twinsChain.jar && rm -rf /home/ubuntu/data/twinsChain.jar"

  ssh ubuntu@$ip "cd /home/ubuntu/Scorex && git pull origin master && sbt publishLocal && sbt \"project examples\" \"assembly\" && cp /home/ubuntu/Scorex/examples/target/scala-2.12/twinsChain.jar /home/ubuntu/data/twinsChain.jar"

   ssh ubuntu@$ip "nohup java -jar -XX:-UseGCOverheadLimit /home/ubuntu/data/twinsChain.jar /home/ubuntu/data/settings.json > /tmp/l.log 2>&1 &"
   sleep 30
done
