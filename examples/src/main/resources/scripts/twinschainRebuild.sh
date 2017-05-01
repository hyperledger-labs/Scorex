#!/usr/bin/env bash

my_dir="$(dirname "$0")"
source "$my_dir/ips.sh"

for ip in "${ips[@]}"
do
  ssh ubuntu@$ip cd /home/ubuntu/Scorex && git pull origin master && sbt publishLocal && sbt "project examples" "assembly" && cp /home/ubuntu/Scorex/examples/target/scala-2.12/twinsChain.jar /home/ubuntu/data
done
