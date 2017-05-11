#!/usr/bin/env bash

my_dir="$(dirname "$0")"
source "$my_dir/ips.sh"

for ip in "${allIps[@]}"
do
   echo "$ip"
   curl -s -X GET --header 'Accept: application/json' 'http://'$ip':9085/debug/info' | grep height
done