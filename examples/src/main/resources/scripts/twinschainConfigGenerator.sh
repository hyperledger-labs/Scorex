#!/usr/bin/env bash

my_dir="$(dirname "$0")"
source "$my_dir/ips.sh"

echo "Arguments: number of configs, data folder"
limit=$(($1+21))
for i in $(seq 21 $limit);
do
   folder="$2/data-$i"
   port=`expr 19000 + $i`
   rpcPort=`expr 29000 + $i`
   original_string="genesis$i"
   seed="${original_string/0/o}"
   peers="\"52.40.210.252:9084\", \"52.88.43.127:9084\", \"35.167.200.197:9084\", \"35.163.27.226:9084\", \"35.167.74.210:9084\", \"54.70.0.50:9084\", \"52.89.211.42:9084\", \"35.167.43.13:9084\", \"35.167.184.105:9084\", \"35.165.178.93:9084\", \"54.191.32.214:9084\", \"52.26.217.230:9084\", \"52.24.190.196:9084\", \"34.209.224.39:9084\", \"52.25.11.49:9084\", \"52.41.61.115:9084\", \"35.163.92.164:9084\", \"35.161.164.5:9084\", \"52.40.158.125:9084\", \"52.24.217.224:9084\""
   conf="{\n  \"p2p\": {\n    \"name\": \"generatedNode$i\",\n    \"bindAddress\": \"0.0.0.0\",\n    \"upnp\": false,\n    \"upnpGatewayTimeout\": 7000,\n    \"upnpDiscoverTimeout\": 3000,\n    \"port\": $port,\n    \"knownPeers\": [$peers],\n    \"addedMaxDelay\": 0,\n    \"maxConnections\": 100\n  },\n  \"RparamX10\": 8,\n  \"targetBlockDelayMillis\": 60000,\n  \"walletDir\": \"$folder/wallet\",\n  \"walletPassword\": \"cookies\",\n  \"walletSeed\": \"$seed\",\n  \"dataDir\": \"$folder/data\",\n  \"logDir\": \"$folder/logs\",\n  \"rpcPort\": $rpcPort,\n  \"rpcAllowed\": [],\n  \"agent\": \"generatedNode$i\",\n  \"version\": [0, 0, 1],\n  \"maxRollback\": 100,\n  \"testnet\": true,\n  \"offlineGeneration\": false,\n  \"blockGenerationDelay\": 100,\n  \"cors\": false\n}"
   printf "$conf" >> "settings-$i.json"
done
