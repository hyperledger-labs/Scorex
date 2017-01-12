Scorex Examples
===============

There are two examples of blockchain systems built on top of Scorex at the moment.
The first one is the simplest proof-of-stake cryptocurrency with account-based transactional model
and everything stored in memory, and could be found under the 
[src/main/scala/examples/curvepos](src/main/scala/examples/curvepos) folder. The second one is TwinsChain 
we are going to explain in details.


TwinsChain
----------

TwinsChain is a hybrid Proof-of-Work(PoW) and Proof-of-Stake(PoS) cryptocurrency with 
1:1 correspondence between PoW and PoS blocks. The main goal is to protect the hybrid 
chain against majority of hashing power being adversarial.

Main points of the implementation: 

* TwinsChain consensus protocol as described in the paper [TODO: link]

* Bitcoin-like transactional model with a transaction having multiple inputs and outputs. There are no 
[Bitcoin-like scripts](https://en.bitcoin.it/wiki/Script) though.

* Persistence done by using [IODB](https://github.com/input-output-hk/iodb) versioned key-value database

To run, you need to install Java8 and SBT (Scala Build Tool) first. 

run `sbt`

then type following in the sbt console:

```
    project examples
    run-main examples.hybrid.HybridApp 
```

Use `run-main examples.hybrid.HybridApp settings2.json`...
`run-main examples.hybrid.HybridApp settings10.json` to create more local nodes on your machine,
or `run-main examples.hybrid.HybridApp testnet-twins.json` to connect to a public testnet.



Command-Line Client
-------------------

Run ./cli.sh after launching server to issue API requests to it via command-line client. See API section below.
Some examples of CLI commands:

 * GET blocks/first
 * POST payment {"amount":400, "fee":1, "sender":"2kx3DyWJpYYfLErWpRMLHwkL1ZGyKHAPNKr", "recipient":"Y2BXLjiAhPUMSo8iBbDEhv81VwKnytTXsH"}