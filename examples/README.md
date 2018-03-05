# Scorex Example: Twinscoin


Currently, Scorex's distribution contains one example of blockchain system built on top of Scorex: **TwinsCoin**.

Twinscoin is a hybrid Proof-of-Work(PoW) and Proof-of-Stake(PoS) cryptocurrency with 
1:1 correspondence between PoW and PoS blocks. The main goal is to protect the hybrid 
chain against majority of hashing power being adversarial.

The main characteristic of the implementation are: 

* The use of the [Twinscoin consensus protocol](https://eprint.iacr.org/2017/232.pdf);

* A Bitcoin-like transaction model with a transaction having multiple inputs and outputs (with no [scripts](https://en.bitcoin.it/wiki/Script), however);

* Persistence through the [IODB](https://github.com/input-output-hk/iodb) versioned key-value database.


### Execution

To run, you need to have Java8 and [SBT (Scala Build Tool)](http://www.scala-sbt.org) installed.

In the terminal, run `sbt`, and then type the following commands in the SBT shell:

1. `> project examples`
2.  `> runMain examples.hybrid.HybridApp src/main/resources/settings.conf`

This will create and run a node with the given settings. To create and run more nodes, repeat the steps above using the other setting files (e.g. `settings2.conf`, `settings3.conf`, ...) in the `src/main/resources/` folder.


_Note to MacOS users:_ The 127.0.0.X loopback addresses (for X > 1) are disabled by default in MacOS. Therefore, the creation of nodes with `settingsX.conf` (for X > 1) fails. To prevent the failure, [enable the addresses](https://superuser.com/questions/458875/how-do-you-get-loopback-addresses-other-than-127-0-0-1-to-work-on-os-x?noredirect=1&lq=1).


### Web User Interface

You can interact with the nodes you created through a web browser. For the node created with `settings.conf`, visit `localhost:9085`.

TODO: Add instructions about how to perform a transaction through `wallet/transfer`.

