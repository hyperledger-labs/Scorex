2.0.0-M5
--------

* Issue #19 fixed
* MapDB dependency removed
* StateChanges reworked

2.0.0-M4
--------

* IODB dependency upgraded to 0.2.+
* TwinsChain example massively improved, README on it has been added 
(see "examples" folder)
* akka-http dependency removed, Swagger updated


2.0.0-M3
--------

Serialization have been reworked throughout the core to be consistent 
(see the new Serializer interface).

Handshake timeout implemented: (p2p/handshakeTimeout in settings.json)
Agent name and version have been moved to settings.json 
("agent" and "version" settings)
 
Hybrid chain example got bugfixing and new tests.


2.0.0-M2
--------

* Wallet interface added
* MvStore dependency removed
* StoredBlockchain trait removed
* ViewSynchronizer trait removed
* Miner and MiningController are removed from the core
* Maven artefact has been renamed from "scorex-basics" to "scorex-core"
* blockFields method of a Block has been removed