2.0.0-RC4
---------
* *modifierCompanions* renamed to *modifierSerializers* in *NodeViewHolder*

2.0.0-RC3
---------
* *MinimalState* interface simplification: *validate()* puled away from the basic trait 
* *maxRollback* field added to *MinimalState*
* No *transactions* field with an optional value in *PersistentNodeViewModifier*, 
use *TransactionsCarryingPersistentNodeViewModifier* descendant for modifiers with transactions.
* Non-exhaustive pattern-matching fix in *NodeViewholder.pmodModify()*
* Simplification of type parameters in many classes around the whole codebase
* *FastCryptographicHash* removed
* Some obsolete code removed, such as *temp/mining* folder, *ScoreObserver* class
* Scrypto 2.0.0
* Using tagged types instead of *Array[Byte]*, *suppertagged* microframework is used for that 

2.0.0-RC2
---------
* *MinimalState* interface made minimal
* protocolVersion in P2P Handshake
* Scrypto 1.2.3
* *BoxMinimalState* moved to *scorex.mid.state*

2.0.0-RC1
---------
* Transaction interface simplified (*fee* & *timestamp* fields removed)
* Scala 2.12
* IODB 0.3.1
* *reportInvalid()* in History
* Issue #19 fixed
* MapDB dependency removed
* StateChanges reworked
* TwinsCoin example improved

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