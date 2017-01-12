Scorex 2 - The modular blockchain framework [![Build Status](https://travis-ci.org/ScorexFoundation/Scorex.svg?branch=master)](https://travis-ci.org/ScorexFoundation/Scorex)
====================================================================================================================================================================================

[![Join the chat at https://gitter.im/input-output-hk/Scorex](https://badges.gitter.im/input-output-hk/Scorex.svg)](https://gitter.im/input-output-hk/Scorex?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Scorex and Scorex 2
-------------------

It is experimental and still raw new major release of the Scorex. Previous
version with documentation could be found at [https://github.com/input-output-hk/Scorex](https://github.com/input-output-hk/Scorex).


Motivation
----------

 Bitcoin Core source code contains more 100K lines of code(80K of C++ only), Nxt is more than 45K
 line of Java code. All parts of the design(network/transactional/consensus protocols) are mixed in a hard way.
 So researchers and developers are not in good start positions to make experiments.

 In opposite, Scorex core is less than 4K lines of Scala code. Abstract core concepts allow to implement a broad range
 of systems, including ones with multiple types of blocks and non-linear history.

Features
--------

* Compact, functional code
* Modular design with fine granularity
* Scala language
* Asynchronous network layer on top of TCP
* JSON API
* Command line client for the JSON API
* Cryptographic primitives externalized into [separate scrypto framework](https://github.com/input-output-hk/scrypto)
* Few examples out-of-box

Documentation
-------------

[Please refer to the tutorial](https://github.com/ScorexFoundation/ScorexTutorial)


Examples
--------

There are two examples of blockchain systems built with Scorex. Details are available in 
the [dedicated readme](examples/README.md).



Contributions
-------------

Contributions are welcome! Please take a look into [issues](https://github.com/ScorexFoundation/ScorexTutorial/issues).
 Testing codebase is very small at the moment, so writing a test is not just good for start, but useful for the product as well.

License
-------

To the extent possible under law, the authors have dedicated all copyright and related and neighboring
rights to this software to the public domain worldwide. This software is distributed without any warranty.
You can find applied CC0 license legalcode in the [COPYING](COPYING)