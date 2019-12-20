# Scorex-testkit - test kit for Scorex projects

Scorex comes with a dedicated module scorex-testkit for supporting tests at different levels.
Testkit contains property tests common for all blockchains and allow to test blockchain node for them.

### Example:

All test scenarios are combined in BlockchainSanity test, leaving generators to concrete blockchain implementation. This generators are implemented in [a test for the Hybrid example](https://github.com/ScorexFoundation/Scorex/blob/master/examples/src/test/scala/hybrid/HybridSanity.scala).

### Implemented test scenarios
- [x] Valid block should be successfully applied to history and available by id after that.
- [x] Valid box should be successfully applied to state, it's available by id after that.
- [x] State should be able to generate changes from valid block and apply them.
- [x] Wallet should contain secrets for all it's public propositions.
- [x] State changes application and rollback should lead to the same state and the component changes should also be rolled back.
- [x] Transactions successfully added to memory pool should be available by id.
- [x] Transactions once added to a block should be removed from the local copy of mempool.
- [x] Mempool should be able to store a lot of transactions and filtering of valid and invalid transactions should be fast.
- [x] Minimal state should be able to add and remove boxes based on received transaction's validity.
- [x] Modifier (to change state) application should lead to new minimal state whose elements' intersection with previous ones is not complete (at least some new boxes are introduced and some previous ones removed).
- [x] Application of the same modifier twice should be unsuccessful.
- [x] Application of invalid modifier (inconsistent with the previous ones) should be unsuccessful.
- [x] Application of a valid modifier after rollback should be successful.
- [x] Invalid modifiers should not be able to be added to history.
- [x] Once an invalid modifier is appended to history, then history should not contain it and neither should it be available in history by it's id.
- [x] History should contain valid modifier and report if a modifier if semantically valid after successfully appending it to history.
- [x] BlockchainSanity test that combines all this test.

### Coming test scenarios:
- [ ] Block application and rollback leads to the same history (rollback is not defined for history yet)
- [ ] NodeView apply block to both state and history or don't apply to any of them
- [ ] It's not possible to apply transaction to a state twice
- [ ] Tests for invalid transactions/blocks/etc.


