# Scorex-testkit - test kit for Scorex projects

Scorex comes with a dedicated module scorex-testkit for supporting tests at different levels.
Testkit contains property tests common for all blockchains and allow to test blockchain node for them.

### Example:
TODO
### Implemented test scenarios
- Valid block should be successfully applied to history and available by id after that
- Valid box should be successfully applied to state, it's available by id after that
- State should be able to generate changes from valid block and apply them
- Wallet should contain secrets for all it's public propositions
- State changes application and rollback should lead to the same state
- Transactions successfully added to memory pool should be available by id

### Coming test scenarios:
- Block application and rollback leads to the same history (rollback is not defined for history yet)
- NodeView apply block to both state and history or don't apply to any of them
- It's not possible to apply transaction to a state twice
- Tests dor invalid transactions/blocks/etc.


