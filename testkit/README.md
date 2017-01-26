# Scorex-testkit - test kit for Scorex projects

Scorex comes with a dedicated module scorex-testkit for supporting tests at different levels.
Testkit contains property tests common for all blockchains and allow to test blockchain node for them.

### Example:
TODO
### Implemented test scenarios
- If block is successfully applied to history, it's available by id
- If box is successfully applied to state, it's available by id
- State is able to generate changes from block and apply them
- Wallet contains secrets for all it's public propositions
- State changes application and rollback leads to the same state

### Coming test scenarios:
- Block application and rollback leads to the same history (rollback is not defined for history yet)
- NodeView apply block to both state and history or don't apply to any of them
- Transactions added to memory pool are available by id
- It's not possible to apply transaction to a state twice
