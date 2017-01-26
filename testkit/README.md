# Scorex-testkit - test kit for Scorex projects

Scorex comes with a dedicated module scorex-testkit for supporting tests at different levels.
Testkit contains property tests common for all blockchains and allow to test blockchain node for them.

### Example:
TODO
### Implemented test scenarios
- If block is successfully applied to history, it's available by id
- If box is successfully applied to state, it's available by id
- State is able to generate changes from block and apply them

### Coming test scenarios:
- Block application and rollback leads to the same state
- Block application and rollback leads to the same history
- NodeView apply block to both state and history or don't apply to any of them
- Wallet contains secrets for all it's public propositions
- Transactions added to memory pool are available by id
