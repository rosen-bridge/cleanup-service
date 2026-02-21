# Cleanup Service

Rosen Bridge cleanup service for handling fraud detection and slashing operations.

## Overview

This service monitors the Rosen Bridge for fraudulent behavior by watchers and executes slashing transactions to penalize malicious actors.

## Structure

- `packages/fraud-tx`: Transaction builder for creating fraud boxes from trigger events
- `packages/slash-tx`: Transaction builder for slashing fraudulent watchers
- `services/cleanup`: Main service for monitoring trigger events, creating fraud transactions, and executing slashing

## Getting Started

```bash
# Install dependencies
npm install

# Build all packages
npm run build

# Run tests
npm test

# Lint code
npm run lint
```

## Development

This is a monorepo managed with npm workspaces. Each package and service has its own `package.json` and can be developed independently.

## License

See the main Rosen Bridge repository for license information.
