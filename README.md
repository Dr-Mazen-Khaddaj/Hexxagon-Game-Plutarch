# Hexxagon Game on Cardano
Welcome to the official repository for the Hexxagon Game on Cardano. This project brings the classic Hexxagon game to the Cardano blockchain, allowing players to compete by locking cryptocurrency. The winner takes the total sum of crypto. Our implementation leverages the high security and determinism of the Cardano blockchain to ensure a fair and secure gaming experience.

# Project Setup
This repository contains the on-chain code for the Hexxagon Game on Cardano. The smart contracts are written in Plutarch.

# Prerequisites
Linux OS
Nix (See the [Nix Installation Guide](NIX-INSTALLATION.md) for detailed setup instructions)

# Installation
**Clone the Repository:**
```bash
git clone https://github.com/Dr-Mazen-Khaddaj/Hexxagon-Game-Plutarch.git
```
**Clone the Off-chain Code Repository:**
Note: Ensure to clone the off-chain repository inside the main repository to maintain common modules.
```bash
cd Hexxagon-Game-Plutarch/
git clone https://github.com/Dr-Mazen-Khaddaj/Hexxagon-Game-Atlas.git
```
**Set Up the Development Environment:**
This command sets up the Nix environment needed for development. This may take some time.
```bash
nix develop
```
**Build the Project:**
```bash
cabal update
cabal build
```
**Run Tests and Serialize Contracts:**
```bash
cabal run Hexxagon-Game-Plutarch-test   # Run smart contract tests
cabal run                               # Serialize smart contracts to CBOR and write them to files
```

# Contact
For any issues or further questions, please contact Dr. Mazen Khaddaj at mazenkhaddaj@outlook.com.

# License
This project is not licensed under any open source license.