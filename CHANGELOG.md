# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.1] - 2025-03-28

### Fixed
- Removed erroneous `#eval Stream` line in Raw.lean
- Fixed inconsistent spacing in `deriving` clauses
- Improved normalizeSample function by removing redundant variable
- Added missing BEq derivation for SyncPulse structure

## [0.1.0] - 2025-03-28

### Added
- Initial project setup
- Added original ntsc-demodulator repository as a submodule
- Created basic documentation including README and LICENSE
- Setup .gitignore to exclude debug and target directories