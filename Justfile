# Justfile for NTSCDemodulator project

# List all available commands
default:
    @just --list

# Update all git submodules
update-submodules:
    git submodule update --init --recursive

# Check status of all submodules
submodule-status:
    git submodule status

# Run tests for the project
test:
    @echo "Running tests..."
    # Add test commands here as the project develops

# Build the project
build:
    @echo "Building project..."
    lake build

# Clean build artifacts
clean:
    @echo "Cleaning build artifacts..."
    lake clean

# Format code according to project standards
format:
    @echo "Formatting code..."
    # Add formatting commands here as the project develops

# Update changelog with new version
changelog VERSION MESSAGE:
    @echo "Updating changelog with version {{VERSION}}..."
    @sed -i '' "s/## \[Unreleased\]/## [Unreleased]\n\n## [{{VERSION}}] - $(date +%Y-%m-%d)/" CHANGELOG.md
    @sed -i '' "/## \[{{VERSION}}\]/a\\\n### Added\n- {{MESSAGE}}" CHANGELOG.md

# Run Lean 4 REPL
repl:
    lake env lean

# Create a new Lean 4 module file
new-module NAME:
    @echo "Creating new module {{NAME}}..."
    @echo "/-\nNTSC Demodulator - {{NAME}} Module\n-/\n\nimport NTSCDemodulator\n\nnamespace NTSCDemodulator.{{NAME}}\n\n-- Module contents go here\n\nend NTSCDemodulator.{{NAME}}" > NTSCDemodulator/{{NAME}}.lean

# Run typechecking on the codebase
typecheck:
    lake env lean --no-run --make NTSCDemodulator