"""Entry point for running war_rig as a module.

Usage:
    python -m war_rig <command> [options]

Examples:
    python -m war_rig batch path/to/source/directory
    python -m war_rig analyze path/to/PROGRAM.cbl
    python -m war_rig status
"""

from war_rig.cli import app

if __name__ == "__main__":
    app()
