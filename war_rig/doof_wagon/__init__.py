"""Doof Wagon integration bridge for War Rig.

Exposes schema-validated IPC over the contract defined in the Doof Wagon SPEC.
All public behavior is additive: when the `--doof-wagon-output` flag is absent
on a CLI command, War Rig behaves exactly as before.

See: /home/andras/doof-wagon/SPEC.md
"""

from .bridge import (
    DoofWagonOptions,
    emit_open_questions,
    load_priors,
    wait_for_signal,
)

__all__ = [
    "DoofWagonOptions",
    "emit_open_questions",
    "load_priors",
    "wait_for_signal",
]
