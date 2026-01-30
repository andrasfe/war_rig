"""Input/Output operations for War Rig.

This package provides file reading and output generation:

- reader: Source file reading and discovery
- writer: Output generation (JSON, Markdown)

Example:
    from war_rig.io import SourceReader, DocumentationWriter

    reader = SourceReader(config.system)
    files = reader.discover_files()
    source = reader.read_file(files[0])

    writer = DocumentationWriter(config.system)
    writer.write_documentation(result)
"""

from war_rig.io.reader import SourceFile, SourceReader
from war_rig.io.writer import DocumentationWriter

__all__ = [
    "DocumentationWriter",
    "SourceFile",
    "SourceReader",
]
