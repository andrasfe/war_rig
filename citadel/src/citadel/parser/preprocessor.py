"""
Preprocessor for source code parsing.

Handles comment stripping, string masking, and line continuation
to prepare source content for pattern-based extraction. Preserves
line number mappings for accurate error reporting.
"""

import re
from typing import ClassVar

from pydantic import BaseModel, Field

from citadel.specs.schema import ArtifactSpec


class PreprocessedSource(BaseModel):
    """
    Result of preprocessing source content.

    Contains the cleaned content suitable for pattern matching,
    along with mappings back to the original source for error reporting.

    Attributes:
        cleaned: Cleaned content with comments stripped and strings masked.
        original: Original unmodified content.
        line_map: Mapping from cleaned line numbers to original line numbers.
        masked_strings: List of (start_pos, end_pos, original_string) tuples
            recording what strings were masked and where.
    """

    cleaned: str
    original: str
    line_map: dict[int, int] = Field(default_factory=dict)
    masked_strings: list[tuple[int, int, str]] = Field(default_factory=list)


class Preprocessor:
    """
    Handles comment stripping, string masking, and line continuation.

    Prepares source content for pattern-based extraction by removing
    or masking elements that could cause false matches. Preserves
    line number mappings for accurate error reporting.

    The processing order is:
    1. Strip fixed-column comments (COBOL-style)
    2. Strip line comments
    3. Strip block comments
    4. Mask string literals with unique placeholders
    5. Join continuation lines
    """

    # Constants for string placeholder generation
    STRING_PLACEHOLDER_PREFIX: ClassVar[str] = "__STR_"
    STRING_PLACEHOLDER_SUFFIX: ClassVar[str] = "__"

    def __init__(self, spec: ArtifactSpec) -> None:
        """
        Initialize preprocessor with language spec.

        Args:
            spec: Artifact specification defining language syntax
                  (comments, strings, continuation rules).
        """
        self._spec = spec
        self._string_counter = 0

    def process(self, content: str) -> PreprocessedSource:
        """
        Clean source for pattern matching.

        Applies the following transformations in order:
        1. Strip fixed-column comments (COBOL-style column 7)
        2. Strip line comments (// or # style)
        3. Strip block comments (/* */ style)
        4. Mask string literals with unique placeholders
        5. Join continuation lines

        Args:
            content: The original source code content.

        Returns:
            PreprocessedSource with cleaned content and mappings
        """
        # Reset string counter for each new file
        self._string_counter = 0
        original = content

        # Initialize identity line map
        lines = content.splitlines()
        line_map: dict[int, int] = {i + 1: i + 1 for i in range(len(lines))}

        # Step 0: Strip sequence number columns (e.g., COBOL columns 1-6)
        if self._spec.comments.strip_columns_start is not None:
            content = self._strip_sequence_columns(content)

        # Step 1: Strip fixed-column comments (e.g., COBOL column 7)
        if self._spec.comments.fixed_column is not None:
            content = self._strip_fixed_column_comments(content)

        # Step 2: Strip line comments (e.g., // or #)
        if self._spec.comments.line_prefix:
            content = self._strip_line_comments(content)

        # Step 3: Strip block comments (e.g., /* */)
        if self._spec.comments.block_start and self._spec.comments.block_end:
            content = self._strip_block_comments(content)

        # Step 4: Mask string literals
        content, masked_strings = self._mask_strings(content)

        # Step 5: Join continued lines (if applicable)
        if self._spec.continuation:
            content, line_map = self._join_continuations(content)

        return PreprocessedSource(
            cleaned=content,
            original=original,
            line_map=line_map,
            masked_strings=masked_strings,
        )

    def _strip_line_comments(self, content: str) -> str:
        """Remove line comments (// or # style)."""
        prefix = self._spec.comments.line_prefix
        if not prefix:
            return content

        # Escape special regex characters in the prefix
        escaped_prefix = re.escape(prefix)

        # Match the comment prefix and everything after it on the line
        # But be careful not to match inside strings
        lines = content.splitlines(keepends=True)
        result = []

        for line in lines:
            # Find the comment start, being careful about strings
            in_string = False
            string_char = None
            i = 0
            comment_start = -1

            while i < len(line):
                char = line[i]

                if not in_string:
                    # Check for string start
                    for delim in self._spec.strings.delimiters:
                        if line[i:].startswith(delim):
                            # Check for triple-quoted strings
                            if self._spec.strings.triple_quoted and line[i:].startswith(
                                delim * 3
                            ):
                                in_string = True
                                string_char = delim * 3
                                i += 3
                                break
                            else:
                                in_string = True
                                string_char = delim
                                i += len(delim)
                            break
                    else:
                        # Check for comment start
                        if line[i:].startswith(prefix):
                            comment_start = i
                            break
                        i += 1
                else:
                    # Inside a string, look for end
                    escape = self._spec.strings.escape_char
                    if escape and char == escape and i + 1 < len(line):
                        # Skip escaped character
                        i += 2
                    elif string_char and line[i:].startswith(string_char):
                        in_string = False
                        string_char = None
                        i += len(string_char) if string_char else 1
                    else:
                        i += 1

            if comment_start >= 0:
                result.append(line[:comment_start] + ("\n" if line.endswith("\n") else ""))
            else:
                result.append(line)

        return "".join(result)

    def _strip_block_comments(self, content: str) -> str:
        """Remove block comments (/* */ style)."""
        block_start = self._spec.comments.block_start
        block_end = self._spec.comments.block_end

        if not block_start or not block_end:
            return content

        # Escape special regex characters
        escaped_start = re.escape(block_start)
        escaped_end = re.escape(block_end)

        # Pattern to match block comments, including nested newlines
        # Use non-greedy match to handle multiple block comments
        pattern = rf"{escaped_start}.*?{escaped_end}"
        return re.sub(pattern, "", content, flags=re.DOTALL)

    def _strip_sequence_columns(self, content: str) -> str:
        """
        Strip sequence number columns from fixed-format source.

        In COBOL fixed format, columns 1-6 contain sequence numbers
        that should not be parsed as code. This removes those columns
        from each line.

        Args:
            content: Source content to process.

        Returns:
            Content with sequence columns removed from each line.
        """
        start = self._spec.comments.strip_columns_start
        end = self._spec.comments.strip_columns_end

        if start is None:
            return content

        # Default end to start if not specified
        if end is None:
            end = start

        # Convert to 0-indexed
        start_idx = start - 1
        end_idx = end  # end is inclusive, so we use it directly as the slice end

        lines = content.splitlines(keepends=True)
        result = []

        for line in lines:
            if len(line) >= end_idx:
                # Remove the specified columns
                # Keep everything after the stripped columns
                new_line = line[end_idx:]
                result.append(new_line)
            else:
                # Line is too short - keep as-is
                result.append(line)

        return "".join(result)

    def _strip_fixed_column_comments(self, content: str) -> str:
        """
        Remove COBOL-style column-based comments.

        In COBOL fixed format, column 7 (1-indexed) contains an indicator:
        - '*' indicates a comment line
        - '/' indicates a comment line (page eject)
        - 'D' or 'd' indicates a debugging line (treated as comment)

        The specified indicator from the spec is used, plus standard
        COBOL comment indicators.

        Args:
            content: Source content to process.

        Returns:
            Content with fixed-column comment lines blanked out
            (preserving line count for accurate line mapping).
        """
        column = self._spec.comments.fixed_column
        indicator = self._spec.comments.fixed_indicator

        if column is None:
            return content

        # Column is 1-indexed, so subtract 1 for 0-indexed access
        col_index = column - 1

        # Build set of comment indicators
        # Include the configured indicator plus standard COBOL ones
        comment_indicators: set[str] = set()
        if indicator:
            comment_indicators.add(indicator)
        # Standard COBOL comment indicators for column 7
        comment_indicators.update(["*", "/", "D", "d"])

        lines = content.splitlines(keepends=True)
        result = []

        for line in lines:
            # Check if the line is long enough and has a comment indicator
            if len(line) > col_index and line[col_index] in comment_indicators:
                # Comment line - replace with empty or preserve newline
                if line.endswith("\n"):
                    result.append("\n")
                else:
                    result.append("")
            else:
                result.append(line)

        return "".join(result)

    def _mask_strings(self, content: str) -> tuple[str, list[tuple[int, int, str]]]:
        """
        Replace string literals with unique placeholders.

        This prevents patterns from matching content inside strings,
        which could lead to false positives (e.g., matching a table name
        that appears in a string literal).

        Placeholders use the format __STR_0__, __STR_1__, etc.

        Args:
            content: Source content to process.

        Returns:
            Tuple of (masked_content, list of (start, end, original) tuples)
        """
        masked_strings: list[tuple[int, int, str]] = []
        result: list[str] = []
        i = 0
        delimiters = self._spec.strings.delimiters
        escape_char = self._spec.strings.escape_char
        triple_quoted = self._spec.strings.triple_quoted

        if not delimiters:
            return content, masked_strings

        while i < len(content):
            found_string = False

            for delim in delimiters:
                # Check for triple-quoted strings first if supported (Python-style)
                triple_delim = delim * 3
                if triple_quoted and content[i:].startswith(triple_delim):
                    start_pos = i
                    i += 3  # Skip opening delimiter

                    # Find closing triple delimiter
                    end_idx = content.find(triple_delim, i)
                    if end_idx >= 0:
                        i = end_idx + 3
                    else:
                        # Unclosed string - consume rest of content
                        i = len(content)

                    original = content[start_pos:i]
                    placeholder = self._make_string_placeholder()
                    masked_strings.append((start_pos, i, original))
                    result.append(placeholder)
                    found_string = True
                    break

                elif content[i:].startswith(delim):
                    # Single or double quoted string
                    start_pos = i
                    i += len(delim)  # Skip opening delimiter

                    while i < len(content):
                        char = content[i]

                        # Check for escape sequence
                        if escape_char and char == escape_char and i + 1 < len(content):
                            # Skip escape char and the next char
                            i += 2
                            continue

                        # Check for doubled delimiter as escape (e.g., '' in SQL/COBOL)
                        if not escape_char and content[i:].startswith(delim + delim):
                            # Skip both delimiters
                            i += 2 * len(delim)
                            continue

                        # Check for closing delimiter
                        if content[i:].startswith(delim):
                            i += len(delim)
                            break

                        # Newline typically ends strings (except in some languages)
                        # We don't break on newline to handle multiline strings
                        i += 1
                    else:
                        # Reached end of content without closing delimiter
                        pass

                    original = content[start_pos:i]
                    placeholder = self._make_string_placeholder()
                    masked_strings.append((start_pos, i, original))
                    result.append(placeholder)
                    found_string = True
                    break

            if not found_string:
                result.append(content[i])
                i += 1

        return "".join(result), masked_strings

    def _make_string_placeholder(self) -> str:
        """
        Generate a unique placeholder for a masked string.

        Returns:
            A unique placeholder string like "__STR_0__", "__STR_1__", etc.
        """
        placeholder = (
            f"{self.STRING_PLACEHOLDER_PREFIX}"
            f"{self._string_counter}"
            f"{self.STRING_PLACEHOLDER_SUFFIX}"
        )
        self._string_counter += 1
        return placeholder

    def _join_continuations(self, content: str) -> tuple[str, dict[int, int]]:
        """
        Join continued lines into single logical lines.

        Handles two styles of continuation:
        1. Trailing character (e.g., backslash at end of line)
        2. Leading character in specific column (e.g., COBOL column 7 hyphen '-')

        For COBOL fixed format, a '-' in column 7 indicates that the
        current line continues the previous line.

        Args:
            content: Source content to process.

        Returns:
            Tuple of (joined_content, line_number_mapping)
            The line_map maps cleaned line numbers to original line numbers.
        """
        continuation = self._spec.continuation
        if not continuation:
            # Identity mapping
            lines = content.splitlines()
            return content, {i + 1: i + 1 for i in range(len(lines))}

        lines = content.splitlines()
        result_lines: list[str] = []
        line_map: dict[int, int] = {}
        current_line = ""
        original_line_start: int | None = None

        for i, line in enumerate(lines):
            original_line_num = i + 1

            # Check for trailing continuation character (e.g., backslash)
            if (
                continuation.trailing_char
                and line.rstrip().endswith(continuation.trailing_char)
            ):
                if original_line_start is None:
                    original_line_start = original_line_num
                # Remove the trailing continuation char and append
                current_line += line.rstrip()[: -len(continuation.trailing_char)]
                continue

            # Check for leading continuation character at specific column (COBOL-style)
            if continuation.leading_char and continuation.leading_column is not None:
                col_idx = continuation.leading_column - 1
                if len(line) > col_idx and line[col_idx] == continuation.leading_char:
                    # This is a continuation of the previous line
                    # In COBOL, continuation starts at column 12 (Area B starts at 12)
                    # The content after column 7 is appended to the previous line
                    continuation_text = line[col_idx + 1 :]
                    if original_line_start is None:
                        original_line_start = original_line_num
                    current_line += continuation_text
                    continue

            # Not a continuation line - flush current accumulated line
            if current_line:
                # Append the current (non-continuation) line to accumulated content
                result_lines.append(current_line + line)
                line_map[len(result_lines)] = (
                    original_line_start if original_line_start else original_line_num
                )
                current_line = ""
                original_line_start = None
            else:
                result_lines.append(line)
                line_map[len(result_lines)] = original_line_num

        # Don't forget the last accumulated line
        if current_line:
            result_lines.append(current_line)
            line_map[len(result_lines)] = (
                original_line_start if original_line_start else len(lines)
            )

        return "\n".join(result_lines), line_map

    def unmask_strings(
        self, content: str, masked_strings: list[tuple[int, int, str]]
    ) -> str:
        """
        Restore original strings from placeholders.

        This is useful for generating accurate error messages that include
        the original string content.

        Args:
            content: Content with placeholders.
            masked_strings: List of (start, end, original) from masking.

        Returns:
            Content with original strings restored.
        """
        result = content
        for i, (_, _, original) in enumerate(masked_strings):
            placeholder = (
                f"{self.STRING_PLACEHOLDER_PREFIX}{i}{self.STRING_PLACEHOLDER_SUFFIX}"
            )
            result = result.replace(placeholder, original, 1)
        return result

    def get_original_line(self, cleaned_line: int, line_map: dict[int, int]) -> int:
        """
        Map a cleaned line number back to the original line number.

        Args:
            cleaned_line: Line number in the cleaned content.
            line_map: The line mapping from preprocessing.

        Returns:
            The corresponding line number in the original content.
        """
        return line_map.get(cleaned_line, cleaned_line)
