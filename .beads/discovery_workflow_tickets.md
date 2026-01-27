# Missing Program Discovery Workflow - Implementation Tickets

**Created**: 2026-01-22
**Feature**: Missing Program Discovery Workflow
**Total Tickets**: 6

## Executive Summary

When call graph analysis finds that a documented program calls an undocumented program (XYZ), a "discovery" ticket is created. Currently these tickets get stuck because Scribe doesn't know how to search for and document missing programs. This feature enables Scribe to intelligently search for programs, document them as internal routines if found within another file, or document them as external/missing with proper metadata. The Challenger gains validation capabilities for discovery tickets, and the call graph gets enhanced status indicators.

## Dependency Graph

```
[Ticket 1: Scribe Discovery Search] ----+
                                        |
[Ticket 2: Symbol Documentation]  ------+---> [Ticket 5: Challenger Discovery Validation]
                                        |
[Ticket 3: External/Missing Docs] ------+
                                        |
[Ticket 4: System Utility Classification] ---> [Ticket 6: Call Graph Status Indicators]
```

- Tickets 1-4 can be developed in parallel (independent Scribe enhancements)
- Ticket 5 depends on Tickets 1-3 (Challenger needs to validate what Scribe produces)
- Ticket 6 depends on Ticket 4 (needs system utility classification data)

---

## Ticket 1: Scribe Discovery Search

**ID**: DISC-001
**Type**: Feature
**Priority**: High (1)
**Estimated Effort**: Medium
**Dependencies**: None

### Summary
Add discovery ticket handling to ScribeWorker that searches for missing programs in the input directory and source files.

### Files to Modify
- `/home/andras/war_rig/war_rig/workers/scribe_pool.py`

### Context
When call graph analysis identifies a missing program (e.g., SUBPROG1), the ticket engine creates a DOCUMENTATION ticket with `metadata.discovery=True`. The Scribe worker currently doesn't know how to handle these tickets because the file doesn't exist at the expected path. We need Scribe to actively search for the program.

### Requirements

1. **Add `_handle_discovery_ticket()` method** (around line 400, after `_process_ticket()`):
   ```python
   async def _handle_discovery_ticket(self, ticket: ProgramManagerTicket) -> ScribeOutput:
       """Handle a discovery ticket by searching for the missing program.

       Discovery tickets are created when call graph analysis finds a program
       that is called but not documented. This method searches for the program
       in multiple ways:
       1. Direct file match in input directory
       2. Symbol search across all source files

       Args:
           ticket: Discovery ticket with metadata.discovery=True

       Returns:
           ScribeOutput with documentation or search results
       """
   ```

2. **Modify `_process_ticket()` method** to detect discovery tickets:
   - Check if `ticket.metadata.get("discovery") == True`
   - If discovery ticket, call `_handle_discovery_ticket()` instead of normal processing
   - Add early detection before loading source code (which would fail)

3. **File search logic in `_handle_discovery_ticket()`**:
   - Search `input_directory` (non-recursive) for files matching program_id
   - File extensions to check: `*.cbl`, `*.cob`, `*.prc`, `*.jcl`, `*.asm`
   - Match patterns: exact match (PROGNAME.cbl), prefix match (PROGNAME*.cbl)
   - If found, process as normal documentation ticket by calling `_process_documentation_ticket()`

4. **Add `_search_symbol_in_sources()` method**:
   ```python
   def _search_symbol_in_sources(
       self,
       program_id: str,
       source_extensions: list[str] = None
   ) -> list[tuple[Path, int, str]]:
       """Search all source files for references to a program symbol.

       Searches for patterns like:
       - CALL 'PROGNAME' or CALL "PROGNAME"
       - CALL PROGNAME (without quotes)
       - PERFORM PROGNAME
       - EXEC CICS LINK PROGRAM('PROGNAME')
       - EXEC CICS LINK PROGRAM(PROGNAME)

       Args:
           program_id: The program/symbol name to search for
           source_extensions: File extensions to search (default: cbl, cob, prc, jcl)

       Returns:
           List of (file_path, line_number, matching_line) tuples
       """
   ```

5. **Search patterns** (regex):
   ```python
   patterns = [
       rf"CALL\s+['\"]?{re.escape(program_id)}['\"]?",
       rf"PERFORM\s+{re.escape(program_id)}(?:\s|\.)",
       rf"EXEC\s+CICS\s+LINK\s+PROGRAM\s*\(\s*['\"]?{re.escape(program_id)}['\"]?\s*\)",
       rf"EXEC\s+CICS\s+XCTL\s+PROGRAM\s*\(\s*['\"]?{re.escape(program_id)}['\"]?\s*\)",
   ]
   ```

6. **Decision routing based on search results**:
   - If file found: Process as normal documentation
   - If symbol found in another file: Call `_document_as_internal_routine()` (Ticket 2)
   - If not found anywhere: Call `_document_missing_program()` (Ticket 3)

### Acceptance Criteria
- [ ] Discovery tickets (metadata.discovery=True) are detected and handled specially
- [ ] File search covers all expected extensions in input_directory
- [ ] Symbol search finds CALL, PERFORM, and CICS LINK/XCTL patterns
- [ ] Search results are logged for debugging
- [ ] Method gracefully handles empty directories and permission errors
- [ ] Unit tests cover file search and symbol search scenarios

### Technical Notes
- Use `Path.glob()` for file search (non-recursive by default)
- Case-insensitive matching for program names (COBOL is case-insensitive)
- Limit symbol search to files < 1MB to avoid memory issues
- Consider caching file list if multiple discovery tickets in same batch

---

## Ticket 2: Symbol Documentation in Parent File

**ID**: DISC-002
**Type**: Feature
**Priority**: High (1)
**Estimated Effort**: Medium
**Dependencies**: Ticket 1

### Summary
When a searched symbol is found as a paragraph/section in another file (not as a separate program), document it as an internal routine within that parent file's documentation.

### Files to Modify
- `/home/andras/war_rig/war_rig/workers/scribe_pool.py`
- `/home/andras/war_rig/war_rig/models/templates.py`

### Context
Some "missing programs" are actually internal paragraphs or sections within a larger COBOL program. For example, SUBPROG1 might be a PERFORM target within MAINPROG.cbl. These shouldn't be documented as separate programs but should be noted in the parent program's documentation.

### Requirements

#### Part A: Template Enhancement (`templates.py`)

1. **Add `InternalRoutine` model** (after `Paragraph` class, around line 405):
   ```python
   class InternalRoutine(BaseModel):
       """Description of an internal routine discovered during call graph analysis.

       Used when a 'missing program' is found to be a paragraph/section
       within another program rather than a separate program file.
       """

       name: str = Field(description="Routine name (paragraph/section name)")
       routine_type: str = Field(
           default="paragraph",
           description="Type: 'paragraph', 'section', or 'entry_point'"
       )
       parent_file: str = Field(description="File where this routine is defined")
       line_number: int | None = Field(default=None, description="Line where routine starts")
       brief_description: str | None = Field(
           default=None,
           description="Brief description of what this routine does"
       )
       discovered_from: list[str] = Field(
           default_factory=list,
           description="Programs that call/reference this routine"
       )
       citation: int | None = Field(default=None, description="Line number in parent")
   ```

2. **Add `internal_routines` field to `DocumentationTemplate`** (around line 545):
   ```python
   internal_routines: list[InternalRoutine] = Field(
       default_factory=list,
       description="Internal routines discovered as PERFORM targets (not separate programs)",
   )
   ```

#### Part B: Scribe Worker Enhancement (`scribe_pool.py`)

3. **Add `_document_as_internal_routine()` method**:
   ```python
   async def _document_as_internal_routine(
       self,
       ticket: ProgramManagerTicket,
       parent_file: Path,
       line_number: int,
       context: str,
   ) -> ScribeOutput:
       """Document a symbol as an internal routine within a parent file.

       When a 'missing program' is found as a paragraph/section in another
       file, we update that file's documentation to include it as an
       internal routine rather than creating separate documentation.

       Args:
           ticket: The discovery ticket for the missing program
           parent_file: Path to the file containing the routine
           line_number: Line number where the routine is defined/called
           context: The matching line of code for context

       Returns:
           ScribeOutput indicating success and what was documented
       """
   ```

4. **Implementation steps for `_document_as_internal_routine()`**:
   - Load parent file's existing `.doc.json` using `_load_previous_template()`
   - If no existing documentation, return error (parent should be documented first)
   - Create `InternalRoutine` entry with:
     - `name`: ticket.program_id
     - `routine_type`: Infer from context (PERFORM = paragraph, SECTION header = section)
     - `parent_file`: parent_file.name
     - `line_number`: line_number
     - `discovered_from`: Extract from ticket metadata (which file(s) call this)
   - Append to `internal_routines` list (avoid duplicates by name)
   - Save updated documentation using `_save_template()`
   - Mark discovery ticket as COMPLETED with reason:
     `f"Documented as internal routine '{ticket.program_id}' in {parent_file.name}"`

5. **Duplicate detection logic**:
   ```python
   # Check if already documented
   existing_names = {r.name.upper() for r in template.internal_routines}
   if ticket.program_id.upper() in existing_names:
       logger.info(f"Internal routine {ticket.program_id} already documented in {parent_file}")
       return ScribeOutput(
           success=True,
           template=template,
           open_questions=[f"Routine {ticket.program_id} already documented"]
       )
   ```

### Acceptance Criteria
- [ ] `InternalRoutine` model added to templates.py with all specified fields
- [ ] `internal_routines` field added to DocumentationTemplate
- [ ] Parent file's documentation is loaded and updated (not overwritten)
- [ ] Duplicate routines are detected and skipped
- [ ] Discovery ticket marked COMPLETED with descriptive reason
- [ ] Unit tests for template updates and duplicate detection
- [ ] Backward compatible with existing .doc.json files (field is optional)

### Technical Notes
- Use `DocumentationTemplate.load_lenient()` to handle schema variations
- The parent file path comes from `_search_symbol_in_sources()` results
- Consider extracting brief description from surrounding comments in source
- Log changes for audit trail

---

## Ticket 3: External/Missing Program Documentation

**ID**: DISC-003
**Type**: Feature
**Priority**: High (1)
**Estimated Effort**: Medium
**Dependencies**: Ticket 1

### Summary
When a program cannot be found as a file or symbol, create documentation marking it as external or missing with metadata about the search performed and likely explanations.

### Files to Modify
- `/home/andras/war_rig/war_rig/workers/scribe_pool.py`

### Context
Some programs referenced in call graphs truly don't exist in the codebase. They might be external utilities, third-party libraries, or genuinely missing source code. We need to document these cases properly rather than leaving tickets stuck.

### Requirements

1. **Add `_document_missing_program()` method**:
   ```python
   async def _document_missing_program(
       self,
       ticket: ProgramManagerTicket,
       search_results: dict[str, Any],
   ) -> ScribeOutput:
       """Document a program as external or missing.

       Creates a documentation entry for a program that couldn't be found
       in the codebase, including metadata about the search performed
       and likely explanations based on naming patterns.

       Args:
           ticket: The discovery ticket
           search_results: Dict with keys:
               - 'file_paths_checked': List of paths searched
               - 'patterns_searched': List of regex patterns used
               - 'source_files_scanned': Count of files scanned for symbol

       Returns:
           ScribeOutput with external/missing documentation
       """
   ```

2. **Create documentation template** with special fields:
   ```python
   template = DocumentationTemplate(
       header=HeaderSection(
           program_id=ticket.program_id,
           file_name=f"{ticket.program_id}.EXTERNAL",  # Special marker
           file_type=FileType.OTHER,
           analyzed_by="WAR_RIG_DISCOVERY",
           analyzed_at=datetime.utcnow(),
       ),
       purpose=PurposeSection(
           summary=f"External/missing program: {ticket.program_id}",
           business_context=self._infer_business_context(ticket.program_id),
           program_type=None,  # Unknown
       ),
   )
   ```

3. **Add external program metadata** to template (via `metadata` field or new fields):
   ```python
   # Add to template.header or as separate metadata
   external_metadata = {
       "is_external": True,
       "status": "external" | "missing",  # external if likely utility, missing if unknown
       "called_from": self._get_callers(ticket),
       "search_performed": {
           "file_extensions_checked": [".cbl", ".cob", ".prc", ".jcl", ".asm"],
           "paths_searched": search_results["file_paths_checked"],
           "patterns_used": search_results["patterns_searched"],
           "source_files_scanned": search_results["source_files_scanned"],
           "searched_at": datetime.utcnow().isoformat(),
       },
       "likely_explanation": self._infer_explanation(ticket.program_id),
       "recommended_action": self._recommend_action(ticket.program_id),
   }
   ```

4. **Add `_infer_explanation()` helper method**:
   ```python
   def _infer_explanation(self, program_id: str) -> str:
       """Infer likely explanation based on program naming patterns."""
       upper_id = program_id.upper()

       # System utilities (handled by Ticket 4, but provide explanation)
       if any(upper_id.startswith(p) for p in ["IEF", "IEB", "IDC", "DFH", "DSN", "CEE"]):
           return "IBM system utility - standard mainframe component"

       # Common patterns
       if upper_id.startswith("UT") or "UTIL" in upper_id:
           return "Likely a utility program - may be in a shared library"
       if upper_id.endswith("X") or upper_id.endswith("Z"):
           return "Naming suggests exit routine or extension module"

       return "Source not found in provided codebase - may be external dependency or missing"
   ```

5. **Add `_recommend_action()` helper method**:
   ```python
   def _recommend_action(self, program_id: str) -> str:
       """Recommend next steps for the missing program."""
       upper_id = program_id.upper()

       if self._is_likely_system_utility(upper_id):
           return "Add to system utilities list if this is a standard IBM utility"
       if "DB2" in upper_id or upper_id.startswith("DSN"):
           return "Verify DB2 configuration - may be stored procedure or trigger"

       return "Locate source from production library or verify with SME"
   ```

6. **Add `_get_callers()` helper**:
   ```python
   def _get_callers(self, ticket: ProgramManagerTicket) -> list[str]:
       """Get list of programs that call this missing program."""
       # Extract from ticket metadata (set by call_graph analysis)
       return ticket.metadata.get("called_from", [])
   ```

7. **Save as special documentation file**:
   - File name: `{program_id}.doc.json` (same as normal docs)
   - Include `is_external: true` flag at top level for easy identification
   - Store in same output directory as other documentation

### Acceptance Criteria
- [ ] Missing programs get documented with is_external=true flag
- [ ] Search metadata is comprehensive (paths, patterns, file counts)
- [ ] Explanation inference covers common patterns (system utils, naming conventions)
- [ ] Recommended actions are actionable and relevant
- [ ] called_from list correctly populated from ticket metadata
- [ ] Documentation file is valid JSON and loadable by other tools
- [ ] Unit tests for inference logic and edge cases

### Technical Notes
- The `called_from` information comes from call_graph analysis (set in ticket metadata)
- Keep explanations concise but informative
- Consider making patterns configurable for different mainframe environments

---

## Ticket 4: System Utility Auto-Classification

**ID**: DISC-004
**Type**: Feature
**Priority**: Medium (2)
**Estimated Effort**: Medium
**Dependencies**: None

### Summary
Automatically classify IBM system utilities by naming pattern and update the call graph analysis to track them separately from custom missing programs.

### Files to Modify
- `/home/andras/war_rig/war_rig/analysis/call_graph.py`

### Context
The call_graph module already has a `SYSTEM_UTILITIES` frozenset with specific program names. However, many system utilities follow naming patterns (IEF*, DFH*, etc.) that aren't explicitly listed. We need pattern-based detection to reduce false positives in the "missing programs" list.

### Requirements

1. **Add `SYSTEM_UTILITY_PATTERNS` constant** (after `SYSTEM_UTILITIES`, around line 85):
   ```python
   # Pattern prefixes that indicate system utilities
   SYSTEM_UTILITY_PATTERNS: tuple[str, ...] = (
       # IBM standard utilities
       "IEF",      # Job Entry Facility
       "IEB",      # Data set utilities
       "IEH",      # Catalog utilities
       "IDC",      # Access Method Services (IDCAMS)
       "AMS",      # Access Method Services
       "DFS",      # IMS
       "IGY",      # COBOL compiler/runtime
       "IGZ",      # COBOL runtime
       "CEE",      # Language Environment
       "EDC",      # C/C++ runtime
       # CICS
       "DFH",      # CICS programs
       "EYU",      # CICS utilities
       # DB2
       "DSN",      # DB2 programs
       "DSQ",      # QMF
       # MQ
       "CSQ",      # MQ Series
       "AMQ",      # MQ (alternate prefix)
       # Other IBM
       "ERB",      # RMF
       "EZA",      # TCP/IP
       "FTP",      # FTP utilities
       "GIM",      # SMP/E
       "HZS",      # Health Checker
       "IKJ",      # TSO
       "ISP",      # ISPF
       "ISR",      # ISPF
       # Compilers and Language processors
       "ASM",      # Assembler
       "IEV",      # Assembler (old)
       "PLI",      # PL/I
       "IBM",      # Generic IBM prefix
   )
   ```

2. **Add `_detect_system_utility()` method to `CallGraphAnalyzer`**:
   ```python
   def _detect_system_utility(self, program_name: str) -> bool:
       """Detect if a program name matches system utility patterns.

       Checks both the explicit SYSTEM_UTILITIES set and naming patterns.

       Args:
           program_name: Program name to check

       Returns:
           True if program is a system utility
       """
       upper_name = program_name.upper()

       # Check explicit list first
       if upper_name in self.system_utilities:
           return True

       # Check pattern prefixes
       for pattern in SYSTEM_UTILITY_PATTERNS:
           if upper_name.startswith(pattern):
               logger.debug(f"Classified {program_name} as system utility (pattern: {pattern}*)")
               return True

       return False
   ```

3. **Update `analyze()` method** to use pattern detection (around line 185):
   ```python
   # Classify external dependencies (BEFORE existing classification)
   # Use pattern detection for more comprehensive classification
   for dep in external_deps:
       if self._detect_system_utility(dep):
           system_utils.add(dep)
       else:
           custom_missing.add(dep)

   # Replace the simple set intersection
   # OLD: system_utils = external_deps & self.system_utilities
   # OLD: custom_missing = external_deps - system_utils
   ```

4. **Add `auto_classified_utilities` to `CallGraphAnalysis`** dataclass:
   ```python
   @dataclass
   class CallGraphAnalysis:
       """Results of call graph analysis."""
       documented_programs: dict[str, ProgramInfo]
       external_dependencies: set[str]
       system_utilities: set[str]
       auto_classified_utilities: set[str]  # NEW: Utilities detected by pattern
       custom_missing: set[str]
       # ... rest unchanged
   ```

5. **Track auto-classified separately**:
   ```python
   auto_classified = set()
   for dep in external_deps:
       if dep.upper() in self.system_utilities:
           system_utils.add(dep)  # Explicit list
       elif self._detect_system_utility(dep):
           system_utils.add(dep)
           auto_classified.add(dep)  # Pattern match
       else:
           custom_missing.add(dep)
   ```

6. **Update `generate_system_design_md()` to show auto-classified utilities** (around line 575):
   ```python
   # Add section for auto-classified utilities
   if analysis.auto_classified_utilities:
       lines.append("### Auto-Classified System Utilities")
       lines.append("")
       lines.append("The following programs were classified as system utilities based on naming patterns:")
       lines.append("")
       lines.append("| Program | Pattern Match | Classification |")
       lines.append("|---------|---------------|----------------|")
       for prog in sorted(analysis.auto_classified_utilities):
           pattern = self._get_matching_pattern(prog)
           lines.append(f"| `{prog}` | {pattern}* | IBM System Utility |")
       lines.append("")
   ```

7. **Add `_get_matching_pattern()` helper**:
   ```python
   def _get_matching_pattern(self, program_name: str) -> str:
       """Get the pattern that matched for a system utility."""
       upper_name = program_name.upper()
       for pattern in SYSTEM_UTILITY_PATTERNS:
           if upper_name.startswith(pattern):
               return pattern
       return "EXPLICIT"  # Was in explicit list
   ```

### Acceptance Criteria
- [ ] Pattern-based detection catches common IBM prefixes
- [ ] Explicit list still takes precedence
- [ ] Auto-classified utilities tracked separately for reporting
- [ ] SYSTEM_DESIGN.md shows auto-classified utilities with their pattern
- [ ] Logging shows classification decisions for debugging
- [ ] custom_missing count is reduced by pattern detection
- [ ] Unit tests for pattern matching edge cases

### Technical Notes
- Keep patterns as a tuple for performance (membership testing)
- Consider making patterns configurable via config file
- Log at DEBUG level to avoid noise in normal runs
- Patterns are case-insensitive (always uppercase internally)

---

## Ticket 5: Challenger Discovery Validation

**ID**: DISC-005
**Type**: Feature
**Priority**: Medium (2)
**Estimated Effort**: Medium
**Dependencies**: Tickets 1, 2, 3

### Summary
Enhance Challenger validation to properly validate discovery tickets, ensuring searches were thorough before approving closure.

### Files to Modify
- `/home/andras/war_rig/war_rig/workers/challenger_pool.py`

### Context
Discovery tickets need special validation because the Challenger must verify that the search was comprehensive before accepting that a program is truly missing. This prevents premature closure of tickets that could have been resolved with more thorough searching.

### Requirements

1. **Add `_validate_discovery_ticket()` method** to `ChallengerWorker`:
   ```python
   def _validate_discovery_ticket(
       self,
       ticket: ProgramManagerTicket,
       template: DocumentationTemplate | None,
   ) -> ValidationResult:
       """Validate a discovery ticket's search completeness.

       For discovery tickets (metadata.discovery=True), validates that:
       1. All expected file extensions were searched
       2. Symbol search patterns were applied
       3. If marked external/missing, the explanation is reasonable

       Args:
           ticket: The discovery ticket being validated
           template: The documentation template (may have is_external flag)

       Returns:
           ValidationResult with validation findings
       """
   ```

2. **Modify `_process_ticket()` to detect discovery tickets** (around line 380):
   ```python
   # Check for discovery ticket
   if ticket.metadata.get("discovery"):
       logger.info(f"Validating discovery ticket for {ticket.program_id}")
       result = self._validate_discovery_ticket(ticket, template)
       # ... handle result
   ```

3. **Validation checks in `_validate_discovery_ticket()`**:

   **Check 1: File extension coverage**:
   ```python
   expected_extensions = {".cbl", ".cob", ".prc", ".jcl", ".asm"}
   search_info = template_metadata.get("search_performed", {})
   searched_extensions = set(search_info.get("file_extensions_checked", []))

   missing_extensions = expected_extensions - searched_extensions
   if missing_extensions:
       issues.append(
           f"Search incomplete: did not check extensions {missing_extensions}"
       )
   ```

   **Check 2: Symbol search performed**:
   ```python
   patterns_used = search_info.get("patterns_used", [])
   required_patterns = ["CALL", "PERFORM"]  # At minimum

   if not any("CALL" in p.upper() for p in patterns_used):
       issues.append("Symbol search did not include CALL pattern")
   if not any("PERFORM" in p.upper() for p in patterns_used):
       issues.append("Symbol search did not include PERFORM pattern")
   ```

   **Check 3: Reasonable file scan count**:
   ```python
   files_scanned = search_info.get("source_files_scanned", 0)
   if files_scanned == 0:
       issues.append("No source files were scanned for symbol search")
   ```

   **Check 4: External/missing explanation quality**:
   ```python
   if template_metadata.get("is_external"):
       explanation = template_metadata.get("likely_explanation", "")
       if len(explanation) < 20:
           issues.append("External program explanation is too brief")

       recommended_action = template_metadata.get("recommended_action", "")
       if not recommended_action:
           issues.append("No recommended action provided for external program")
   ```

4. **Return appropriate validation result**:
   ```python
   if issues:
       return ValidationResult(
           success=True,
           is_valid=False,
           issues_found=issues,
           blocking_questions=[
               {
                   "question_id": f"DISC-VAL-{ticket.ticket_id[-8:]}",
                   "question": f"Discovery search incomplete: {'; '.join(issues)}",
                   "severity": "BLOCKING",
                   "section": "discovery",
               }
           ],
       )

   return ValidationResult(
       success=True,
       is_valid=True,
       issues_found=[],
   )
   ```

5. **Handle internal routine documentation validation**:
   ```python
   # If documented as internal routine, verify parent file exists
   if ticket.metadata.get("documented_as") == "internal_routine":
       parent_file = ticket.metadata.get("parent_file")
       if parent_file:
           parent_doc_path = self._get_doc_path(parent_file)
           if not parent_doc_path.exists():
               issues.append(f"Parent file {parent_file} not documented")
   ```

### Acceptance Criteria
- [ ] Discovery tickets are detected and routed to special validation
- [ ] File extension coverage is validated
- [ ] Symbol search patterns are verified
- [ ] External/missing explanations are checked for quality
- [ ] Internal routine documentation references are validated
- [ ] Validation failures create blocking questions
- [ ] Unit tests cover all validation scenarios

### Technical Notes
- The template metadata fields come from Ticket 3's implementation
- Validation should be forgiving of slight variations in pattern names
- Consider configurable extension list for different environments

---

## Ticket 6: Call Graph Status Indicators

**ID**: DISC-006
**Type**: Feature
**Priority**: Low (3)
**Estimated Effort**: Small
**Dependencies**: Ticket 4

### Summary
Enhance call graph visualization to distinguish between documented, internal routine, external, and missing programs using different node shapes and colors.

### Files to Modify
- `/home/andras/war_rig/war_rig/analysis/call_graph.py`

### Context
The call graph currently shows documented programs and missing programs with basic styling. After the discovery workflow, we have more nuanced statuses (internal routines, external programs, confirmed missing). The visualization should reflect these distinctions.

### Requirements

1. **Add `program_status` field to `CallGraphAnalysis`** dataclass:
   ```python
   @dataclass
   class CallGraphAnalysis:
       # ... existing fields ...
       program_status: dict[str, str] = field(default_factory=dict)
       # Status values: "documented", "internal_routine", "external", "missing"
   ```

2. **Populate `program_status` in `analyze()` method**:
   ```python
   # After building programs dict
   program_status = {}

   # Documented programs
   for prog_id in programs.keys():
       program_status[prog_id] = "documented"

   # Check for internal routines in documentation
   for doc in docs:
       internal_routines = doc.get("internal_routines", [])
       for routine in internal_routines:
           routine_name = routine.get("name", "").upper()
           if routine_name:
               program_status[routine_name] = "internal_routine"

   # Check for external programs (is_external flag)
   for doc in docs:
       if doc.get("is_external") or doc.get("header", {}).get("file_name", "").endswith(".EXTERNAL"):
           prog_id = self._get_program_id(doc)
           if doc.get("status") == "missing":
               program_status[prog_id] = "missing"
           else:
               program_status[prog_id] = "external"

   # Remaining custom_missing are truly missing
   for prog in custom_missing:
       if prog not in program_status:
           program_status[prog] = "missing"
   ```

3. **Update `_generate_mermaid_diagram()` to use status-based styling**:
   ```python
   def _generate_mermaid_diagram(self, analysis: CallGraphAnalysis) -> str:
       lines = ["```mermaid", "flowchart TD"]

       # Node shape mapping
       # documented: rectangle [name]
       # internal_routine: stadium ([name])
       # external: parallelogram [/name/]
       # missing: flag shape >name]

       def get_node_shape(prog: str, status: str) -> str:
           node_id = sanitize_id(prog)
           if status == "internal_routine":
               return f"    {node_id}([{prog}])"  # Stadium shape
           elif status == "external":
               return f"    {node_id}[/{prog}/]"  # Parallelogram
           elif status == "missing":
               return f"    {node_id}>{prog}]"    # Flag shape (asymmetric)
           else:  # documented
               # Keep existing entry point/leaf node distinction
               if prog in analysis.entry_points:
                   return f"    {node_id}([{prog}])"  # Stadium for entry
               elif prog in analysis.leaf_nodes:
                   return f"    {node_id}[/{prog}/]"  # Parallelogram for leaf
               else:
                   return f"    {node_id}[{prog}]"   # Rectangle

       # ... rest of diagram generation
   ```

4. **Update styling section**:
   ```python
   lines.append("")
   lines.append("    %% Styling by status")

   # Documented (green)
   documented = [sanitize_id(p) for p in analysis.documented_programs.keys()]
   if documented:
       lines.append("    classDef documented fill:#90EE90,stroke:#228B22")
       lines.append(f"    class {','.join(documented)} documented")

   # Internal routines (light blue)
   internal = [sanitize_id(p) for p, s in analysis.program_status.items() if s == "internal_routine"]
   if internal:
       lines.append("    classDef internal fill:#ADD8E6,stroke:#4682B4")
       lines.append(f"    class {','.join(internal)} internal")

   # External (yellow)
   external = [sanitize_id(p) for p, s in analysis.program_status.items() if s == "external"]
   if external:
       lines.append("    classDef external fill:#FFFFE0,stroke:#DAA520")
       lines.append(f"    class {','.join(external)} external")

   # Missing (red)
   missing = [sanitize_id(p) for p, s in analysis.program_status.items() if s == "missing"]
   if missing:
       lines.append("    classDef missing fill:#FFB6C1,stroke:#DC143C")
       lines.append(f"    class {','.join(missing)} missing")
   ```

5. **Add legend to diagram**:
   ```python
   lines.append("")
   lines.append("    %% Legend")
   lines.append("    subgraph Legend")
   lines.append("        L1[Documented]:::documented")
   lines.append("        L2([Internal Routine]):::internal")
   lines.append("        L3[/External/]:::external")
   lines.append("        L4>Missing]:::missing")
   lines.append("    end")
   ```

6. **Update `generate_markdown_report()` to show status summary**:
   ```python
   # Add status breakdown section
   lines.append("## Program Status Breakdown")
   lines.append("")
   lines.append("| Status | Count | Programs |")
   lines.append("|--------|-------|----------|")

   status_groups = {}
   for prog, status in analysis.program_status.items():
       status_groups.setdefault(status, []).append(prog)

   for status in ["documented", "internal_routine", "external", "missing"]:
       progs = status_groups.get(status, [])
       prog_list = ", ".join(sorted(progs)[:5])
       if len(progs) > 5:
           prog_list += f" (+{len(progs)-5} more)"
       status_label = status.replace("_", " ").title()
       lines.append(f"| {status_label} | {len(progs)} | {prog_list} |")
   ```

### Acceptance Criteria
- [ ] program_status dict populated during analysis
- [ ] Mermaid diagram uses distinct shapes for each status
- [ ] Color scheme distinguishes documented/internal/external/missing
- [ ] Legend included in diagram
- [ ] Markdown report includes status breakdown table
- [ ] Internal routines correctly identified from documentation
- [ ] Unit tests verify status assignment logic

### Technical Notes
- Mermaid shape reference: https://mermaid.js.org/syntax/flowchart.html
- Keep colors colorblind-friendly if possible
- Consider making status->shape mapping configurable
- Legend should be collapsible in larger diagrams

---

## Open Questions

1. **Parent file not documented**: What if a symbol is found in a parent file that hasn't been documented yet? Should we:
   - Queue documentation of parent first? (preferred)
   - Skip and log a warning?
   - Create stub documentation?

2. **Multiple matches**: What if a symbol is found in multiple files? Should we:
   - Document in all files?
   - Pick the most likely one (by call frequency)?
   - Ask for clarification?

3. **Dynamic calls**: How should we handle dynamic CALL statements where the program name comes from a variable? Currently these are skipped, but discovery might help.

4. **Cross-batch discovery**: If a discovery ticket can't be resolved in one batch, should it persist to the next batch or be marked as permanently missing?

5. **System utility verification**: Should we allow users to confirm/reject auto-classified system utilities? This could improve accuracy over time.

---

## Implementation Order Recommendation

1. **Phase 1 (Core Discovery)**: Tickets 1, 3, 4 (can be parallel)
   - Establishes basic search and classification infrastructure

2. **Phase 2 (Internal Routines)**: Ticket 2
   - Depends on Ticket 1's search implementation

3. **Phase 3 (Validation)**: Ticket 5
   - Depends on Tickets 1-3 being complete

4. **Phase 4 (Visualization)**: Ticket 6
   - Can be done last as it's purely reporting

Total estimated effort: ~2-3 weeks with focused development
