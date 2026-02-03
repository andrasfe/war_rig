# CodeWhisper Architecture

## Overview

CodeWhisper is an interactive chatbot CLI for exploring mainframe codebases. It uses LangGraph for agent orchestration and a skills-based knowledge system to provide contextual, accurate answers about legacy code.

## System Context

```
+------------------+     +-----------------+     +------------------+
|     User         |     |   CodeWhisper   |     |   Resources      |
|                  |     |                 |     |                  |
|  CLI Terminal    +---->+  LangGraph      +---->+  Skills (docs)   |
|                  |     |  Agent          |     |  Source Code     |
|  Questions       |     |                 |     |  LLM API         |
+------------------+     +-----------------+     +------------------+
```

## Design Goals

1. **Conversational**: Natural language interaction for code exploration
2. **Knowledge-Based**: Skills provide structured documentation the agent can access
3. **Tool-Augmented**: Agent uses tools to search and read code
4. **Extensible**: Easy to add new skills and capabilities
5. **Offline-Capable**: Skills work without internet (only LLM needs connectivity)

## Architecture Diagram

```
+------------------------------------------------------------------------+
|                              CodeWhisper                                |
+------------------------------------------------------------------------+
|                                                                        |
|  +------------------+     +-------------------+     +-----------------+ |
|  |      CLI         |     |      Agent        |     |    Skills       | |
|  |                  |     |                   |     |                 | |
|  | - typer/rich     |     | - LangGraph       |     | - Loader        | |
|  | - REPL loop      +---->+ - StateGraph      +<--->+ - Index         | |
|  | - Config         |     | - Tools           |     | - Search        | |
|  +------------------+     +-------------------+     +-----------------+ |
|                                   |                                    |
|                                   v                                    |
|                           +---------------+                            |
|                           |    Search     |                            |
|                           |               |                            |
|                           | - Code search |                            |
|                           | - File read   |                            |
|                           +---------------+                            |
|                                                                        |
+------------------------------------------------------------------------+
                                   |
                                   v
                          +----------------+
                          | llm-providers  |
                          |                |
                          | - OpenRouter   |
                          | - Anthropic    |
                          | - OpenAI       |
                          +----------------+
```

## Core Components

### CLI Layer (`cli.py`)

The CLI provides the user interface:

- **Interactive REPL**: Main interaction mode with rich formatting
- **Single Query Mode**: One-shot questions via `--query`
- **Configuration**: CLI args, env vars, and .env file support

```python
@app.command()
def main(
    skills_dir: Path,
    code_dir: Path,
    model: str,
    query: str | None,
):
    ...
```

### Agent Layer (`agent/`)

#### State (`state.py`)

The agent state accumulates information through the conversation:

```python
@dataclass
class AgentState:
    messages: Annotated[Sequence[Any], add_messages]  # Conversation
    loaded_skills: list[str]          # Currently loaded skills
    skill_contexts: list[SkillContext] # Full skill content
    search_results: list[CodeSearchResult]
    file_contents: list[FileContent]
    current_query: str
```

**Key Design Decisions:**

- Uses LangGraph's `add_messages` reducer for conversation accumulation
- Separates loaded skill names from full content for efficiency
- Tracks search results and file reads for context

#### Graph (`graph.py`)

The LangGraph StateGraph defines the agent's execution flow:

```
        +-------+
        | START |
        +---+---+
            |
            v
        +-------+
        | agent |<----+
        +---+---+     |
            |         |
    +-------+-------+ |
    |               | |
    v               v |
+-------+       +-------+
|  END  |       | tools |
+-------+       +---+---+
                    |
                    +-----+
```

**Nodes:**
- `agent`: LLM reasoning node - decides next action
- `tools`: Executes tool calls and returns results

**Conditional Logic:**
- If agent generates tool calls -> route to `tools` node
- If agent generates response -> route to `END`

#### Tools (`tools.py`)

Tools extend the agent's capabilities:

| Tool | Purpose |
|------|---------|
| `search_skills` | Find relevant skills by keyword |
| `load_skill` | Load full skill content into context |
| `search_code` | Find patterns in source files |
| `read_file` | Read source file contents |

Tools are defined using LangChain's `@tool` decorator with Pydantic schemas:

```python
class SkillSearchInput(BaseModel):
    query: str
    limit: int = 5

@tool(args_schema=SkillSearchInput)
def search_skills(query: str, limit: int = 5) -> str:
    ...
```

### Skills Layer (`skills/`)

#### Loader (`loader.py`)

Loads skill files from disk:

```python
class SkillsLoader:
    def __init__(self, skills_dir: Path): ...
    def load_all(self) -> list[Skill]: ...
    def iter_skills(self) -> Iterator[Skill]: ...
```

**Skill Format:**
```markdown
---
name: cbpaup0c
description: Batch cleanup of expired authorizations
---

# CBPAUP0C

Markdown content...
```

#### Index (`index.py`)

Provides search over loaded skills:

```python
class SkillsIndex:
    def search(self, query: str, limit: int = 5) -> list[SearchResult]: ...
    def get(self, name: str) -> Skill | None: ...
```

**Search Implementation:**
- Builds inverted index on load
- Simple keyword matching with weighted scoring
- Name matches weighted higher than content matches

### Search Layer (`search/`)

#### Code Search (`code_search.py`)

Pattern-based search across source files:

```python
class CodeSearcher:
    def search(
        self,
        pattern: str,        # Regex
        file_pattern: str,   # Glob
        context_lines: int,
    ) -> list[CodeSearchResult]: ...
```

**Implementation:**
- Uses ripgrep when available (fast)
- Falls back to pure Python (portable)
- Returns matches with context lines

## Data Flow

### Query Processing

```
1. User enters question in CLI
   |
   v
2. CLI creates initial AgentState with user message
   |
   v
3. Agent node processes state:
   - Examines query and loaded context
   - Decides: respond directly or use tools?
   |
   v
4a. If tools needed:
   - Agent generates tool calls
   - Tools node executes calls
   - Results added to state
   - Loop back to agent
   |
4b. If ready to respond:
   - Agent generates response
   - Response returned to CLI
   |
   v
5. CLI displays formatted response
```

### Skill Loading Flow

```
1. Agent decides skill needed (via search_skills result)
   |
   v
2. Agent calls load_skill(name)
   |
   v
3. Skills index retrieves full skill content
   |
   v
4. Skill content added to state.skill_contexts
   |
   v
5. Agent can now reference skill content in responses
```

## Configuration

### Hierarchical Configuration

```
CLI Arguments (highest priority)
         |
         v
Environment Variables (CODEWHISPER_*)
         |
         v
.env File
         |
         v
Default Values (lowest priority)
```

### Key Configuration Options

| Option | Purpose | Default |
|--------|---------|---------|
| `skills_dir` | Path to skills | `./skills` |
| `code_dir` | Path to source code | `.` |
| `model` | LLM model identifier | `anthropic/claude-sonnet-4-20250514` |
| `provider` | LLM provider | `openrouter` |
| `max_history` | Conversation turns to keep | `20` |

## Extension Points

### Adding New Tools

1. Define input schema as Pydantic model
2. Create tool function with `@tool` decorator
3. Add to `get_all_tools()` return list

### Adding New Skills

1. Create `SKILL.md` file with frontmatter
2. Place in skills directory
3. Skills are loaded automatically

### Custom Providers

Use llm-providers package's plugin system or factory.

## Error Handling

### Tool Errors

- Tools return error messages as strings
- Agent can reason about errors and try alternatives
- Example: "File not found: xyz.cbl"

### LLM Errors

- Timeout: Long prompts may take time
- Rate limits: Handled by llm-providers
- Connection issues: Displayed to user

### Graceful Degradation

- Missing skills directory: Error at startup
- Missing code directory: Error at startup
- LLM unavailable: Cannot process queries (displayed)

## Future Considerations

1. **Semantic Search**: Vector-based skill search
2. **Memory**: Persistent conversation history
3. **Multi-turn Context**: Better context window management
4. **Code Understanding**: AST-based code analysis
5. **Interactive Debugging**: Step through code explanations
6. **Export**: Save conversations as documentation

## Security Notes

- File reading restricted to code_dir (path traversal prevention)
- API keys managed via environment variables
- No code execution - only reading and searching
- Skills are trusted input (from documentation system)
