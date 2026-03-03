/*
 * Traverses a ProLeap ASG and emits a JSON AST matching the citadel contract.
 *
 * Output schema:
 * {
 *   "program_id": "PROG1",
 *   "paragraphs": [{
 *     "name": "MAIN-PARA",
 *     "line_start": 42, "line_end": 58,
 *     "statements": [{
 *       "type": "IF",
 *       "text": "IF WS-STATUS = 'ACTIVE'",
 *       "line_start": 43, "line_end": 43,
 *       "children": [...],
 *       "attributes": {"condition": "WS-STATUS = 'ACTIVE'"}
 *     }]
 *   }]
 * }
 */
package citadel.proleap;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.Scope;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.metamodel.procedure.Section;
import io.proleap.cobol.asg.metamodel.procedure.Statement;
import io.proleap.cobol.asg.metamodel.procedure.StatementTypeEnum;
import io.proleap.cobol.asg.metamodel.procedure.call.CallStatement;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.EvaluateStatement;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.WhenOther;
import io.proleap.cobol.asg.metamodel.procedure.evaluate.WhenPhrase;
import io.proleap.cobol.asg.metamodel.procedure.execcics.ExecCicsStatement;
import io.proleap.cobol.asg.metamodel.procedure.execsql.ExecSqlStatement;
import io.proleap.cobol.asg.metamodel.procedure.execsqlims.ExecSqlImsStatement;
import io.proleap.cobol.asg.metamodel.procedure.gotostmt.GoToStatement;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.Else;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.IfStatement;
import io.proleap.cobol.asg.metamodel.procedure.ifstmt.Then;
import io.proleap.cobol.asg.metamodel.procedure.move.MoveStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformInlineStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformProcedureStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement;
import io.proleap.cobol.asg.metamodel.procedure.perform.PerformType;
import io.proleap.cobol.asg.metamodel.valuestmt.ValueStmt;

public class JsonAstSerializer {

    private static final Gson GSON = new GsonBuilder().disableHtmlEscaping().create();

    /**
     * Serialize a parsed Program to JSON.
     */
    public static String serialize(Program program, File inputFile) {
        JsonObject root = new JsonObject();

        // Derive program_id from filename
        String fileName = inputFile.getName();
        int dot = fileName.lastIndexOf('.');
        String programId = (dot > 0 ? fileName.substring(0, dot) : fileName).toUpperCase();
        root.addProperty("program_id", programId);

        JsonArray paragraphs = new JsonArray();

        for (CompilationUnit cu : program.getCompilationUnits()) {
            ProgramUnit pu = cu.getProgramUnit();
            if (pu == null) continue;

            ProcedureDivision procDiv = pu.getProcedureDivision();
            if (procDiv == null) continue;

            // Get all paragraphs (including those in sections)
            List<Paragraph> allParagraphs = procDiv.getParagraphs();
            if (allParagraphs != null) {
                for (Paragraph para : allParagraphs) {
                    JsonObject paraJson = serializeParagraph(para);
                    if (paraJson != null) {
                        paragraphs.add(paraJson);
                    }
                }
            }
        }

        root.add("paragraphs", paragraphs);
        return GSON.toJson(root);
    }

    private static JsonObject serializeParagraph(Paragraph paragraph) {
        if (paragraph.getParagraphName() == null) return null;

        String name = paragraph.getParagraphName().getName();
        if (name == null || name.isBlank()) return null;

        JsonObject obj = new JsonObject();
        obj.addProperty("name", name.toUpperCase());

        // Line range from ANTLR context
        ParserRuleContext ctx = paragraph.getCtx();
        int lineStart = 0, lineEnd = 0;
        if (ctx != null) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            if (start != null) lineStart = start.getLine();
            if (stop != null) lineEnd = stop.getLine();
        }
        obj.addProperty("line_start", lineStart);
        obj.addProperty("line_end", lineEnd);

        // Serialize statements
        JsonArray statements = new JsonArray();
        List<Statement> stmts = paragraph.getStatements();
        if (stmts != null) {
            for (Statement stmt : stmts) {
                JsonObject stmtJson = serializeStatement(stmt);
                if (stmtJson != null) {
                    statements.add(stmtJson);
                }
            }
        }
        obj.add("statements", statements);

        return obj;
    }

    private static JsonObject serializeStatement(Statement stmt) {
        if (stmt == null) return null;

        JsonObject obj = new JsonObject();

        // Map ProLeap StatementType to citadel type string
        String type = mapStatementType(stmt);
        obj.addProperty("type", type);

        // Source text and line range
        ParserRuleContext ctx = stmt.getCtx();
        String sourceText = "";
        int lineStart = 0, lineEnd = 0;
        if (ctx != null) {
            sourceText = getSourceText(ctx);
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            if (start != null) lineStart = start.getLine();
            if (stop != null) lineEnd = stop.getLine();
        }
        obj.addProperty("text", sourceText);
        obj.addProperty("line_start", lineStart);
        obj.addProperty("line_end", lineEnd);

        // Attributes and children depend on statement type
        JsonObject attrs = new JsonObject();
        JsonArray children = new JsonArray();

        if (stmt instanceof IfStatement ifStmt) {
            serializeIf(ifStmt, attrs, children);
        } else if (stmt instanceof EvaluateStatement evalStmt) {
            serializeEvaluate(evalStmt, attrs, children);
        } else if (stmt instanceof PerformStatement perfStmt) {
            serializePerform(perfStmt, type, attrs, children, obj);
        } else if (stmt instanceof MoveStatement moveStmt) {
            serializeMove(moveStmt, attrs);
        } else if (stmt instanceof CallStatement callStmt) {
            serializeCall(callStmt, attrs);
        } else if (stmt instanceof GoToStatement gotoStmt) {
            serializeGoTo(gotoStmt, attrs);
        } else if (stmt instanceof ExecSqlStatement sqlStmt) {
            attrs.addProperty("raw_text", sqlStmt.getExecSqlText() != null ? sqlStmt.getExecSqlText() : "");
        } else if (stmt instanceof ExecCicsStatement cicsStmt) {
            attrs.addProperty("raw_text", cicsStmt.getExecCicsText() != null ? cicsStmt.getExecCicsText() : "");
        } else if (stmt instanceof ExecSqlImsStatement imsStmt) {
            attrs.addProperty("raw_text", imsStmt.getExecSqlImsText() != null ? imsStmt.getExecSqlImsText() : "");
        }

        obj.add("attributes", attrs);
        obj.add("children", children);

        return obj;
    }

    // ── Statement type mapping ──────────────────────────────────────

    private static String mapStatementType(Statement stmt) {
        StatementTypeEnum ste = getTypeEnum(stmt);
        if (ste == null) return "UNKNOWN";

        // Handle PERFORM subtypes
        if (ste == StatementTypeEnum.PERFORM && stmt instanceof PerformStatement ps) {
            return mapPerformType(ps);
        }

        return switch (ste) {
            case IF -> "IF";
            case EVALUATE -> "EVALUATE";
            case ACCEPT -> "ACCEPT";
            case ADD -> "ADD";
            case ALTER -> "ALTER";
            case CALL -> "CALL";
            case CANCEL -> "UNKNOWN";  // not in citadel
            case CLOSE -> "CLOSE";
            case COMPUTE -> "COMPUTE";
            case CONTINUE -> "CONTINUE";
            case DELETE -> "UNKNOWN";
            case DISABLE -> "UNKNOWN";
            case DISPLAY -> "DISPLAY";
            case DIVIDE -> "DIVIDE";
            case ENABLE -> "UNKNOWN";
            case ENTRY -> "UNKNOWN";
            case EXEC_CICS -> "EXEC_CICS";
            case EXEC_SQL -> "EXEC_SQL";
            case EXEC_SQLIMS -> "EXEC_DLI";
            case EXHIBIT -> "DISPLAY";
            case EXIT -> "EXIT";
            case GENERATE -> "UNKNOWN";
            case GO_BACK -> "GOBACK";
            case GO_TO -> "GO_TO";
            case INITIALIZE -> "INITIALIZE";
            case INITIATE -> "UNKNOWN";
            case INSPECT -> "INSPECT";
            case MERGE -> "SORT";
            case MOVE -> "MOVE";
            case MULTIPLY -> "MULTIPLY";
            case OPEN -> "OPEN";
            case PERFORM -> "PERFORM";
            case PURGE -> "UNKNOWN";
            case READ -> "READ";
            case RECEIVE -> "UNKNOWN";
            case RELEASE -> "UNKNOWN";
            case RETURN -> "UNKNOWN";
            case REWRITE -> "REWRITE";
            case SEARCH -> "SEARCH";
            case SEND -> "UNKNOWN";
            case SET -> "SET";
            case SORT -> "SORT";
            case START -> "UNKNOWN";
            case STOP -> "STOP_RUN";
            case STRING -> "STRING";
            case SUBTRACT -> "SUBTRACT";
            case TERMINATE -> "UNKNOWN";
            case UNSTRING -> "UNSTRING";
            case USE -> "UNKNOWN";
            case WRITE -> "WRITE";
            default -> "UNKNOWN";
        };
    }

    private static String mapPerformType(PerformStatement ps) {
        PerformStatement.PerformStatementType pst = ps.getPerformStatementType();
        if (pst == PerformStatement.PerformStatementType.INLINE) {
            return "PERFORM_INLINE";
        }
        // Procedure-style PERFORM
        PerformProcedureStatement proc = ps.getPerformProcedureStatement();
        if (proc != null) {
            List<?> calls = proc.getCalls();
            if (calls != null && calls.size() >= 2) {
                return "PERFORM_THRU";
            }
            PerformType pt = proc.getPerformType();
            if (pt != null) {
                PerformType.PerformTypeType ptt = pt.getPerformTypeType();
                if (ptt == PerformType.PerformTypeType.UNTIL) {
                    return "PERFORM_UNTIL";
                }
            }
        }
        return "PERFORM";
    }

    private static StatementTypeEnum getTypeEnum(Statement stmt) {
        try {
            Object type = stmt.getStatementType();
            if (type instanceof StatementTypeEnum) {
                return (StatementTypeEnum) type;
            }
        } catch (Exception e) {
            // defensive
        }
        return null;
    }

    // ── Nested scope serialization ──────────────────────────────────

    private static void serializeIf(IfStatement ifStmt, JsonObject attrs, JsonArray children) {
        // Condition
        try {
            ValueStmt condStmt = ifStmt.getCondition();
            if (condStmt != null && condStmt.getCtx() != null) {
                attrs.addProperty("condition", getSourceText(condStmt.getCtx()));
            }
        } catch (Exception e) {
            // condition extraction failed, not critical
        }

        // Then branch
        Then thenBranch = ifStmt.getThen();
        if (thenBranch != null) {
            serializeScopeStatements(thenBranch, children);
        }

        // Else branch
        Else elseBranch = ifStmt.getElse();
        if (elseBranch != null) {
            JsonObject elseNode = new JsonObject();
            elseNode.addProperty("type", "ELSE");
            elseNode.addProperty("text", "ELSE");

            ParserRuleContext elseCtx = elseBranch.getCtx();
            if (elseCtx != null) {
                Token start = elseCtx.getStart();
                Token stop = elseCtx.getStop();
                elseNode.addProperty("line_start", start != null ? start.getLine() : 0);
                elseNode.addProperty("line_end", stop != null ? stop.getLine() : 0);
            } else {
                elseNode.addProperty("line_start", 0);
                elseNode.addProperty("line_end", 0);
            }

            JsonArray elseChildren = new JsonArray();
            serializeScopeStatements(elseBranch, elseChildren);
            elseNode.add("children", elseChildren);
            elseNode.add("attributes", new JsonObject());

            children.add(elseNode);
        }
    }

    private static void serializeEvaluate(EvaluateStatement evalStmt, JsonObject attrs, JsonArray children) {
        // Subject
        try {
            var select = evalStmt.getSelect();
            if (select != null && select.getCtx() != null) {
                attrs.addProperty("subject", getSourceText(select.getCtx()));
            }
        } catch (Exception e) {
            // not critical
        }

        // WHEN phrases (each is a Scope containing statements)
        List<WhenPhrase> whenPhrases = evalStmt.getWhenPhrases();
        if (whenPhrases != null) {
            for (WhenPhrase wp : whenPhrases) {
                JsonObject whenNode = new JsonObject();
                whenNode.addProperty("type", "WHEN");

                // Get WHEN value text
                String whenText = "WHEN";
                try {
                    var whens = wp.getWhens();
                    if (whens != null && !whens.isEmpty()) {
                        var firstWhen = whens.get(0);
                        if (firstWhen.getCtx() != null) {
                            whenText = "WHEN " + getSourceText(firstWhen.getCtx());
                        }
                    }
                } catch (Exception e) {
                    // not critical
                }
                whenNode.addProperty("text", whenText);

                ParserRuleContext wpCtx = wp.getCtx();
                if (wpCtx != null) {
                    Token start = wpCtx.getStart();
                    Token stop = wpCtx.getStop();
                    whenNode.addProperty("line_start", start != null ? start.getLine() : 0);
                    whenNode.addProperty("line_end", stop != null ? stop.getLine() : 0);
                } else {
                    whenNode.addProperty("line_start", 0);
                    whenNode.addProperty("line_end", 0);
                }

                JsonArray whenChildren = new JsonArray();
                serializeScopeStatements(wp, whenChildren);
                whenNode.add("children", whenChildren);
                whenNode.add("attributes", new JsonObject());

                children.add(whenNode);
            }
        }

        // WHEN OTHER
        WhenOther whenOther = evalStmt.getWhenOther();
        if (whenOther != null) {
            JsonObject otherNode = new JsonObject();
            otherNode.addProperty("type", "WHEN");
            otherNode.addProperty("text", "WHEN OTHER");

            ParserRuleContext otherCtx = whenOther.getCtx();
            if (otherCtx != null) {
                Token start = otherCtx.getStart();
                Token stop = otherCtx.getStop();
                otherNode.addProperty("line_start", start != null ? start.getLine() : 0);
                otherNode.addProperty("line_end", stop != null ? stop.getLine() : 0);
            } else {
                otherNode.addProperty("line_start", 0);
                otherNode.addProperty("line_end", 0);
            }

            JsonArray otherChildren = new JsonArray();
            // WhenOther extends Scope, so we can pass it directly
            serializeScopeStatements(whenOther, otherChildren);
            otherNode.add("children", otherChildren);
            otherNode.add("attributes", new JsonObject());

            children.add(otherNode);
        }
    }

    private static void serializePerform(PerformStatement perfStmt, String type,
                                          JsonObject attrs, JsonArray children, JsonObject obj) {
        if (perfStmt.getPerformStatementType() == PerformStatement.PerformStatementType.INLINE) {
            // Inline PERFORM — has nested statements
            PerformInlineStatement inlineStmt = perfStmt.getPerformInlineStatement();
            if (inlineStmt != null) {
                // Condition
                PerformType pt = inlineStmt.getPerformType();
                if (pt != null) {
                    try {
                        if (pt.getUntil() != null && pt.getUntil().getCtx() != null) {
                            attrs.addProperty("condition", getSourceText(pt.getUntil().getCtx()));
                        } else if (pt.getVarying() != null && pt.getVarying().getCtx() != null) {
                            attrs.addProperty("varying", getSourceText(pt.getVarying().getCtx()));
                        }
                    } catch (Exception e) {
                        // not critical
                    }
                }
                serializeScopeStatements(inlineStmt, children);
            }
        } else {
            // Procedure-style PERFORM
            PerformProcedureStatement proc = perfStmt.getPerformProcedureStatement();
            if (proc != null) {
                var calls = proc.getCalls();
                if (calls != null && !calls.isEmpty()) {
                    String target = calls.get(0).getName();
                    if (target != null) attrs.addProperty("target", target.toUpperCase());

                    if (calls.size() >= 2) {
                        String thru = calls.get(1).getName();
                        if (thru != null) attrs.addProperty("thru", thru.toUpperCase());
                    }
                }
                // UNTIL condition
                PerformType pt = proc.getPerformType();
                if (pt != null) {
                    try {
                        if (pt.getUntil() != null && pt.getUntil().getCtx() != null) {
                            attrs.addProperty("condition", getSourceText(pt.getUntil().getCtx()));
                        } else if (pt.getTimes() != null && pt.getTimes().getCtx() != null) {
                            attrs.addProperty("times", getSourceText(pt.getTimes().getCtx()));
                        }
                    } catch (Exception e) {
                        // not critical
                    }
                }
            }
        }
    }

    private static void serializeMove(MoveStatement moveStmt, JsonObject attrs) {
        try {
            if (moveStmt.getMoveToStatement() != null) {
                var moveTo = moveStmt.getMoveToStatement();
                if (moveTo.getSendingArea() != null && moveTo.getSendingArea().getCtx() != null) {
                    attrs.addProperty("source", getSourceText(moveTo.getSendingArea().getCtx()));
                }
                var receivingAreas = moveTo.getReceivingAreaCalls();
                if (receivingAreas != null && !receivingAreas.isEmpty()) {
                    StringBuilder targets = new StringBuilder();
                    for (var call : receivingAreas) {
                        if (!targets.isEmpty()) targets.append(", ");
                        targets.append(call.getName() != null ? call.getName() : "");
                    }
                    attrs.addProperty("targets", targets.toString());
                }
            }
        } catch (Exception e) {
            // not critical
        }
    }

    private static void serializeCall(CallStatement callStmt, JsonObject attrs) {
        try {
            ValueStmt progStmt = callStmt.getProgramValueStmt();
            if (progStmt != null && progStmt.getCtx() != null) {
                String target = getSourceText(progStmt.getCtx());
                target = target.replace("'", "").replace("\"", "").trim();
                attrs.addProperty("target", target.toUpperCase());
            }
        } catch (Exception e) {
            // not critical
        }
    }

    private static void serializeGoTo(GoToStatement gotoStmt, JsonObject attrs) {
        try {
            if (gotoStmt.getSimple() != null && gotoStmt.getSimple().getCtx() != null) {
                attrs.addProperty("target", getSourceText(gotoStmt.getSimple().getCtx()).toUpperCase());
            }
        } catch (Exception e) {
            // not critical
        }
    }

    // ── Helpers ─────────────────────────────────────────────────────

    private static void serializeScopeStatements(Scope scope, JsonArray target) {
        List<Statement> stmts = scope.getStatements();
        if (stmts != null) {
            for (Statement stmt : stmts) {
                JsonObject stmtJson = serializeStatement(stmt);
                if (stmtJson != null) {
                    target.add(stmtJson);
                }
            }
        }
    }

    /**
     * Extract source text from a ParserRuleContext, preserving whitespace
     * by reading the underlying character stream between start and stop tokens.
     */
    private static String getSourceText(ParserRuleContext ctx) {
        if (ctx == null) return "";

        try {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            if (start != null && stop != null) {
                // Use the underlying input stream for whitespace-preserving text
                var inputStream = start.getTokenSource().getInputStream();
                if (inputStream != null) {
                    int startIdx = start.getStartIndex();
                    int stopIdx = stop.getStopIndex();
                    if (startIdx >= 0 && stopIdx >= startIdx) {
                        String text = inputStream.getText(
                            org.antlr.v4.runtime.misc.Interval.of(startIdx, stopIdx));
                        if (text != null && !text.isBlank()) {
                            // Normalize: collapse multi-line to single, trim
                            text = text.replaceAll("\\s+", " ").strip();
                            if (text.endsWith(".")) {
                                text = text.substring(0, text.length() - 1).strip();
                            }
                            if (text.length() > 200) {
                                text = text.substring(0, 197) + "...";
                            }
                            return text;
                        }
                    }
                }
            }
        } catch (Exception e) {
            // fall through to ctx.getText()
        }

        // Fallback: concatenated tokens (no whitespace)
        String text = ctx.getText();
        if (text != null) {
            text = text.strip();
            if (text.endsWith(".")) {
                text = text.substring(0, text.length() - 1).strip();
            }
            if (text.length() > 200) {
                text = text.substring(0, 197) + "...";
            }
            return text;
        }
        return "";
    }
}
