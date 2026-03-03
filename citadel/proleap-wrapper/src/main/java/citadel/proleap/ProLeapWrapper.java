/*
 * Thin wrapper around ProLeap COBOL parser for citadel AST generation.
 *
 * Reads a COBOL source file, builds an ASG via ProLeap, serializes the
 * procedure-division structure to JSON on stdout.
 *
 * Missing copybooks (IBM MQ, CICS, DB2, or any external dependency) are
 * handled automatically: when ProLeap's preprocessor throws because a
 * COPY member cannot be found, the wrapper creates an empty stub in a
 * temp directory and retries.  This lets the parser proceed — fields from
 * stub copybooks will be unresolved, but the procedure-division control
 * flow (which is what citadel cares about) parses correctly.
 *
 * Usage:
 *   java -jar proleap-wrapper-fat.jar <source-file> [options]
 *
 * Options:
 *   --copybook-dir=<dir>   Add a copybook search directory (repeatable)
 *   --format=fixed|free    Source format (default: fixed)
 *   --version              Print version and exit
 */
package citadel.proleap;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.params.impl.CobolParserParamsImpl;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.preprocessor.exception.CobolPreprocessorException;

public class ProLeapWrapper {

    static final String VERSION = "1.0.0";

    /**
     * Maximum number of stub-creation retries.  Each retry resolves one
     * missing copybook, so this caps the total number of external
     * dependencies a single source file can reference.
     */
    private static final int MAX_STUB_RETRIES = 50;

    /**
     * Pattern to extract the copybook name from ProLeap's error message:
     * "Could not find copy book CMQODV in directory of ..."
     */
    private static final Pattern MISSING_COPYBOOK_RE =
            Pattern.compile("Could not find copy book (\\S+) in directory");

    public static void main(String[] args) {
        if (args.length == 0) {
            System.err.println("Usage: proleap-wrapper <source-file> [--copybook-dir=<dir>]... [--format=fixed|free] [--version]");
            System.exit(1);
        }

        String sourceFile = null;
        List<File> copybookDirs = new ArrayList<>();
        CobolSourceFormatEnum format = CobolSourceFormatEnum.FIXED;

        for (String arg : args) {
            if ("--version".equals(arg)) {
                System.out.println("proleap-wrapper " + VERSION);
                System.exit(0);
            } else if (arg.startsWith("--copybook-dir=")) {
                copybookDirs.add(new File(arg.substring("--copybook-dir=".length())));
            } else if (arg.startsWith("--format=")) {
                String fmt = arg.substring("--format=".length()).toUpperCase();
                if ("VARIABLE".equals(fmt) || "FREE".equals(fmt)) {
                    format = CobolSourceFormatEnum.VARIABLE;
                } else if ("TANDEM".equals(fmt)) {
                    format = CobolSourceFormatEnum.TANDEM;
                }
                // default stays FIXED
            } else if (!arg.startsWith("-")) {
                sourceFile = arg;
            } else {
                System.err.println("Unknown option: " + arg);
                System.exit(1);
            }
        }

        if (sourceFile == null) {
            System.err.println("ERROR: No source file specified");
            System.exit(1);
        }

        File inputFile = new File(sourceFile);
        if (!inputFile.isFile()) {
            System.err.println("ERROR: File not found: " + sourceFile);
            System.exit(1);
        }

        // Always include the source file's directory as a copybook dir
        File sourceDir = inputFile.getAbsoluteFile().getParentFile();
        if (!copybookDirs.contains(sourceDir)) {
            copybookDirs.add(sourceDir);
        }

        try {
            Path stubDir = Files.createTempDirectory("proleap-stubs-");
            copybookDirs.add(stubDir.toFile());

            Program program = parseWithStubRetry(
                    inputFile, copybookDirs, format, stubDir);

            String json = JsonAstSerializer.serialize(program, inputFile);
            System.out.println(json);

            // Clean up temp stubs
            deleteDir(stubDir);
        } catch (Exception e) {
            System.err.println("ERROR: Parse failed: " + e.getMessage());
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    /**
     * Attempt to parse the file.  When the preprocessor cannot resolve a
     * COPY member, create an empty stub and retry.
     */
    private static Program parseWithStubRetry(
            File inputFile,
            List<File> copybookDirs,
            CobolSourceFormatEnum format,
            Path stubDir) throws Exception {

        Set<String> stubbed = new HashSet<>();

        for (int attempt = 0; attempt <= MAX_STUB_RETRIES; attempt++) {
            try {
                CobolParserParams params = new CobolParserParamsImpl();
                params.setFormat(format);
                params.setCopyBookDirectories(copybookDirs);
                params.setIgnoreSyntaxErrors(true);

                return new CobolParserRunnerImpl().analyzeFile(inputFile, params);

            } catch (CobolPreprocessorException e) {
                String name = extractMissingCopybook(e.getMessage());

                if (name == null || stubbed.contains(name)) {
                    // Can't extract a name, or we already stubbed it and it
                    // still fails — give up with the original error.
                    throw e;
                }

                // Create an empty stub copybook
                createStub(stubDir, name);
                stubbed.add(name);
                System.err.println("STUB: created empty copybook for " + name);
            }
        }

        throw new RuntimeException(
                "Exceeded " + MAX_STUB_RETRIES + " stub retries for " + inputFile);
    }

    /**
     * Extract the copybook name from a ProLeap preprocessor error message.
     */
    private static String extractMissingCopybook(String message) {
        if (message == null) return null;
        Matcher m = MISSING_COPYBOOK_RE.matcher(message);
        return m.find() ? m.group(1) : null;
    }

    /**
     * Create an empty copybook stub file.  ProLeap searches for copybooks
     * with several extensions (.cpy, .CPY, .cbl, .CBL, and bare name), so
     * we create both the bare name and a .cpy variant.
     */
    private static void createStub(Path stubDir, String name) throws IOException {
        // Empty content — enough for COPY resolution to succeed
        byte[] empty = new byte[0];
        Files.write(stubDir.resolve(name), empty);
        Files.write(stubDir.resolve(name + ".cpy"), empty);
        Files.write(stubDir.resolve(name + ".CPY"), empty);
    }

    /**
     * Recursively delete a temp directory.
     */
    private static void deleteDir(Path dir) {
        try {
            File[] files = dir.toFile().listFiles();
            if (files != null) {
                for (File f : files) {
                    f.delete();
                }
            }
            dir.toFile().delete();
        } catch (Exception e) {
            // best effort cleanup
        }
    }
}
