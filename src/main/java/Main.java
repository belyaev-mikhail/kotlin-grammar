import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.misc.ParseCancellationException;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class Main {
    private static Stream<File> kotlinFiles(File base) {
        if(!base.isDirectory() && base.getName().endsWith(".kt")) {
            return Stream.of(base);
        }

        File[] files = base.listFiles();
        if(files == null) return Stream.empty();

        return Stream.of(files).flatMap(Main::kotlinFiles);
    }

    private static KotlinParser parseFile(File file) throws IOException {
        KotlinLexer kl = new KotlinLexer(new ANTLRFileStream(file.getAbsolutePath()));
        CommonTokenStream kts = new CommonTokenStream(kl);
        KotlinParser kp = new KotlinParser(kts);
        kp.removeErrorListeners();
        kp.addErrorListener(new ErrorPrinter());
        return kp;
    }

    private static KotlinParser parseString(String code) throws IOException {
        KotlinLexer kl = new KotlinLexer(new ANTLRInputStream(code));
        CommonTokenStream kts = new CommonTokenStream(kl);
        KotlinParser kp = new KotlinParser(kts);
        kp.removeErrorListeners();
        kp.addErrorListener(new ErrorPrinter());
        return kp;
    }

    static class ErrorPrinter extends BaseErrorListener {
        @Override
        public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e)
                throws ParseCancellationException {
            throw new ParseCancellationException("line " + line + ":" + charPositionInLine + " " + msg);
        }
    }

    public static void main(String[] args) throws IOException {

        String base = args.length > 0 ? args[0] : "examples";

        Stream<File> files = kotlinFiles(new File(base));
        int totalFiles = 0;
        int successful = 0;

        List<File> fileList = files.collect(Collectors.toList());
        for (File source: fileList) try {
            totalFiles++;
            System.out.println("Parsing: " + source);
            System.out.flush();
            KotlinParser kp = parseFile(source);

            KotlinParser.KotlinFileContext ctx = kp.kotlinFile();
            if(fileList.size() == 1){
                List<String> ruleList = Arrays
                        .stream(kp.getRuleNames())
                        .filter(it -> it.matches("[a-zA-Z]*"))
                        .collect(Collectors.toList());

                ctx.inspect(ruleList);
            }
            successful++;
        } catch (Exception ex) {
            System.out.println("Exception: " + ex.getMessage());
        }

        System.out.println("Total files: " + totalFiles + "; successfully parsed: " + successful);
    }
}