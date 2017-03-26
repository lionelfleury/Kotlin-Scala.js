package ch.epfl.k2sjsir.codegen;

import org.jetbrains.kotlin.ir.SourceManager.FileEntry;
import org.jetbrains.kotlin.ir.declarations.IrFile;
import org.scalajs.core.ir.Position;
import org.scalajs.core.ir.Position.SourceFile$;

public class Positioner {

    private final FileEntry fileEntry;

    public Positioner(IrFile irFile) {
        this.fileEntry = irFile.getFileEntry();
    }

    public Position getPos(int startOffset) {
        String file = fileEntry.getName();
        int line = fileEntry.getLineNumber(startOffset);
        int column = fileEntry.getColumnNumber(startOffset);
        return new Position(SourceFile$.MODULE$.apply(file), line, column);
    }

}
