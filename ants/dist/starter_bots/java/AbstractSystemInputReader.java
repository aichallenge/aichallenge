import java.io.IOException;

/**
 * Handles system input stream reading.
 */
public abstract class AbstractSystemInputReader {
    /**
     * Reads system input stream line by line. All characters are converted to lower case and each
     * line is passed for processing to {@link #processLine(String)} method.
     * 
     * @throws IOException if an I/O error occurs
     */
    public void readSystemInput() throws IOException {
        StringBuilder line = new StringBuilder();
        int c;
        while ((c = System.in.read()) >= 0) {
            if (c == '\r' || c == '\n') {
                processLine(line.toString().toLowerCase().trim());
                line.setLength(0);
            } else {
                line = line.append((char)c);
            }
        }
    }
    
    /**
     * Process a line read out by {@link #readSystemInput()} method in a way defined by subclass
     * implementation.
     * 
     * @param line single, trimmed line of system input
     */
    public abstract void processLine(String line);
}
