package docent;

import java.io.*;
import java.util.*;

public class DatasetSplitter {
    private static Random random = new Random();

    public static void split(String filename, int[] elementCounts, boolean useHeader)
            throws IOException {
        File input = new File(filename);
        BufferedReader reader = new BufferedReader(new FileReader(input));
        String baseOutFileName = input.getName();
        String outFilesPath = input.getParent();

        BufferedWriter[] writers = new BufferedWriter[elementCounts.length];
        for (int i = 0; i < writers.length; i++) {
            String outFileName = outFilesPath + File.separatorChar + i + "_" + elementCounts[i] + baseOutFileName;
            writers[i] = new BufferedWriter(new FileWriter(outFileName));
        }

        String header = reader.readLine();
        String line;
        Set<String> lines = new HashSet<String>();

        try {
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
            for (int i = 0; i < elementCounts.length; ++i) {
                int elementCount = elementCounts[i];
                BufferedWriter currentWriter = writers[i];
                if (useHeader) {
                    currentWriter.write(header);
                    currentWriter.newLine();
                }
                for (int j = 0; j < elementCount; ++j) {
                    int randomIndex = Math.abs(random.nextInt()) % lines.size();
                    String randomLine = lines.toArray(new String[lines.size()])[randomIndex];
                    currentWriter.write(randomLine);
                    currentWriter.newLine();
                }
            }
        } finally {
            reader.close();
            for (BufferedWriter writer : writers) {
                writer.close();
            }
        }
    }

    public static void main(String[] args) throws IOException {
        if (args.length <= 2) {
            throw new RuntimeException("Usage: java docent.DatasetSplitter " +
                    "input_file use_header count1 count2 count3 ...");
        }
        String filename = args[0];
        boolean useHeader = Boolean.parseBoolean(args[1]);
        int[] elementCounts = new int[args.length - 2];
        for (int i = 2; i < args.length; i++) {
            int elementCount = Integer.parseInt(args[i]);
            if (elementCount <= 1) {
                throw new RuntimeException("Number of elements must be >= 1");
            }
            elementCounts[i - 2] = elementCount;
        }
        DatasetSplitter.split(filename, elementCounts, useHeader);
    }
}
