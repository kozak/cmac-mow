package wtf;

import org.apache.log4j.Logger;

import java.util.HashMap;

public class Cmac {
    private Logger logger = Logger.getLogger(getClass());
    private float[] weights;
    private int nWeightBits;
    private final AttrDesc[] attrDescs;
    private final int nLayers;

    public Cmac(AttrDesc[] baseAttrDesc, int nLayers, int nWeightBits) {
        this.attrDescs = baseAttrDesc;
        this.nLayers = nLayers;
        this.nWeightBits = nWeightBits;
        weights = new float[2 << (nWeightBits - 1)];
    }

    public void train(float[][] input, float[] desiredOutput, float desiredMse, float tr) {
        float mse;
        while ((mse = mse(input, desiredOutput)) > desiredMse) {
            logger.info("Training... Mse = " + mse);
            for (int i = 0; i < desiredOutput.length; i++) {
                float[] example = input[i];
                int[] weightIndices = getRealWeightIndices(getWeightIndicesActivatedBy(example));
                float actualOutput = computeOutput(example);
                float weightUpdate = (desiredOutput[i] - actualOutput) * tr / weightIndices.length;
                for (int weightIndex : weightIndices) {
                    weights[weightIndex] += weightUpdate;
                }
            }
        }
    }

    private int[] getRealWeightIndices(int[] indices) {
        int[] realIndices = new int[indices.length];
        for (int i = 0; i < realIndices.length; i++) {
            realIndices[i] = hash(indices[i] * (i + 1), nWeightBits);
        }
        return realIndices;
    }

    private float mse(float[][] input, float[] desiredOutput) {
        float[] outputs = new float[input.length];
        for (int i = 0; i < input.length; i++) {
            outputs[i] = computeOutput(input[i]);
        }
        float mse = 0.0f;

        for (int i = 0; i < outputs.length; i++) {
            float diff = (outputs[i] - desiredOutput[i]);
            mse += diff * diff;
        }
        mse /= outputs.length;
        return mse;
    }

    public float computeOutput(float[] input) {
        if (logger.isDebugEnabled()) {
            StringBuffer sb = new StringBuffer();
            for (float v : input) {
                sb.append(v).append(" ");
            }
            logger.debug("Computing out for input [" + sb + "]");
        }
        int[] weightIndices = getWeightIndicesActivatedBy(input);
        int[] realWeightIndices = getRealWeightIndices(weightIndices);

        if (logger.isDebugEnabled()) {
            StringBuilder sb = new StringBuilder();
            for (int weightIndice : weightIndices) {
                sb.append(weightIndice).append(" ");
            }
            logger.debug("Indices referenced: " + sb);
            sb = new StringBuilder();
            for (int realWeightIndice : realWeightIndices) {
                sb.append(realWeightIndice).append(" ");
            }
            logger.debug("REAL Indices referenced: " + sb);
        }

        float output = 0.0f;
        for (int weightIndice : realWeightIndices) {
            output += weights[weightIndice];
        }
        return output;
    }

    private int[] getWeightIndicesActivatedBy(float[] input) {
        int[] weightIndices = new int[nLayers];
        for (int i = 0; i < nLayers; i++) {
            int[] indices = new int[attrDescs.length];
            for (int j = 0; j < attrDescs.length; j++) {
                AttrDesc attrDesc = attrDescs[j];
                indices[j] = getInterval(attrDescs[j], i, input[j]);
            }
            weightIndices[i] = getWeightIndex(indices, attrDescs);
        }
        return weightIndices;
    }

    public int getInterval(AttrDesc attrDesc, float iLayer, float input) {
        if (iLayer == 0) {
            if (input == attrDesc.max) {
                return (attrDesc.nFirstLayerDivisions - 1);
            }
            return (int) Math.floor(
                    (input - attrDesc.min) / (attrDesc.max - attrDesc.min) * attrDesc.nFirstLayerDivisions
            );
        } else {
            float intervalWidth = (attrDesc.max - attrDesc.min) / attrDesc.nFirstLayerDivisions;
            float shift = (intervalWidth / nLayers) * iLayer;
            float inputPos = input - shift;
            return (int) Math.ceil(
                    (inputPos - attrDesc.min) / (attrDesc.max - attrDesc.min) * attrDesc.nFirstLayerDivisions
            );
        }
    }

    private int getWeightIndex(int[] intervals, AttrDesc[] attrDescs) {
        int index = 0;
        int multiplier = 1;
        for (int i = 0; i < intervals.length; i++) {
            index += intervals[i] * multiplier;
            multiplier *= attrDescs[i].nFirstLayerDivisions;
        }
        return index;
    }

    private int[] getWeightIndicesForLayer(float[] inputs, int iLayer) {
        return null;
    }

    public AttrDesc[] getAttrDescs() {
        return attrDescs;
    }

    private static int hash(int a, int nBits) {
        a = (a + 0x7ed55d16) + (a << 12);
        a = (a ^ 0xc761c23c) ^ (a >> 19);
        a = (a + 0x165667b1) + (a << 5);
        a = (a + 0xd3a2646c) ^ (a << 9);
        a = (a + 0xfd7046c5) + (a << 3);
        a = (a ^ 0xb55a4f09) ^ (a >> 16);
        return Math.abs(a) % (2 << (nBits - 1));
    }

    public static void main(String[] args) {
        for (int i = 0; i < 1000; i++) {
            System.out.println(hash(i, 16));
        }
    }
}
