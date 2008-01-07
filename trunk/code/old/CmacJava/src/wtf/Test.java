package wtf;

public class Test {
    public static void main(String[] args) {
        float[][] sinX = new float[1000][2];
        float[] vals = new float[sinX.length];
        for (int i = 0; i < sinX.length; i++) {
            float x = (float) ((-1.0d * Math.PI) + (Math.PI / (sinX.length / 2) * i));
            float x2 = x / -2.0f;
            sinX[i][0] = x;
            sinX[i][1] = x2;
            vals[i] = (float) (Math.sin(x) - Math.sin(x2) * 3 * Math.cos(x2 / 2));
            System.out.println("X = " + sinX[i][0] + ", sin(x) = " + vals[i]);
        }

        AttrDesc[] attrs = new AttrDesc[] {
                new AttrDesc("sin", -4.0f, 4.0f, 200),
                new AttrDesc("sin2", -4.0f, 4.0f, 200)
        };
        Cmac cmac = new Cmac(attrs, 30, 26);
//        cmac.train(sinX, vals, 0.000001f, 0.4f);
        for (int i = 0; i < vals.length; i++) {
            float val = vals[i];
            System.out.println("Desired out = " + val + ", actual = " + cmac.computeOutput(sinX[i]));
        }

        float[][] input = new float[][] {
                new float[] {1},
                new float[] {2},
                new float[] {3}
        };
        float[] output = new float[] {2,4,6};
        Cmac cmac2 = new Cmac(
                new AttrDesc[] {
                        new AttrDesc("lol", 1,3,3)
                }, 4, 26
        );
        cmac2.train(input, output, 0.000001f, 0.3f);


        for (int i = 0; i < output.length; i++) {
            float val = output[i];
            System.out.println("Desired out = " + val + ", actual = " + cmac2.computeOutput(input[i]));
        }
        System.out.println(cmac2.computeOutput(new float[] {1.5f}));
    }
}
