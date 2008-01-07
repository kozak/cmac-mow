package wtf;

public class CmacTest {
    public static void main(String[] args) {
        Cmac cmac = new Cmac(
                new AttrDesc[] {
                    new AttrDesc("at0", 1.0f, 10.0f, 9)     
                }, 3, 3
        );
        int nLayers = 3;
        for (int i = 0; i < nLayers; i++) {
            System.out.println(cmac.getInterval(cmac.getAttrDescs()[0], i, 6.34f));
        }
    }
}
