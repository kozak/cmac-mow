package wtf;

public class AttrDesc {
    public AttrDesc(String name, float min, float max, int nFirstLayerDivisions) {
        this.name = name;
        this.min = min;
        this.max = max;
        this.nFirstLayerDivisions = nFirstLayerDivisions;
    }

    public String name;
    public float min;
    public float max;
    public int nFirstLayerDivisions;
}
