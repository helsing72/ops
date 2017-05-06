/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package configlib;

import configlib.*;
import java.util.Vector;

/**
 *
 * @author angr
 */
public class TestConfigClass
{
    public String filename = "";
    private String pfilename = "";
    public int aIjhhvvnt;
    public double aDouble;
    public Vector<String> strings = new Vector<String>();

    public byte b;
    public TestClass2 class2 = new TestClass2();
    public Vector<TestClass2> node = new Vector<TestClass2>();

    public TestConfigClass()
    {
        strings.add(new String("Ett"));
        strings.add("Två");
        strings.add("osv...");

        node.add(class2);
        node.add(class2);
    }

}
