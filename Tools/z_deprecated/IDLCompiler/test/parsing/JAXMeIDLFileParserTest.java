/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package parsing;

import java.util.logging.Level;
import java.util.logging.Logger;
import junit.framework.TestCase;

/**
 *
 * @author angr
 */
public class JAXMeIDLFileParserTest extends TestCase {
    
    public JAXMeIDLFileParserTest(String testName) {
        super(testName);
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public void testParse() throws Exception
    {
        System.out.println("parse");
        String context = "//lsdnknkldgfndkgldfngl\npackage testpack.parpack; //hehehhfjbnjbjbsv \nclass TestClass extends test.TestBaseClass //jklnfasnkfonfdgh \n{ //hatte svat.char[] chattAr; ntratt\ndouble d ;\n double[] dubs ;fnatt.cat.Tratt t; svatt.chat[] chattAr;////fghfknlgfhn\n}";
        JAXMeIDLFileParser instance = new JAXMeIDLFileParser();
        IDLClass expResult = new IDLClass("TestClass", "testpack.parpack");
        expResult.setBaseClassName("test.TestBaseClass");
        IDLClass result = instance.parse(context);

        System.out.println("" + result.getFields());

        assertEquals(expResult.getClassName(), result.getClassName());
        assertEquals(expResult.getPackageName(), result.getPackageName());
        assertEquals(expResult.getBaseClassName(), result.getBaseClassName());
        
    }

    public static void main(String[] args)
    {
        try
        {
            JAXMeIDLFileParserTest test = new JAXMeIDLFileParserTest("debug test");
            test.testParse();






        } catch (Exception ex)
        {
            Logger.getLogger(JAXMeIDLFileParserTest.class.getName()).log(Level.SEVERE, null, ex);
        }
    
    }

    public void testParse_String() throws Exception
    {
        System.out.println("parse");
        String context = "";
        JAXMeIDLFileParser instance = new JAXMeIDLFileParser();
        IDLClass expResult = null;
        IDLClass result = instance.parse(context);
        assertEquals(expResult, result);
        fail("The test case is a prototype.");
    }

}
