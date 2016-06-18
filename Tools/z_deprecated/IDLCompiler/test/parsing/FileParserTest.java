/*
 * FileParserTest.java
 * JUnit based test
 *
 * Created on den 12 november 2007, 09:41
 */

package parsing;

import junit.framework.*;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 *
 * @author angr
 */
public class FileParserTest extends TestCase
{
    
    public FileParserTest(String testName)
    {
        super(testName);
    }

    protected void setUp() throws Exception
    {
    }

    protected void tearDown() throws Exception
    {
    }

    public static Test suite()
    {
        TestSuite suite = new TestSuite(FileParserTest.class);
        
        return suite;
    }

    /**
     * Test of parse method, of class parsing.FileParser.
     */
    public void testParse() throws Exception
    {
        System.out.println("parse");
        
        String context1 = "package test.testpack;\n   class TestClass\n{\n\tdouble d;\n}";
        String context2 = "package test.test.testpack;\n   class TestClass\n{\n\tdouble d;\n}";
        String context3 = "\npack age test.test.testpack ;\n   class TestClass \n{\n\tdouble d;\n}";
        String context4 = "package test.test.testpack\n   class TestClass\n{\n\tdouble d;\n}";
        
        String context5 = "package test.test.testpack;\n   class TestClass  {\n\tdouble d;\n}";
        String context6 = "package test.test.testpack;\n   class Test Class{\n\tdouble d;\n}";
        String context7 = "package test.test.testpack;\n   class TestClass\n{\n" +
                          "\t//Det här är en double\n" +
                          "\tdouble d;\n" +
                          "\tdouble[] ds;\n" +
                          "\tidltype test.test2.Test2Class idlTestObject;" +
                          "\n}";
        
        
        FileParser instance = new FileParser();
        
        IDLClass expResult = null;
        
        IDLClass result = instance.parse(context1);
        assertEquals(result.getPackageName(), "test.testpack");
        assertEquals(result.getClassName(), "TestClass");
        
        result = instance.parse(context2);
        assertEquals(result.getPackageName(), "test.test.testpack");
        assertEquals(result.getClassName(), "TestClass");
        
        try
        {
            
            result = instance.parse(context3);
            fail("Should through");
        } 
        catch (ParseException ex)
        {
            assertEquals("At line 2: Invalid package declaration.", ex.getMessage());
        }
        
        try
        {
            
            result = instance.parse(context4);
            fail("Should through");
        } 
        catch (ParseException ex)
        {
            assertEquals("At line 1: Missing ; .", ex.getMessage());
        }
        
        result = instance.parse(context5);
        assertEquals(result.getPackageName(), "test.test.testpack");
        assertEquals(result.getClassName(), "TestClass");
        
        try
        {
            
            result = instance.parse(context6);
            fail("Should through");
        } 
        catch (ParseException ex)
        {
            assertEquals("At line 2: Invalid class declaration.", ex.getMessage());
        }
        
        result = instance.parse(context7);
        assertEquals(result.getPackageName(), "test.test.testpack");
        assertEquals(result.getClassName(), "TestClass");
        assertEquals(result.getFields().get(0).getComment(), "//Det här är en double");
        
        
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
    
}
