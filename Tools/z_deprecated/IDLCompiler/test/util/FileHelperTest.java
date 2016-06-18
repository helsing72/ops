/*
 * FileHelperTest.java
 * JUnit based test
 *
 * Created on den 27 november 2007, 08:47
 */

package util;

import junit.framework.*;

/**
 *
 * @author angr
 */
public class FileHelperTest extends TestCase
{
    
    public FileHelperTest(String testName)
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
        TestSuite suite = new TestSuite(FileHelperTest.class);
        
        return suite;
    }

    /**
     * Test of getRelativePath method, of class util.FileHelper.
     */
    public void testGetRelativePath()
    {
        System.out.println("getRelativePath");
        
        String currentDirectory = "C:\\TestDir\\Current\\";
        String childPath = "C:\\TestDir\\Current\\ChildDir\\myfile.idl";
        
        String expResult = "\\ChildDir\\myfile.idl";
        String result = FileHelper.getRelativePath(currentDirectory, childPath);
        assertEquals(expResult, result);
        
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
    
}
