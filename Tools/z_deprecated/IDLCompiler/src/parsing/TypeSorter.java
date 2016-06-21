/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package parsing;

/**
 *
 * @author angr
 */
public class TypeSorter 
{
    private IDLClass sortedClass;
    private final String supportedTypes = " boolean boolean[] byte byte[] int int[] long long[] float float[] double double[] string string[] idltype ";
    public  IDLClass resort(IDLClass inClass)
    {
        sortedClass = new IDLClass();
        sortedClass.setBaseClassName(inClass.getBaseClassName());
        sortedClass.setClassName(inClass.getClassName());
        sortedClass.setPackageName(inClass.getPackageName());
     
        findAndPut(inClass, "boolean");
        findAndPut(inClass, "double");
        findAndPut(inClass, "byte");
        findAndPut(inClass, "int");
        findAndPut(inClass, "long");
        findAndPut(inClass, "float");
        findAndPut(inClass, "string");
        
        findAndPut(inClass, "double[]");
        findAndPut(inClass, "boolean[]");
        findAndPut(inClass, "byte[]");
        findAndPut(inClass, "int[]");
        findAndPut(inClass, "float[]");
        findAndPut(inClass, "string[]");
        findAndPut(inClass, "long[]");
        
        findAndPutIDLTypes(inClass);
       
        return sortedClass;
    }

    private void findAndPut(IDLClass inClass, String string)
    {
        for (IDLField field : inClass.getFields())
        {
            if(field.getType().equals(string))
            {
                sortedClass.addIDLField(field);
            }
        }
    }

    private void findAndPutIDLTypes(IDLClass inClass)
    {
        for (IDLField field : inClass.getFields())
        {
            if(!supportedTypes.contains(field.getType()))
            {
                sortedClass.addIDLField(field);
            }
        }
    }

}
