/*
 * CSCompiler.java
 *
 * Created on den 20 maj 2007, 19:06
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package idlcompiler.compilers;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Vector;

/**
 *
 * @author Anton Gravestam
 */
public class CSCompiler extends Compiler
{
    
    /** Creates a new instance of CSCompiler */
    public CSCompiler(File idlFile, File baseDir) throws FileNotFoundException, IOException
    {
        super(idlFile, baseDir, new File("cstemplate.tpl"), new File("cssubscribertemplate.tpl"), new File("cspublishertemplate.tpl"), new File("cshelpertemplate.tpl"));
        
    }
    String tab = "    ";
    String createSerialization()
    {
        
        String ret = "" + //"char* serialize()  \n" + tab + "" +
                //"{\n" + tab + tab + "" + "" +
                //"byte[] b = new byte[ops::Manager::MAX_SIZE];\n" + tab + tab +
                "OPS.WriteByteBuffer buf = new OPS.WriteByteBuffer(GetSize(o));\n" + tab + tab +
                className + " o" + className + " = (" + className + ")o;\n" + tab + tab;
        
        ret += "buf.WriteOPSObjectFields(o);\n" + tab + tab;
        
        String objectName = "o" + className + ".";
        int i = 0;
        for (String[] sArr : members)
        {
            if(sArr[0].equals("string"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("boolean"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("int"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("long"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("double"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("float"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("byte"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("string[]"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("int[]"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("long[]"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("double[]"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("float[]"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            else if(sArr[0].equals("byte[]"))
            {
                ret += "buf.Write(" + objectName + sArr[1] + ");\n" + tab + tab + "";
            }
            //assume idltype-array
            else if(sArr[0].endsWith("[]"))
            {
                ret += "buf.Write("+ objectName + sArr[1] + ".Count); \n\t";
                ret += "foreach(" + sArr[0].substring(0, sArr[0].indexOf("[]")) + "  __idltype in "+ objectName + sArr[1] + ")\n\t{\n\t\t";  
                    ret += "buf.Write(__idltype, new " + substring(sArr[0], "[]") +"Helper());\n" + tab + tab + "";
                ret += "}\n\t";
            }
            //assume idltype
            else
            {
                ret += "buf.Write(" + objectName + sArr[1] + ", new " + sArr[0] +"Helper());\n" + tab + tab + "";
            }
            
        }
        
        
        ret += "return buf.GetBytes();\n" + tab + tab;
        return ret;
    }
    
    
    
    String createDeserialization()
    {
        String ret = //"void deserialize(char* b)\n" + tab + "" +
                //"{\n" + tab + tab + "" +
                //"char* b = new char[ops::Manager::MAX_SIZE];\n" + tab + tab + "" +
                "OPS.ReadByteBuffer buf = new OPS.ReadByteBuffer(b);\n" + tab + tab + "" +
                "" + className + " o" + className + " = new " + className + "();\n" + tab + tab;
        
        ret += "buf.ReadOPSObjectFields(o" + className + ");\n" + tab + tab;
        
        String objectName = "o" + className + ".";
        
        int i = 0;
        for (String[] sArr : members)
        {
            if(sArr[0].equals("string"))
            {
                
                ret += objectName + sArr[1] + " = buf.ReadString();\n" + tab + tab + "";
                
            }
            else if(sArr[0].equals("boolean"))
            {
                ret += objectName + sArr[1] + " = buf.ReadBool();\n" + tab + tab + "";
            }
            else if(sArr[0].equals("int"))
            {
                ret += objectName + sArr[1] + " = buf.ReadInt();\n" + tab + tab + "";
            }
            else if(sArr[0].equals("long"))
            {
                ret += objectName + sArr[1] + " = buf.ReadLong();\n" + tab + tab + "";
            }
            else if(sArr[0].equals("double"))
            {
                ret += objectName + sArr[1] + " = buf.ReadDouble();\n" + tab + tab + "";
            }
            else if(sArr[0].equals("float"))
            {
                ret += objectName + sArr[1] + " = buf.ReadFloat();\n" + tab + tab + "";
            }
            else if(sArr[0].equals("byte"))
            {
                ret += objectName + sArr[1] + " = buf.ReadByte();\n" + tab + tab + "";
            }
            else if(sArr[0].equals("string[]"))
            {
                ret += objectName + sArr[1] + " = buf.ReadStrings();\n" + tab + tab + "";
                
            }
            else if(sArr[0].equals("int[]"))
            {
                ret += objectName + sArr[1] + " = buf.ReadInts();\n" + tab + tab + "";
            }
            else if(sArr[0].equals("long[]"))
            {
                ret += objectName + sArr[1] + " = buf.ReadLongs();\n" + tab + tab + "";
            }
            else if(sArr[0].equals("double[]"))
            {
                ret += objectName + sArr[1] + " = buf.ReadDoubles();\n" + tab + tab + "";
            }
            else if(sArr[0].equals("float[]"))
            {
                ret += objectName + sArr[1] + " = buf.ReadFloats();\n" + tab + tab + "";
            }
            else if(sArr[0].equals("byte[]"))
            {
                ret += objectName + sArr[1] + " = buf.ReadBytes();\n" + tab + tab + "";
            }
            else if(sArr[0].endsWith("[]"))
            {
                ret += "int size"  + sArr[1] + " = " + "buf.ReadInt();\n\t";
                ret += "for(int __i = 0 ; __i < size"  + sArr[1] + "; __i++)\n\t\t";
                ret += "{\n\t\t";
                ret += objectName + sArr[1] + ".Add((" + substring(sArr[0], "[]") +")buf.ReadOPSObject(new " + substring(sArr[0], "[]") + "Helper()));\n" + tab + tab + "";
                ret += "}\n\t";
            }
            else
            {
                ret += objectName + sArr[1] + " = (" + sArr[0] +")buf.ReadOPSObject(new " + sArr[0] + "Helper());\n" + tab + tab + "";
            }
            
            
        }
        
        ret += "return o" + className + ";";
        return ret;
        
    }
    
    protected String languageType(String s)
    {
        if(s.equals("string"))
            return "string";
        else if(s.equals("boolean"))
            return "bool";
        else if(s.equals("int"))
            return "int";
        else if(s.equals("long"))
            return "long";
        else if(s.equals("double"))
            return "double"; 
        else if(s.equals("float"))
            return "float"; 
        else if(s.equals("byte"))
            return "byte";
        else if(s.equals("string[]"))
            return "List<string>";
        else if(s.equals("int[]"))
            return "List<int>"; 
        else if(s.equals("double[]"))
            return "List<double>"; 
        else if(s.equals("float[]"))
            return "List<float>"; 
        else if(s.equals("byte[]"))
            return "List<byte>";
        else if(s.endsWith("[]"))
            return "List<" + s.substring(0, s.indexOf('[')) + ">"; 
        return s;
        
    }
    
    protected String createConstructrBody()
    {
        String ret = "";
        int i = 0;
        for (String[] sArr : members)
        {
            if (sArr[0].endsWith("[]"))
            {
                ret += "" + sArr[1] + " = new " + languageType(sArr[0]) + "();\n\t";
            }
            else if (sArr[0].equals("string"))
                ret += "" + sArr[1] + " = \"\";\n\t";
            else if (sArr[0].equals("boolean"))
                ret += "" + sArr[1] + " = false;\n\t";
            else if("double byte int float long".contains(sArr[0]))
               ret += "" + sArr[1] + " = 0;\n\t";
            //TODO: Decide if idltypes should be initialized to null to avoid cyclic memory allocations
            else
                ret += "" + sArr[1] + " = new " + sArr[0] + "();\n\t";
	
        }
        return ret;
    }
    
    protected String createDeclarations()
    {
        String ret = "";
        int i = 0;
        for (String[] sArr : members)
        {
            ret += "public " + languageType(sArr[0]) + " " + sArr[1] + ";\n\t";
        }
        return ret;
    }
    
    public String getLanguageOutputDirectory()
    {
        return "C#";
    }
    
    protected String createDestructorBody()
    {
        return null;
    }
    
    protected String createGetSizeBody()
    {
        String ret = "int i = 0;\n\t\t";
        
        ret += "" + className + " o" + className + " = (" + className + ")o;";
        
        ret += "i += GetOPSObjectFieldsSize(o" + className + ");\n\t";
        
        String objectName = "o" + className + ".";
        
        for (String[] sArr : members)
        {
            if(sArr[0].equals("string"))
            {
                ret += "i += " + objectName + sArr[1] + ".Length + 4;\n\t";
                
            }
            else if(sArr[0].equals("int"))
            {
                ret += "i += 4;\n\t";
            }
            else if(sArr[0].equals("long"))
            {
                ret += "i += 8;\n\t";
            }
            else if(sArr[0].equals("double"))
            {
                ret += "i += 8;\n\t";
            }
            else if(sArr[0].equals("float"))
            {
                ret += "i += 4;\n\t";
            }
            else if(sArr[0].equals("byte"))
            {
                ret += "i += 1;\n\t";
            }
            else if(sArr[0].equals("boolean"))
            {
                ret += "i += 1;\n\t";
            }
            
            else if(sArr[0].equals("string[]"))
            {
                ret += "i += 4;\n\t";
                ret += "foreach(string __s in "+ objectName + sArr[1] + ")\n\t{\n\t\t";
                ret += "i += __s.Length + 4; \n\t\t";
                ret += "}\n\t";
                
            }
            else if(sArr[0].equals("int[]"))
            {
                ret += "i += " + objectName + sArr[1] + ".Count*4 + 4;\n\t";
            }
            else if(sArr[0].equals("long[]"))
            {
                ret += "i += " + objectName + sArr[1] + ".Count*8 + 4;\n\t";
            }
            else if(sArr[0].equals("double[]"))
            {
                ret += "i += " + objectName + sArr[1] + ".Count*8 + 4;\n\t";
            }
            else if(sArr[0].equals("float[]"))
            {
                ret += "i += " + objectName + sArr[1] + ".Count*4 + 4;\n\t";
            }
            else if(sArr[0].equals("byte[]"))
            {
                ret += "i += " + objectName + sArr[1] + ".Count + 4;\n\t";
            }
            else if(sArr[0].equals("boolean[]"))
            {
                ret += "i += " + objectName +  sArr[1] + ".Count + 4;\n\t";
            }
            else if (sArr[0].endsWith("[]"))
            {
                ret += "i += 4;\n\t";
                ret += sArr[0].substring(0, sArr[0].indexOf("[]")) + "Helper "+ sArr[1] +"Helper = new " + sArr[0].substring(0, sArr[0].indexOf("[]")) + "Helper();\n\t\t";
                ret += "foreach(" + sArr[0].substring(0, sArr[0].indexOf("[]")) + " idltype in "+ objectName + sArr[1] + ")\n\t{\n\t\t";
                ret += "i += " + sArr[1] +"Helper.GetSize(idltype) + 4; \n\t\t";
                ret += "}\n\t";
                
            }
            
            else
            {
                ret += "i += 4;\n\t";
                String className = sArr[0];
                ret += className + "Helper "+ sArr[1] +"Helper = new " + className + "Helper();\n\t\t";
                ret += "i += " + sArr[1] +"Helper.GetSize(" + objectName + sArr[1] +");\n\t\t\t";
            }
            
            
        }
        ret += "return i;\n;";
        return ret;
    }
    
    protected String createImports()
    {
        return "";
    }

    protected String applyLanguagePackageSeparator(String packageName)
    {
        return packageName;
    }

    protected void generateAndApplyPackageAndClassName()
    {
        String langPackageName = applyLanguagePackageSeparator(packageName);
        outFileText = outFileText.replaceAll("__packageName", langPackageName);
        outFileText = outFileText.replaceAll("__className", className);
        outSubFileText = outSubFileText.replaceAll("__packageName", langPackageName);
        outSubFileText = outSubFileText.replaceAll("__className", className);
        outHelperFileText = outHelperFileText.replaceAll("__packageName", langPackageName);
        outHelperFileText = outHelperFileText.replaceAll("__className", className);
        outPubFileText = outPubFileText.replaceAll("__packageName", langPackageName);
        outPubFileText = outPubFileText.replaceAll("__className", className);
    }

    @Override
    protected String createEnumDataText(Vector<String> enumList)
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    protected String createEnumHelperText(Vector<String> enumList)
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }
    
}
