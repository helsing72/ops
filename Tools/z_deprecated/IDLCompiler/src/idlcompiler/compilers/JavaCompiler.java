/*
 * JavaCompiler.java
 *
 * Created on den 20 maj 2007, 16:17
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
public class JavaCompiler extends Compiler
{
    String tab = "    ";
    
    /** Creates a new instance of JavaCompiler */
    public JavaCompiler(File idlFile, File baseDir) throws FileNotFoundException, IOException
    {
        super(idlFile, baseDir, new File("javatemplate.tpl"), new File("javasubscribertemplate.tpl"), new File("javapublishertemplate.tpl"), new File("javahelpertemplate.tpl"));
        //super(idlFile, templateFile, subscriberFile);
        
    }
    

    String createSerialization()
    {
       
        String ret = //"public byte[] serialize() throws IOException \n" +
                     //"{\n" + tab +"" +
                     "ByteArrayOutputStream baos = new ByteArrayOutputStream();\n" + tab +"" +
                     "DataOutputStream dos = new DataOutputStream(baos);\n" + tab +"" +
                     "" + className + " o" + className + " = (" + className + ")o;\n" + tab +"";
        String objectName = "o" + className + ".";
        
        ret += "writeOPSObjectFields(dos, o" + className + ");\n" + tab +"";
        
        int i = 0;
        for (String[] sArr : members)
        {
                if(sArr[0].equals("string"))
                {
                    ret += "dos.writeInt("+ objectName + sArr[1] + ".length()); \n" + tab +"";
                    ret += "dos.writeBytes("+ objectName + sArr[1] + ");\n" + tab +"";
                       
                }
                else if(sArr[0].equals("boolean"))
                {
                    ret += "dos.write((byte)("+ objectName + sArr[1] + " ? 1 : 0));\n" + tab +"";
                }
                else if(sArr[0].equals("int"))
                {
                    ret += "dos.writeInt("+ objectName + sArr[1] + ");\n" + tab +"";
                }
                else if(sArr[0].equals("long"))
                {
                    ret += "dos.writeLong("+ objectName + sArr[1] + ");\n" + tab +"";
                }
                else if(sArr[0].equals("double"))
                {
                    ret += "dos.writeDouble("+ objectName + sArr[1] + ");\n" + tab +"";
                }
                else if(sArr[0].equals("float"))
                {
                    ret += "dos.writeFloat("+ objectName + sArr[1] + ");\n" + tab +"";
                }
                else if(sArr[0].equals("byte"))
                {
                    ret += "dos.write("+ objectName + sArr[1] + ");\n" + tab +"";
                }
                else if(sArr[0].equals("string[]"))
                {
                    ret += "dos.writeInt("+ objectName + sArr[1] + ".size()); \n" + tab +"";
                    ret += "for(String __s : "+ objectName + sArr[1] + ")\n" + tab +"{\n" + tab +"" + tab +"";
                        ret += "dos.writeInt(__s.length()); \n" + tab +"" + tab +"";
                        ret += "dos.writeBytes(__s);\n" + tab +"";
                    ret += "}\n" + tab +"";
                       
                }
                else if(sArr[0].equals("int[]"))
                {
                    ret += "dos.writeInt("+ objectName + sArr[1] + ".size()); \n" + tab +"";
                    ret += "for(Integer __i : " + objectName + sArr[1] + ")\n" + tab +"" + tab +"";
                    ret += "dos.writeInt(__i);\n" + tab +"";
                }
                else if(sArr[0].equals("long[]"))
                {
                    ret += "dos.writeInt("+ objectName + sArr[1] + ".size()); \n" + tab +"";
                    ret += "for(Long __i : " + objectName + sArr[1] + ")\n" + tab +"" + tab +"";
                    ret += "dos.writeLong(__i);\n" + tab +"";
                }
                else if(sArr[0].equals("double[]"))
                {
                    ret += "dos.writeInt("+ objectName + sArr[1] + ".size()); \n" + tab +"";
                    ret += "for(Double __d : "+ objectName + sArr[1] + ")\n" + tab +"" + tab +"";
                    ret += "dos.writeDouble(__d);\n" + tab +"";
                }
                else if(sArr[0].equals("float[]"))
                {
                    ret += "dos.writeInt("+ objectName + sArr[1] + ".size()); \n" + tab +"";
                    ret += "for(Float __f : "+ objectName + sArr[1] + ")\n" + tab +"" + tab +"";
                    ret += "dos.writeFloat(__f);\n" + tab +"";
                }
                else if(sArr[0].equals("byte[]"))
                {
                    ret += "dos.writeInt("+ objectName + sArr[1] + ".size()); \n" + tab +"";
                    ret += "for(Byte __b : "+ objectName + sArr[1] + ")\n" + tab +"" + tab +"";
                    ret += "dos.write(__b);\n" + tab +"";
                }
                else if(sArr[0].equals("boolean[]"))
                {
                    ret += "dos.writeInt("+ objectName + sArr[1] + ".size()); \n" + tab +"";
                    ret += "for(Boolean __b : "+ objectName + sArr[1] + ")\n" + tab +"" + tab +"";
                    ret += "dos.write(__b ? 1 : 0);\n" + tab +"";
                }
                //idltype-array
                else if(sArr[0].endsWith("[]"))
                {
                    ret += sArr[0].substring(0, sArr[0].indexOf("[]")) + "Helper "+ sArr[1] + "Helper = new " + sArr[0].substring(0, sArr[0].indexOf("[]")) + "Helper();\n" + tab +"" + tab +"";
                    ret += "dos.writeInt("+ objectName + sArr[1] + ".size()); \n" + tab +"";
                    ret += "for(" + sArr[0].substring(0, sArr[0].indexOf("[]")) + "  __idltype : "+ objectName + sArr[1] + ")\n" + tab +"{\n" + tab +"" + tab +"";
                        
                        ret += "dos.writeInt(" + sArr[1] + "Helper.getSize(__idltype));\n" + tab +"";
                        ret += "dos.write(" + sArr[1] + "Helper.serialize(__idltype));\n" + tab +"";
                        
                    ret += "}\n" + tab +"";
                    
                }
                //idltype
                else 
                {
                    ret += sArr[0] + "Helper "+ sArr[1] +"Helper = new " + sArr[0] + "Helper();\n" + tab +"" + tab +"";
                    ret += "dos.writeInt(" + sArr[1] +"Helper.getSize(" + objectName + sArr[1] +"));\n" + tab +"";
                    ret += "dos.write(" + sArr[1] +"Helper.serialize(" + objectName + sArr[1] +"));\n" + tab +"";
                }
            
        }
        
        ret += "return baos.toByteArray();";
        return ret;
    }
    String createDeserialization()
    {
        String ret = //"public void deserialize(byte[] __b) throws IOException\n" +
                     //"{\n" + tab +"" +
                     "ByteArrayInputStream bais = new ByteArrayInputStream(__b);\n" + tab +"" +
                     "DataInputStream dis = new DataInputStream(bais);\n" + tab +"" +
                     "" + className + " o" + className + " = new " + className + "();\n" + tab +"";
        String objectName = "o" + className + ".";
        
        ret += "readOPSObjectFields(dis, o" + className + ");\n" + tab +"";
        
        int i = 0;
        for (String[] sArr : members)
        {
                if(sArr[0].equals("string"))
                {
                    ret += "byte[] __"+ sArr[1] + "Bytes = new byte[dis.readInt()];\n" + tab +"";
                    ret += "dis.read(__"+ sArr[1] + "Bytes);\n" + tab +"";
                    ret +=objectName + sArr[1] + " = new String(__"+  sArr[1] + "Bytes);\n" + tab +"";
                       
                }
                else if(sArr[0].equals("boolean"))
                {
                    ret += objectName + sArr[1] + " = dis.readByte() > 0;\n" + tab +"";
                }
                else if(sArr[0].equals("int"))
                {
                    ret += objectName + sArr[1] + " = dis.readInt();\n" + tab +"";
                }
                else if(sArr[0].equals("long"))
                {
                    ret += objectName + sArr[1] + " = dis.readLong();\n" + tab +"";
                }
                else if(sArr[0].equals("double"))
                {
                    ret += objectName + sArr[1] + " = dis.readDouble();\n" + tab +"";
                }
                else if(sArr[0].equals("float"))
                {
                    ret += objectName + sArr[1] + " = dis.readFloat();\n" + tab +"";
                }
                else if(sArr[0].equals("byte"))
                {
                    ret += objectName + sArr[1] + " = dis.readByte();\n" + tab +"";
                }
                else if(sArr[0].equals("string[]"))
                {
                    ret += "int size"  + sArr[1] + " = " + "dis.readInt();\n" + tab +"";
                    ret += "for(int __i = 0 ; __i < size"  + sArr[1] + "; __i++)\n" + tab +"" + tab +"";
                    ret += "{\n" + tab +"" + tab +"";
                    ret += "byte[] __"+ sArr[1] + "Bytes = new byte[dis.readInt()];\n" + tab +"";
                    ret += "dis.read(__"+ sArr[1] + "Bytes);\n" + tab +"";
                    ret +=objectName + sArr[1] + ".add(new String(__"+  sArr[1] + "Bytes));\n" + tab +"";
                    ret += "}\n" + tab +"";
          
                }
                else if(sArr[0].equals("int[]"))
                {
                    ret += "int size"  + sArr[1] + " = " + "dis.readInt();\n" + tab +"";
                    ret += "for(int __i = 0 ; __i < size"  + sArr[1] + "; __i++)\n" + tab +"" + tab +"";    
                    ret += objectName + sArr[1] + ".add(dis.readInt());\n" + tab +"";
                }
                else if(sArr[0].equals("long[]"))
                {
                    ret += "int size"  + sArr[1] + " = " + "dis.readInt();\n" + tab +"";
                    ret += "for(long __i = 0 ; __i < size"  + sArr[1] + "; __i++)\n" + tab +"" + tab +"";    
                    ret += objectName + sArr[1] + ".add(dis.readLong());\n" + tab +"";
                }
                else if(sArr[0].equals("double[]"))
                {
                    ret += "int size"  + sArr[1] + " = " + "dis.readInt();\n" + tab +"";
                    ret += "for(int __i = 0 ; __i < size"  + sArr[1] + "; __i++)\n" + tab +"" + tab +"";    
                    ret += objectName + sArr[1] + ".add(dis.readDouble());\n" + tab +"";
                }
                else if(sArr[0].equals("float[]"))
                {
                    ret += "int size"  + sArr[1] + " = " + "dis.readInt();\n" + tab +"";
                    ret += "for(int __i = 0 ; __i < size"  + sArr[1] + "; __i++)\n" + tab +"" + tab +"";    
                    ret += objectName + sArr[1] + ".add(dis.readFloat());\n" + tab +"";
                }
                else if(sArr[0].equals("byte[]"))
                {
                    ret += "int size"  + sArr[1] + " = " + "dis.readInt();\n" + tab +"";
                    ret += "for(int __i = 0 ; __i < size"  + sArr[1] + "; __i++)\n" + tab +"" + tab +"";    
                    ret += objectName + sArr[1] + ".add(dis.readByte());\n" + tab +"";
                }
                else if(sArr[0].equals("boolean[]"))
                {
                    ret += "int size"  + sArr[1] + " = " + "dis.readInt();\n" + tab +"";
                    ret += "for(int __i = 0 ; __i < size"  + sArr[1] + "; __i++)\n" + tab +"" + tab +"";    
                    ret += objectName + sArr[1] + ".add(dis.readByte() > 0);\n" + tab +"";
                }
                else if(sArr[0].endsWith("[]"))
                {
                    ret += sArr[0].substring(0, sArr[0].indexOf("[]")) + "Helper "+ sArr[1] + "Helper = new " + sArr[0].substring(0, sArr[0].indexOf("[]")) + "Helper();\n" + tab +"" + tab +"";
                    ret += "int size"  + sArr[1] + " = " + "dis.readInt();\n" + tab +"";
                    ret += "for(int __i = 0 ; __i < size"  + sArr[1] + "; __i++)\n" + tab +"" + tab +"";
                    ret += "{\n" + tab +"" + tab +"";
                        ret += "byte[] "+ sArr[1] + "Bytes = new byte[dis.readInt()];\n" + tab +"";
                        ret += "dis.read("+ sArr[1] + "Bytes);\n" + tab +"";
                        ret +=objectName + sArr[1] + ".add((" + sArr[0].substring(0, sArr[0].indexOf("[]")) + ")"+ sArr[1] +"Helper.deserialize(" + sArr[1] + "Bytes));\n" + tab +"";
                    ret += "}\n" + tab +"";
          
                }
                else
                {
                    
                    ret += sArr[0] + "Helper "+ sArr[1] +"Helper = new " + sArr[0] + "Helper();\n" + tab +"" + tab +"";
                    ret += "byte[] "+ sArr[1] + "Bytes = new byte[dis.readInt()];\n" + tab +"";
                    ret += "dis.read("+ sArr[1] + "Bytes);\n" + tab +"";
                    ret +=objectName + sArr[1] + " = (" + sArr[0] + ")"+ sArr[1] + "Helper.deserialize(" + sArr[1] + "Bytes);\n" + tab +"";
                }
            
        }
        
        ret += "return o" + className + ";";
        return ret;
        
    }
    

    protected String languageType(String s)
    {
        if(s.equals("string"))
            return "String";
        else if(s.equals("boolean"))
            return "boolean";
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
            return "java.util.Vector<String>";
        else if(s.equals("int[]"))
            return "java.util.Vector<Integer>"; 
        else if(s.equals("long[]"))
            return "java.util.Vector<Long>";
        else if(s.equals("double[]"))
            return "java.util.Vector<Double>"; 
        else if(s.equals("float[]"))
            return "java.util.Vector<Float>"; 
        else if(s.equals("byte[]"))
            return "java.util.Vector<Byte>";
        else if(s.equals("boolean[]"))
            return "java.util.Vector<Boolean>";
        else if(s.endsWith("[]"))
            return "java.util.Vector<" + s.substring(0, s.indexOf('[')) + ">"; 
        else if(s.equals("static string"))
            return "final static String";
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
                ret += "" + sArr[1] + " = new " + languageType(sArr[0]) + "();\n" + tab + tab + "";
            }
            else if (sArr[0].equals("string"))
                ret += "" + sArr[1] + " = \"\";\n" + tab + tab + "";
            else if (sArr[0].equals("boolean"))
                ret += "" + sArr[1] + " = false;\n" + tab + tab + "";
            else if("double byte int float long".contains(sArr[0]))
               ret += "" + sArr[1] + " = 0;\n" + tab + tab + "";
            //TODO: Decide if idltypes should be initialized to null to avoid cyclic memory allocations
            else
                ret += "" + sArr[1] + " = new " + sArr[0] + "();\n" + tab + tab + "";
	
        }
        return ret;
    }

    protected String createDeclarations()
    {
        String ret = "";
        int i = 0;
        for (String[] sArr : members)
        {
            ret += "public " + languageType(sArr[0]) + " " + sArr[1] + ";\n" + tab + "";
        }
        return ret;
    }

    public String getLanguageOutputDirectory()
    {
        return "Java";
    }

    protected String createDestructorBody()
    {
        return "";
    }

    protected String createGetSizeBody()
    {
        String ret = "int __i = 0;\n" + tab +"" + tab +"";
        
        ret += "" + className + " o" + className + " = (" + className + ")o;\n" + tab +"";
        
        String objectName = "o" + className + ".";
        
        ret += "__i += getOPSObjectFieldsSize(o" + className + ");\n" + tab +"";
        
        for (String[] sArr : members)
        {
                if(sArr[0].equals("string"))
                {
                    ret += "__i += " + objectName + sArr[1] + ".length() + 4;\n" + tab +"";
                       
                }
                else if(sArr[0].equals("int"))
                {
                    ret += "__i += 4;\n" + tab +"";
                }
                else if(sArr[0].equals("long"))
                {
                    ret += "__i += 8;\n" + tab +"";
                }
                else if(sArr[0].equals("double"))
                {
                    ret += "__i += 8;\n" + tab +"";
                }
                else if(sArr[0].equals("float"))
                {
                    ret += "__i += 4;\n" + tab +"";
                }
                else if(sArr[0].equals("byte"))
                {
                    ret += "__i += 1;\n" + tab +"";
                }
                else if(sArr[0].equals("boolean"))
                {
                    ret += "__i += 1;\n" + tab +"";
                }
                
                else if(sArr[0].equals("string[]"))
                {
                    ret += "__i += 4;\n" + tab +"";
                    ret += "for(String __s : "+ objectName + sArr[1] + ")\n" + tab +"{\n" + tab +"" + tab +"";
                        ret += "__i += __s.length() + 4; \n" + tab +"" + tab +"";
                    ret += "}\n" + tab +"";
                       
                }
                else if(sArr[0].equals("int[]"))
                {
                    ret += "__i += " + objectName + sArr[1] + ".size()*4 + 4;\n" + tab +"";
                }
                else if(sArr[0].equals("long[]"))
                {
                    ret += "__i += " + objectName + sArr[1] + ".size()*8 + 4;\n" + tab +"";
                }
                else if(sArr[0].equals("double[]"))
                {
                    ret += "__i += " + objectName + sArr[1] + ".size()*8 + 4;\n" + tab +"";
                }
                else if(sArr[0].equals("float[]"))
                {
                    ret += "__i += " + objectName + sArr[1] + ".size()*4 + 4;\n" + tab +"";
                }
                else if(sArr[0].equals("byte[]"))
                {
                    ret += "__i += " + objectName + sArr[1] + ".size() + 4;\n" + tab +"";
                }
                else if(sArr[0].equals("boolean[]"))
                {
                    ret += "__i += " + objectName +  sArr[1] + ".size() + 4;\n" + tab +"";
                }
                else if (sArr[0].endsWith("[]"))
                {
                    ret += "__i += 4;\n" + tab +"";
                    ret += sArr[0].substring(0, sArr[0].indexOf("[]")) + "Helper "+ sArr[1] +"Helper = new " + sArr[0].substring(0, sArr[0].indexOf("[]")) + "Helper();\n" + tab +"" + tab +"";
                    ret += "for(" + sArr[0].substring(0, sArr[0].indexOf("[]")) + " idltype : "+ objectName + sArr[1] + ")\n" + tab +"{\n" + tab +"" + tab +"";
                        ret += "__i += " + sArr[1] +"Helper.getSize(idltype) + 4; \n" + tab +"" + tab +"";
                    ret += "}\n" + tab +"";
                    
                }
                //TODO: Fixa bara en typ
                else
                {
                    ret += "__i += 4;\n" + tab +"";
                    String className = sArr[0];
                    ret += className + "Helper "+ sArr[1] +"Helper = new " + className + "Helper();\n" + tab +"" + tab +"";
                    ret += "__i += " + sArr[1] +"Helper.getSize(" + objectName + sArr[1] +");\n" + tab +"" + tab +"" + tab +"";
                }
            
        }
        
        ret += "return __i;\n";
        return ret;
    }

    protected String createImports() {
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
