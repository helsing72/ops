/*
 * CppCompiler.java
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
public class CppCompiler extends Compiler
{

    String tab = "    ";

    /** Creates a new instance of JavaCompiler */
    public CppCompiler(File idlFile, File baseDir) throws FileNotFoundException, IOException
    {

        super(idlFile, baseDir, new File("cpptemplate.tpl"), new File("cppsubscribertemplate.tpl"), new File("cpppublishertemplate.tpl"), new File("cpphelpertemplate.tpl"));

    }

    String createSerialization()
    {

        String ret = "" + //"char* serialize()  \n" + tab + "" +
                //"{\n" + tab + tab + "" + "" +
                //"char* b = new char[getSize(o)];\n" + tab + tab +
                "ops::ByteBuffer buf(b);\n" + tab + tab +
                className + "* o" + className + " = (" + className + "*)o;\n" + tab + tab;

        String objectName = "o" + className + "->";

        ret += "buf.WriteOPSObjectFields(o);\n" + tab + tab;

        int i = 0;
        for (String[] sArr : members)
        {
            if (sArr[0].equals("string"))
            {

                ret += "buf.WriteString(o" + className + "->" + sArr[1] + ");\n" + tab + tab + "";

            } else if (sArr[0].equals("boolean"))
            {
                ret += "buf.WriteChar((char)(o" + className + "->" + sArr[1] + "? 1 : 0));\n" + tab + tab + "";
            } else if (sArr[0].equals("int"))
            {
                ret += "buf.WriteInt(o" + className + "->" + sArr[1] + ");\n" + tab + tab + "";
            } else if (sArr[0].equals("long"))
            {
                ret += "buf.WriteLong(o" + className + "->" + sArr[1] + ");\n" + tab + tab + "";
            } else if (sArr[0].equals("double"))
            {
                ret += "buf.WriteDouble(o" + className + "->" + sArr[1] + ");\n" + tab + tab + "";
            } else if (sArr[0].equals("float"))
            {
                ret += "buf.WriteFloat(o" + className + "->" + sArr[1] + ");\n" + tab + tab + "";
            } else if (sArr[0].equals("byte"))
            {
                ret += "buf.WriteChar(o" + className + "->" + sArr[1] + ");\n" + tab + tab + "";
            } else if (sArr[0].equals("string[]"))
            {

                ret += "buf.WriteStrings(o" + className + "->" + sArr[1] + ");\n" + tab + tab;

            } else if (sArr[0].equals("int[]"))
            {
                ret += "buf.WriteInts(o" + className + "->" + sArr[1] + ");\n" + tab + tab;

            } else if (sArr[0].equals("long[]"))
            {
                ret += "buf.WriteLongs(o" + className + "->" + sArr[1] + ");\n" + tab + tab;

            } else if (sArr[0].equals("double[]"))
            {
                ret += "buf.WriteDoubles(o" + className + "->" + sArr[1] + ");\n" + tab + tab;

            } else if (sArr[0].equals("float[]"))
            {
                ret += "buf.WriteFloats(o" + className + "->" + sArr[1] + ");\n" + tab + tab;
            } else if (sArr[0].equals("byte[]"))
            {
                ret += "buf.WriteBytes(o" + className + "->" + sArr[1] + ");\n" + tab + tab;
            } else if (sArr[0].equals("boolean[]"))
            {
                ret += "buf.WriteBooleans(o" + className + "->" + sArr[1] + ");\n" + tab + tab;
            }  //idltype-array
            else if (sArr[0].endsWith("[]"))
            {
                ret += applyLanguagePackageSeparator(substring(sArr[0], "[]")) + "Helper " + sArr[1] + "Helper ;\n\t\t";
                ret += "buf.WriteInt((int)" + objectName + sArr[1] + ".size()); \n\t\t";
                ret += "for(unsigned int j = 0; j <" + objectName + sArr[1] + ".size() ; j++)\n\t\t{\n\t\t\t";

                ret += "buf.WriteOPSObject(&" + objectName + sArr[1] + "[j], &" + applyLanguagePackageSeparator(substring(sArr[0], "[]")) + "Helper());\n\t\t";

                ret += "}\n\n\t";

            } //idltype
            else
            {
                ret += "buf.WriteOPSObject(&" + objectName + sArr[1] + ",  &" + applyLanguagePackageSeparator(sArr[0]) + "Helper());\n\n\t\t";
            }

        }

        //ret += "return b;\n" + tab + tab + "";


        return ret;
    }

    String createDeserialization()
    {
        String ret = //"void deserialize(char* b)\n" + tab + "" +
                //"{\n" + tab + tab + "" +
                //"char* b = new char[ops::Manager::MAX_SIZE];\n" + tab + tab + "" +
                "ops::ByteBuffer buf(b);\n" + tab + tab + "" +
                "" + className + "* o = new " + className + "();\n" + tab + tab;

        String objectName = "o" + className + "->";

        ret += "buf.ReadOPSObjectFields(o);\n" + tab + tab;

        int i = 0;
        for (String[] sArr : members)
        {
            if (sArr[0].equals("string"))
            {

                ret += "o->" + sArr[1] + " = buf.ReadString();\n" + tab + tab + "";

            } else if (sArr[0].equals("boolean"))
            {
                ret += "o->" + sArr[1] + " = buf.ReadChar() > 0;\n" + tab + tab + "";
            } else if (sArr[0].equals("int"))
            {
                ret += "o->" + sArr[1] + " = buf.ReadInt();\n" + tab + tab + "";
            } else if (sArr[0].equals("long"))
            {
                ret += "o->" + sArr[1] + " = buf.ReadLong();\n" + tab + tab + "";
            } else if (sArr[0].equals("double"))
            {
                ret += "o->" + sArr[1] + " = buf.ReadDouble();\n" + tab + tab + "";
            } else if (sArr[0].equals("float"))
            {
                ret += "o->" + sArr[1] + " = buf.ReadFloat();\n" + tab + tab + "";
            } else if (sArr[0].equals("byte"))
            {
                ret += "o->" + sArr[1] + " = buf.ReadChar();\n" + tab + tab + "";
            } else if (sArr[0].equals("string[]"))
            {

                ret += "buf.ReadStrings(o->" + sArr[1] + ");\n" + tab + tab;

            } else if (sArr[0].equals("int[]"))
            {
                ret += "buf.ReadInts(o->" + sArr[1] + ");\n" + tab + tab;
                
            } else if (sArr[0].equals("long[]"))
            {
                ret += "buf.ReadLongs(o->" + sArr[1] + ");\n" + tab + tab;

            } else if (sArr[0].equals("double[]"))
            {
                ret += "buf.ReadDoubles(o->" + sArr[1] + ");\n" + tab + tab;

            } else if (sArr[0].equals("float[]"))
            {
                ret += "buf.ReadFloats(o->" + sArr[1] + ");\n" + tab + tab;
            } else if (sArr[0].equals("byte[]"))
            {
                ret += "buf.ReadBytes(o->" + sArr[1] + ");\n" + tab + tab;
            } else if (sArr[0].equals("boolean[]"))
            {
                ret += "buf.ReadBooleans(o->" + sArr[1] + ");\n" + tab + tab;
            } else if (sArr[0].endsWith("[]"))
            {
                //ret += sArr[0].substring(0, sArr[0].indexOf("[]")) + "Helper "+ sArr[1] + "Helper;\n\t\t";
                ret += "int size" + sArr[1] + " = " + "buf.ReadInt();\n\t\t";
                ret += "o->" + sArr[1] + ".reserve(size" + sArr[1] + ");";
                ret += "o->" + sArr[1] + ".resize(size" + sArr[1] + ", " + applyLanguagePackageSeparator(substring(sArr[0], "[]")) + "()" + ");";
                ret += "for(int __i = 0 ; __i < size" + sArr[1] + "; __i++)\n\t\t";
                ret += "{\n\t\t";


                ret += applyLanguagePackageSeparator(substring(sArr[0], "[]")) + "* o" + sArr[1] + " = (" + applyLanguagePackageSeparator(substring(sArr[0], "[]")) + "*)buf.ReadOPSObject( &" + applyLanguagePackageSeparator(substring(sArr[0], "[]")) + "Helper());\n\t\t";
                ret += "o->" + sArr[1] + "[__i] = *o" + sArr[1] + ";\n\t\t\t";
                ret += "delete o" + sArr[1] + ";\n\t\t";

                ret += "}\n\n\t";

            } else
            {

                //HeadingData* oheadingData = (HeadingData*)buf.ReadOPSObject(new HeadingDataHelper());
                ret += applyLanguagePackageSeparator(sArr[0]) + "* o" + sArr[1] + " = (" + applyLanguagePackageSeparator(sArr[0]) + "*)buf.ReadOPSObject( &" + applyLanguagePackageSeparator(sArr[0]) + "Helper());\n\t\t";

                //o->headingDataArr.push_back(*oheadingDataArr);
                ret += "o->" + sArr[1] + " = *o" + sArr[1] + ";\n\t\t";

                ret += "delete o" + sArr[1] + ";\n\n\t\t";

            }


        }

        ret += "return o;";
        return ret;

    }

    protected String languageType(String s)
    {
        if (s.equals("string"))
        {
            return "std::string";
        } else if (s.equals("boolean"))
        {
            return "bool";
        } else if (s.equals("int"))
        {
            return "int";
        } else if (s.equals("long"))
        {
            return "__int64";
        } else if (s.equals("double"))
        {
            return "double";
        } else if (s.equals("float"))
        {
            return "float";
        } else if (s.equals("byte"))
        {
            return "char";
        } else if (s.equals("string[]"))
        {
            return "std::vector<std::string>";
        } else if (s.equals("int[]"))
        {
            return "std::vector<int>";
        } else if (s.equals("long[]"))
        {
            return "std::vector<__int64>";
        } else if (s.equals("double[]"))
        {
            return "std::vector<double>";
        } else if (s.equals("float[]"))
        {
            return "std::vector<float>";
        } else if (s.equals("byte[]"))
        {
            return "std::vector<char>";
        } else if (s.equals("boolean[]"))
        {
            return "std::vector<bool>";
        } else if (s.endsWith("[]"))
        {
            return "std::vector<" + applyLanguagePackageSeparator(s.substring(0, s.indexOf('['))) + ">";
        }
        return s;


    }

    protected boolean isBasicType(String s)
    {
        if (s.equals("string"))
        {
            return true;
        } else if (s.equals("boolean"))
        {
            return true;
        } else if (s.equals("int"))
        {
            return true;
        } else if (s.equals("long"))
        {
            return true;
        } else if (s.equals("double"))
        {
            return true;
        } else if (s.equals("float"))
        {
            return true;
        } else if (s.equals("byte"))
        {
            return true;
        }

        return false;

    }

    protected boolean isBasicVector(String s)
    {
        if (s.equals("string[]"))
        {
            return true;
        } else if (s.equals("int[]"))
        {
            return true;
        } else if (s.equals("long[]"))
        {
            return true;
        } else if (s.equals("double[]"))
        {
            return true;
        } else if (s.equals("float[]"))
        {
            return true;
        } else if (s.equals("byte[]"))
        {
            return true;
        } else if (s.equals("boolean[]"))
        {
            return true;
        }

        return false;

    }

    protected boolean isClassVector(String s)
    {
        if (!isBasicVector(s) && s.endsWith("[]"))
        {
            return true;
        }

        return false;

    }

    protected String createConstructrBody()
    {
        String ret = "\n" + tab + tab;
        int i = 0;
        for (String[] sArr : members)
        {
            if (sArr[0].equals("string"))
            {
                ret += "" + sArr[1] + " = \"\";\n" + tab + tab;
            } else if (sArr[0].endsWith("[]"))
            {
                //Do nothing
            } else if ("double byte int float boolean long".contains(sArr[0]))
            {
                ret += "" + sArr[1] + " = 0;\n" + tab + tab;
            }
        //TODO: Decide if idltypes should be initialized to null to avoid cyclic memory allocations
//            else
//                ret += "" + sArr[1] + " = new " + sArr[0] + "();\n\t";

        }
        return ret;
    }

    protected String createDeclarations()
    {
        String ret = "";
        int i = 0;
        for (String[] sArr : members)
        {
            ret += "" + applyLanguagePackageSeparator(languageType(sArr[0])) + " " + sArr[1] + ";\n" + tab + "";
        }
        return ret;
    }

    public String getLanguageOutputDirectory()
    {
        return "C++";
    }

    protected String createDestructorBody()
    {
        String ret = "";
        for (String[] sArr : members)
        {
            if (sArr[0].endsWith("[]"))
            {
                ret += tab + tab + sArr[1] + ".clear();\n";
            }
        }
        return ret;
    }

    protected String createGetSizeBody()
    {
        String ret = "int i = 0;\n\t\t\t";

        ret += "" + className + "* o" + className + " = (" + className + "*)o;";

        String objectName = "o" + className + "->";

        ret += "i += GetOPSObjectFieldsSize(o" + className + ");\n\t\t\t";

        for (String[] sArr : members)
        {
            if (sArr[0].equals("string"))
            {
                ret += "i += (int)" + objectName + sArr[1] + ".size() + 4;\n\t\t\t";

            } else if (sArr[0].equals("int"))
            {
                ret += "i += 4;\n\t\t\t";
            } else if (sArr[0].equals("long"))
            {
                ret += "i += 8;\n\t\t\t";
            } else if (sArr[0].equals("double"))
            {
                ret += "i += 8;\n\t\t\t";
            } else if (sArr[0].equals("float"))
            {
                ret += "i += 4;\n\t\t\t";
            } else if (sArr[0].equals("byte"))
            {
                ret += "i += 1;\n\t\t\t";
            } else if (sArr[0].equals("boolean"))
            {
                ret += "i += 1;\n\t\t\t";
            } else if (sArr[0].equals("string[]"))
            {
                ret += "i += 4;\n\t\t\t";
                ret += "for(unsigned int j = 0; j < " + objectName + sArr[1] + ".size(); j++)\n\t\t\t{\n\t\t\t\t";
                ret += "i += (int)" + objectName + sArr[1] + "[j].size() + 4; \n\t\t\t";
                ret += "}\n\t\t\t";

            } else if (sArr[0].equals("int[]"))
            {
                ret += "i += (int)" + objectName + sArr[1] + ".size()*4 + 4;\n\t\t\t";
            } else if (sArr[0].equals("long[]"))
            {
                ret += "i += (int)" + objectName + sArr[1] + ".size()*8 + 4;\n\t\t\t";
            } else if (sArr[0].equals("double[]"))
            {
                ret += "i += (int)" + objectName + sArr[1] + ".size()*8 + 4;\n\t\t\t";
            } else if (sArr[0].equals("float[]"))
            {
                ret += "i += (int)" + objectName + sArr[1] + ".size()*4 + 4;\n\t\t\t";
            } else if (sArr[0].equals("byte[]"))
            {
                ret += "i += (int)" + objectName + sArr[1] + ".size() + 4;\n\t\t\t";
            } else if (sArr[0].equals("boolean[]"))
            {
                ret += "i += (int)" + objectName + sArr[1] + ".size() + 4;\n\t\t\t";
            } else if (sArr[0].endsWith("[]"))
            {
                ret += "i += 4;\n\t\t\t";
                String fieldClassName = applyLanguagePackageSeparator(substring(sArr[0], "[]"));
                ret += fieldClassName + "Helper " + sArr[1] + "Helper;\n\t\t\t";
                ret += "for(unsigned int j = 0 ; j < " + objectName + sArr[1] + ".size(); j ++)\n\t\t\t{\n\t\t\t\t";
                ret += "i += " + sArr[1] + "Helper.getSize(&" + objectName + sArr[1] + "[j]) + 4; \n\t\t\t";
                ret += "}\n\t\t\t";

            } else
            {
                ret += "i += 4;\n\t\t\t";
                String fieldClassName = applyLanguagePackageSeparator(sArr[0]);
                ret += fieldClassName + "Helper " + sArr[1] + "Helper;\n\t\t\t";
                ret += "i += " + sArr[1] + "Helper.getSize(&" + objectName + sArr[1] + ");\n\t\t\t";
            }

        }

        ret += "return i;\n\t\t";
        return ret;
    }

//    protected String createXMLSerializationMethod()
//    {
//        String ret = "";
//        ret += tab + tab + "std::string output;\n";
//        for (String[] sArr : members)
//        {
//            if(isBasicType(sArr[0]))
//                ret += tab + tab + "output += XMLWriter::getXMLString(" + sArr[1] + ");\n"; 
//            else if(isBasicVector(sArr[0]))
//                ret += tab + tab + "output += XMLWriter::getXMLString(" + sArr[1] + ");\n";
//            else if(isClassVector(sArr[0]))
//                ret += tab + tab + "output += XMLWriter::getXMLString(" + sArr[1] + ", &" + sArr[0] + "Helper());\n";
//            
//        }
//        ret += tab + tab + "return output;\n";
//        
//        return ret;
//        
//        
//    }
    protected String createImports()
    {
        String ret = "";
        String alreadyIncluded = "";
        for (String[] sArr : members)
        {
            if (!("string[] byte[] int[] float[] double[] boolean[] long[]" + alreadyIncluded).contains(sArr[0]))
            {
                String plainFieldClassName = substring(sArr[0], "[]");
                plainFieldClassName = plainFieldClassName.replace(".", "/");
                String includeString = "#include \"" + plainFieldClassName + ".h\" \n#include \"" + plainFieldClassName + "Helper.h\"\n";
                alreadyIncluded += includeString;
                ret += includeString;

            }

        }

        return ret;
    }

    protected String applyLanguagePackageSeparator(String packageName)
    {
        return packageName.replace(".", "::");
    }

    protected void generateAndApplyPackageAndClassName()
    {
        String langPackageString = applyLanguagePackageSeparator(packageName);

        String packageCloserString = "";
        for (int i = 0; i < langPackageString.split("::").length - 1; i++)
        {
            packageCloserString += "\n}";

        }

        langPackageString = langPackageString.replace("::", "\n{\nnamespace ");

        outFileText = outFileText.replaceAll("__underscoredPackName", packageName.replace(".", "_"));
        outFileText = outFileText.replaceAll("__packageDeclaration", langPackageString);
        outFileText = outFileText.replaceAll("__packageName", packageName);
        outFileText = outFileText.replaceAll("__className", className);
        outFileText = outFileText.replaceAll("__packageCloser", packageCloserString);

        outSubFileText = outSubFileText.replaceAll("__underscoredPackName", packageName.replace(".", "_"));
        outSubFileText = outSubFileText.replaceAll("__packageDeclaration", langPackageString);
        outSubFileText = outSubFileText.replaceAll("__packageName", packageName);
        outSubFileText = outSubFileText.replaceAll("__className", className);
        outSubFileText = outSubFileText.replaceAll("__packageCloser", packageCloserString);

        outHelperFileText = outHelperFileText.replaceAll("__underscoredPackName", packageName.replace(".", "_"));
        outHelperFileText = outHelperFileText.replaceAll("__packageDeclaration", langPackageString);
        outHelperFileText = outHelperFileText.replaceAll("__packageName", packageName);
        outHelperFileText = outHelperFileText.replaceAll("__className", className);
        outHelperFileText = outHelperFileText.replaceAll("__packageCloser", packageCloserString);

        outPubFileText = outPubFileText.replaceAll("__underscoredPackName", packageName.replace(".", "_"));
        outPubFileText = outPubFileText.replaceAll("__packageDeclaration", langPackageString);
        outPubFileText = outPubFileText.replaceAll("__packageName", packageName);
        outPubFileText = outPubFileText.replaceAll("__className", className);
        outPubFileText = outPubFileText.replaceAll("__packageCloser", packageCloserString);
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

