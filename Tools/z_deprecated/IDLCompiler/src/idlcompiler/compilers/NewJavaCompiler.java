/*
 * NewJavaCompiler.java
 *
 * Created on den 12 november 2007, 15:39
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package idlcompiler.compilers;

import java.util.Vector;
import parsing.AbstractTemplateBasedIDLCompiler;
import parsing.IDLClass;
import parsing.IDLField;
import parsing.TopicInfo;

/**
 *
 * @author angr
 */
public class NewJavaCompiler extends AbstractTemplateBasedIDLCompiler//implements IDLCompiler
{
    final static String CONSTRUCTOR_BODY_REGEX = "__constructorBody";
    final static String DECLARATIONS_REGEX = "__declarations";
    final static String SERIALIZE_REGEX = "__serialize";
    final static String DESERIALIZE_REGEX = "__deserialize";
    final static String SIZE_REGEX = "__size";
    final static String JAVA_DIR = "Java";
    Vector<IDLClass> idlClasses;
    private String projectDirectory;

    public void compileDataClasses(Vector<IDLClass> idlClasses, String projectDirectory)
    {
        this.idlClasses = idlClasses;
        this.projectDirectory = projectDirectory;
        for (IDLClass iDLClass : idlClasses)
        {
            compileDataClass(iDLClass);
            compileSubscriber(iDLClass);
            compilePublisher(iDLClass);
            compileHelper(iDLClass);
        }

    }

    public void compileTopicConfig(Vector<TopicInfo> topics, String name, String packageString, String projectDirectory)
    {

    }

    void compileDataClass(IDLClass idlClass)
    {
        String className = idlClass.getClassName();
        String packageName = idlClass.getPackageName();

        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(projectDirectory + JAVA_DIR + "/" + packageFilePart + "/" + className + ".java");
        setTemplateFileName("javatemplate.tpl");
        setTabString("    ");//Default is "\t"
        setEndlString("\n");//Default is "\r\n"

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(CONSTRUCTOR_BODY_REGEX, getConstructorBody(idlClass));
        templateText = templateText.replace(DECLARATIONS_REGEX, getDeclarations(idlClass));


        //Save the modified text to the output file.
        saveOutputText(templateText);

    }

    public String getName()
    {
        return "CppFactoryIDLCompiler";
    }

    private void compileHelper(IDLClass idlClass)
    {
        String className = idlClass.getClassName();
        String packageName = idlClass.getPackageName();

        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(projectDirectory + JAVA_DIR + "/" + packageFilePart + "/" + className + "Helper.java");
        setTemplateFileName("javahelpertemplate.tpl");
        setTabString("    ");//Default is "\t"
        setEndlString("\n");//Default is "\r\n"

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(DESERIALIZE_REGEX, getDeserialize(idlClass));
        templateText = templateText.replace(SERIALIZE_REGEX, getSerialize(idlClass));
        templateText = templateText.replace(SIZE_REGEX, getSize(idlClass));

        //Save the modified text to the output file.
        saveOutputText(templateText);
    }

    private void compilePublisher(IDLClass idlClass)
    {
        String className = idlClass.getClassName();
        String packageName = idlClass.getPackageName();

        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(projectDirectory + JAVA_DIR + "/" + packageFilePart + "/" + className + "Publisher.java");
        setTemplateFileName("javapublishertemplate.tpl");
        setTabString("    ");//Default is "\t"
        setEndlString("\n");//Default is "\r\n"

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);

        //Save the modified text to the output file.
        saveOutputText(templateText);
    }

    private void compileSubscriber(IDLClass idlClass)
    {
        String className = idlClass.getClassName();
        String packageName = idlClass.getPackageName();

        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(projectDirectory + JAVA_DIR + "/" + packageFilePart + "/" + className + "Subscriber.java");
        setTemplateFileName("javasubscribertemplate.tpl");
        setTabString("    ");//Default is "\t"
        setEndlString("\n");//Default is "\r\n"

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);

        //Save the modified text to the output file.
        saveOutputText(templateText);

    }

    private String getConstructorBody(IDLClass idlClass)
    {
        String ret = "";
        return ret;
    }

    private String getDeclarations(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields())
        {
            if(!field.getComment().equals(""))
                ret += tab(1) + "///" + field.getComment() + endl();
            if(field.isArray())
            {
                ret += tab(1) + "public " + getDeclareVector(field);
            }
            else if(field.getType().equals("string"))
            {
                    ret += tab(1) + "public " + languageType(field.getType()) + " " + field.getName() + " = \"\";" + endl();
            }
            else if(field.isIdlType())
            {
                    ret += tab(1) + "public " + languageType(field.getType()) + " " + field.getName() + " = new " + languageType(field.getType()) + "();" + endl();
            }
            else //Simple primitive type
            {
                    ret += tab(1) + "public " + languageType(field.getType()) + " " + field.getName() + ";" + endl();
            }

        }
        return ret;
    }

    private String getDeclareVector(IDLField field)
    {
        return languageType(field.getType()) + " " + field.getName() + " = new " + languageType(field.getType()) + "();" + endl();
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

    private String getDeserialize(IDLClass idlClass)
    {
        String ret = "";

        ret += tab(2) + "ReadByteBuffer buf = new ReadByteBuffer(new DataInputStream(new ByteArrayInputStream(__b)));" + endl();
        ret += tab(2) + idlClass.getClassName() + " o = new " + idlClass.getClassName()+ "();" + endl();
        for (IDLField field : idlClass.getFields())
        {

            if(field.isIdlType())
            {
                ret += tab(2) +  idltypeReader(field) + ";" + endl();
            }
            else //Simple primitive type
            {
                ret += tab(2) + "o." + field.getName() + " = buf." + primitiveReader(field) + ";" + endl();
            }

        }
        ret += tab(2) + "return o;" + endl();
        return ret;
    }

    private String getSerialize(IDLClass idlClass)
    {
        String ret = "";

        ret += tab(2) + "WriteByteBuffer buf = new WriteByteBuffer();" + endl();
        ret += tab(2) + idlClass.getClassName() + " oNarr = (" + idlClass.getClassName()+ ")o;" + endl();
        for (IDLField field : idlClass.getFields())
        {
            if(field.isIdlType())
            {
                ret += tab(2) +  idltypeWriter(field) + ";" + endl();
            }
            else //Simple primitive type
            {
                ret += tab(2) + "buf." + primitiveWriter(field) + ";" + endl();
            }

        }
        ret += tab(2) + "return buf.getBytes();" + endl();
        return ret;
    }

    private String getSize(IDLClass idlClass)
    {
        int constSize = 0;
        String ret = tab(2) + "int i = 0;" + endl();
        ret += tab(2) + idlClass.getClassName() + " oNarr = (" + idlClass.getClassName()+ ")o;" + endl();
        for (IDLField field : idlClass.getFields())
        {
            if(!field.isArray() && !field.isIdlType() && !field.getType().replace("[]", "").equals("string"))
            {
                constSize += getPrimitiveSize(field);
            }
            else if(field.isIdlType())
            {
                if(field.isArray())
                {
                    ret += tab(2) + "i += 4;" + endl();
                    ret += tab(2) + "for(int j = 0; j < oNarr." + field.getName() + ".size(); j++ )" + endl();
                    ret += tab(3) + "i += 4 + new " + field.getType().replace("[]", "") + "Helper().getSize(oNarr." + field.getName() + ".get(j));" + endl() + endl();
                }
                else
                {
                    ret += tab(2) + "i += 4 + new " + field.getType() + "Helper().getSize(oNarr." + field.getName() + ");" + endl();
                }
            }
            else if (field.getType().replace("[]", "").equals("string"))
            {
                if(field.isArray())
                {
                    ret += tab(2) + "i += 4;" + endl();
                    ret += tab(2) + "for(int j = 0; j < oNarr." + field.getName() + ".size(); j++ )" + endl();
                    ret += tab(3) + "i += oNarr." + field.getName() + ".get(j).length() + 4;" + endl() + endl();
                }
                else
                {
                    ret += tab(2) + "i += oNarr." + field.getName() + ".length() + 4;" + endl();
                }

            }
            else if (field.isArray())
            {
                ret += tab(2) + "i += 4 + oNarr." + field.getName() + ".size() * " + getPrimitiveSize(field) + ";" + endl();
            }
        }

        ret += tab(2) + "i += " + constSize + ";" + endl();

        ret += tab(2) + "return i;" + endl();
        return ret;
    }

    private String idltypeReader(IDLField field)
    {
        if(field.isArray())
        {
            return  "buf.readOPSObjectArr(o." + field.getName() + ", new " + field.getType().replace("[]", "") + "Helper())";
        }
        else
        {
            return "o." + field.getName() + " = " + "("+ field.getType().replace("[]", "") + ")buf." + "readOPSObject(new " + field.getType() + "Helper())";
        }
    }

    private String idltypeWriter(IDLField field)
    {
        if(field.isArray())
        {
            return "buf.writeOPSArr(oNarr." + field.getName() + ", new " + field.getType().replace("[]", "") + "Helper())";
        }
        else
        {
            return "buf.write(oNarr." + field.getName() + ", new " + field.getType().replace("[]", "") + "Helper())";
        }
    }

    private String primitiveReader(IDLField field)
    {
        if(field.isArray())
        {
            return "read" + field.getType().replace("[]", "Arr()");
        }
        else
        {
            return "read" + field.getType() + "()";
        }
    }

    private String primitiveWriter(IDLField field)
    {
        if(field.isArray())
        {
            return "write" + field.getType().replace("[]", "Arr(oNarr.") + field.getName() + ")" ;
        }
        else
        {
            return "write(oNarr." + field.getName() + ")";
        }
    }

    private int getPrimitiveSize(IDLField field)
    {
        String type = field.getType().replace("[]", "");
        if(type.equals("boolean"))
        {
            return 1;
        }
        if(type.equals("byte"))
        {
            return 1;
        }
        if(type.equals("int"))
        {
            return 4;
        }
        if(type.equals("long"))
        {
            return 8;
        }
        if(type.equals("float"))
        {
            return 4;
        }
        if(type.equals("double"))
        {
            return 8;
        }
        throw new RuntimeException("Unexpected primitive type found when compiling to Java, compilation will fail." + type);
    }

}
