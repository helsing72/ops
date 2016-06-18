/*
 * Compiler.java
 *
 * Created on den 20 maj 2007, 13:05
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package idlcompiler.compilers;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Vector;
import parsing.FileParser;

/**
 *
 * @author Anton Gravestam
 */
public abstract class Compiler
{

    FileInputStream fInTemplate;
    FileInputStream fInIDL;
    FileOutputStream fOut;
    String baseDirString;
    String inTemplate;
    String inIDL;
    String subscriberTemplateString;
    String helperTemplateString;
    String publisherTemplateString;
    protected String outFileText;
    protected String outSubFileText;
    protected String outPubFileText;
    protected String outHelperFileText;
    int index = 0;
    protected String packageName;
    protected String className;
    protected Vector<String[]> members = new Vector<String[]>();
    private boolean enumClass;

    //Depricated, do not use
    public Compiler(File idlFile, File templateFile, File subTemplateFile) throws FileNotFoundException, IOException
    {
        this(idlFile, new File(""), templateFile, subTemplateFile, null, null);
    }

    /** Creates a new instance of Compiler */
    public Compiler(File idlFile, File baseDir, File templateFile, File subTemplateFile, File publisherTemplateFile, File helperTemplateFile) throws FileNotFoundException, IOException
    {
        baseDirString = baseDir.getAbsolutePath() + "/";
        fInTemplate = new FileInputStream(templateFile);
        fInIDL = new FileInputStream(idlFile);
        FileInputStream fInPubTmpl = new FileInputStream(subTemplateFile);
        FileInputStream fInHelperTmpl = new FileInputStream(helperTemplateFile);
        FileInputStream fInPublisherTmpl = new FileInputStream(publisherTemplateFile);

        byte[] b = new byte[fInTemplate.available()];
        fInTemplate.read(b);
        inTemplate = new String(b);

        fInTemplate.close();

        b = new byte[fInPubTmpl.available()];
        fInPubTmpl.read(b);
        subscriberTemplateString = new String(b);

        b = new byte[fInIDL.available()];
        fInIDL.read(b);
        inIDL = new String(b).trim();
        inIDL = FileParser.filterOutComments(inIDL);

        b = new byte[fInHelperTmpl.available()];
        fInHelperTmpl.read(b);
        helperTemplateString = new String(b);

        b = new byte[fInPublisherTmpl.available()];
        fInPublisherTmpl.read(b);
        publisherTemplateString = new String(b);




    }

    public String compile(String extension) throws FileNotFoundException, IOException
    {
        outFileText = inTemplate;
        outSubFileText = subscriberTemplateString;
        outHelperFileText = helperTemplateString;
        outPubFileText = publisherTemplateString;

        parsePackageAndClassName();

        String createdFiles = "";

        if (this.enumClass)
        {
            Vector<String> enumList = parseEnumClass();

            String enumDataText = createEnumDataText(enumList);
            String enumHelperText = createEnumHelperText(enumList);


        }
        else
        {

            parseIDLBody();

            generateAndApplyPackageAndClassName();

            generateAndApplyCodeBodies();

            String outputDirectoryPath = baseDirString + "" + getLanguageOutputDirectory() + "/" + createPackagePath() + "/";

            File dir = new File(outputDirectoryPath);
            dir.mkdirs();



            String dataFilePath = outputDirectoryPath + className + "." + extension;
            createdFiles += "\"" + dataFilePath.replace("\\", "/") + "\"\n";
            createAndWriteFile(dataFilePath, outFileText);

            String subscriberFilePath = outputDirectoryPath + className + "Subscriber." + extension;
            createdFiles += "\"" + subscriberFilePath.replace("\\", "/") + "\"\n";
            createAndWriteFile(subscriberFilePath, outSubFileText);

            String publisherFilePath = outputDirectoryPath + className + "Publisher." + extension;
            createdFiles += "\"" + publisherFilePath.replace("\\", "/") + "\"\n";
            createAndWriteFile(publisherFilePath, outPubFileText);

            String helperFilePath = outputDirectoryPath + className + "Helper." + extension;
            createdFiles += "\"" + helperFilePath.replace("\\", "/") + "\"\n";
            createAndWriteFile(helperFilePath, outHelperFileText);

        }

        return createdFiles;




    }

    protected abstract String createEnumDataText(Vector<String> enumList);

    protected abstract String createEnumHelperText(Vector<String> enumList);

    private Vector<String> parseEnumClass()
    {
        Vector<String> fields = new Vector<String>();
        String subs = inIDL.substring(inIDL.indexOf("{") + 1, inIDL.indexOf("}"));

        String[] fieldWords = subs.split(",");
        for (int i = 0; i < fieldWords.length; i++)
        {
            String string = fieldWords[i];
            fields.add(string.trim());

        }
        return fields;


    }

    private void parsePackageAndClassName()
    {

        packageName = readValue("package");

        className = readDeclaration("class");
        if (className == null)
        {
            className = readDeclaration("enum");
            if (className == null)
            {
                throw new RuntimeException();
            }
            else
            {
                enumClass = true;
            }
        }

    }

    private void generateAndApplyCodeBodies()
    {

        //String constructorParameters = createConstructrParameters();
        String constructorBody = createConstructrBody();
        String destructorBody = createDestructorBody();
        String declarations = createDeclarations();
        String serialization = createSerialization();
        String deserialization = createDeserialization();
        String getSizeBody = createGetSizeBody();
        String imports = createImports();

        //outFileText = outFileText.replaceAll("__constructorParameters", constructorParameters);

        outFileText = outFileText.replaceAll("__constructorBody", constructorBody);

        outFileText = outFileText.replaceAll("__destructorBody", destructorBody);

        outFileText = outFileText.replaceAll("__declarations", declarations);

        //outFileText = outFileText.replaceAll("__serialize", serialization);

        //outFileText = outFileText.replaceAll("__deserialize", deserialization);

        outFileText = outFileText.replaceAll("__imports", imports);

        outHelperFileText = outHelperFileText.replaceAll("__serialize", serialization);

        outHelperFileText = outHelperFileText.replaceAll("__deserialize", deserialization);

        outHelperFileText = outHelperFileText.replaceAll("__size", getSizeBody);

        outHelperFileText = outHelperFileText.replaceAll("__imports", imports);
    }

    protected abstract void generateAndApplyPackageAndClassName();

    private void writeFile(final File outFile, final String fileText) throws FileNotFoundException, IOException
    {

        outFile.createNewFile();

        FileOutputStream fos = new FileOutputStream(outFile);

        fos.write(fileText.getBytes());
        fos.close();
    }

    abstract String createSerialization();

    abstract String createDeserialization();

    protected abstract String languageType(String s);

    private String readValue(String string)
    {
        int tIndex;
        tIndex = inIDL.indexOf(string, index);

        if (tIndex == -1)
        {
            return null;
        }

        index = tIndex + string.length();

        return inIDL.substring(index, inIDL.indexOf(';', index)).trim();


    }

    private String readDeclaration(String string)
    {
        int tIndex;
        tIndex = inIDL.indexOf(string, index);
//        if (tIndex == -1)
//        {
//            tIndex = inIDL.indexOf("\n" + string + "\n", index);
//        }
//        if (tIndex == -1)
//        {
//            tIndex = inIDL.indexOf("\n" + string + "\t", index);
//        }
//        if (tIndex == -1)
//        {
//            tIndex = inIDL.indexOf("\n" + string + "{", index);
//        }
//        if (tIndex == -1)
//        {
//            tIndex = inIDL.indexOf(" " + string + " ", index);
//        }
//        if (tIndex == -1)
//        {
//            tIndex = inIDL.indexOf(" " + string + "\n", index);
//        }
//        if (tIndex == -1)
//        {
//            tIndex = inIDL.indexOf(" " + string + "\t", index);
//        }     
//        if (tIndex == -1)
//        {
//            tIndex = inIDL.indexOf(" " + string + "{", index);
//        }   
//        if (tIndex == -1)
//        {
//            tIndex = inIDL.indexOf("\t" + string + "\n", index);
//        }
//        if (tIndex == -1)
//        {
//            tIndex = inIDL.indexOf("\t" + string + " ", index);
//        }
//        if (tIndex == -1)
//        {
//            tIndex = inIDL.indexOf("\t" + string + "t", index);
//        }
//        if (tIndex == -1)
//        {
//            tIndex = inIDL.indexOf("\t" + string + "{", index);
//        }
        if (tIndex == -1)
        {
            return null;
        }

        index = tIndex + 1 + string.length();

        return inIDL.substring(index, inIDL.indexOf('{', index)).trim();
    }

    private void parseNextDeclaration(String typeString)
    {
        int saveIndex = index;
        String s = null;
        String[] sArr = new String[2];
        while (true)
        {

            s = readValue(typeString + " ");

            if (s == null)
            {
                break;
            }
            sArr[0] = typeString;
            sArr[1] = s;

            members.add(sArr.clone());

        }

        index = saveIndex;

    }

    private String findNewIDLType()
    {
        String[] sArr = new String[2];
        int tIndex;
        tIndex = inIDL.indexOf("idltype", index);

        if (tIndex == -1)
        {
            return null;
        }

        index = tIndex + "idltype".length() + 1;
        while (inIDL.charAt(index) == ' ')
        {
            index++;
        }

        return inIDL.substring(index, inIDL.indexOf(" ", index)).trim();

    }

    private void parseIDLTypeDeclarations()
    {
        int saveIndex = index;
        String s = null;
        String[] sArr = new String[2];
        while (true)
        {
            String typeString = findNewIDLType();
            if (typeString == null)
            {
                break;
            }

            s = readValue(typeString + " ");

            if (s == null)
            {
                break;
            }
            sArr[0] = typeString;
            sArr[1] = s;
            members.add(sArr.clone());

        }

        index = saveIndex;
    }

    private void parseIDLBody()
    {
        int saveIndex = index;

        String s = null;


        String[] sArr = new String[2];

        parseNextDeclaration("boolean");
        parseNextDeclaration("double");
        parseNextDeclaration("byte");
        parseNextDeclaration("int");
        parseNextDeclaration("long");
        parseNextDeclaration("float");
        parseNextDeclaration("string");
        parseNextDeclaration("double[]");
        parseNextDeclaration("boolean[]");
        parseNextDeclaration("byte[]");
        parseNextDeclaration("int[]");
        parseNextDeclaration("float[]");
        parseNextDeclaration("string[]");
        parseNextDeclaration("long[]");
        //parseNextDeclaration("static string");


        parseIDLTypeDeclarations();



    }

//    private String createConstructrParameters()
//    {
//        String ret = "";
//        int i = 0;
//        for (String[] sArr : members)
//        {
//            if(i == 0)
//                ret += languageType(sArr[0]) + " " + sArr[1];
//            else
//                ret += ", " + languageType(sArr[0]) + " " + sArr[1];
//
//            i++;
//        }
//
//        return ret;
//
//    }
    protected abstract String createConstructrBody();

    protected abstract String createDestructorBody();

    protected abstract String createDeclarations();

    protected abstract String createGetSizeBody();

    protected abstract String createImports();

    public abstract String getLanguageOutputDirectory();

    public static String substring(String s, String endString)
    {
        String ret = "";
        try
        {
            ret = s.substring(0, s.indexOf(endString));

        }
        catch (java.lang.StringIndexOutOfBoundsException stringIndexOutOfBoundsException)
        {
            ret = s;
        }


        return ret;
    }

    private String createPackagePath()
    {
        String s = packageName.replace(".", "/");
        return s;
    }

    public static void createAndWriteFile(String outFilePath, String outFileText) throws IOException
    {
        File outFile = new File(outFilePath);

        outFile.createNewFile();


        FileOutputStream fos = new FileOutputStream(outFile);

        fos.write(outFileText.getBytes());
        fos.close();
    }

    protected abstract String applyLanguagePackageSeparator(String packageName);
}
