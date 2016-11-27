/** -*-java-*- Copyright (C) 2014 Saab Dynamics AB
 ***************************************************************************
 *  @file   opsc/CppCompiler.java
 *  @author Mattias Helsing <mattias.helsing@saabgroup.com>
 *
 * This file is based on:
 *   Tools/NBOPSIDLSupport/src/ops/netbeansmodules/idlsupport/CppCompiler.java
 * It is a complete copy of the said file but with dependencies to NB
 * completely removed. I have also added support for runtime configuration
 * of where output should ge and where templates can be found
 *
 ***************************************************************************
 */

package opsc;

import java.io.IOException;
import java.io.InputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Vector;
import parsing.AbstractTemplateBasedIDLCompiler;
import parsing.IDLClass;
import parsing.IDLField;
import parsing.TopicInfo;

/**
 *
 * @author angr
 */
public class CppCompiler extends opsc.Compiler
{

    final static String CONSTRUCTOR_BODY_REGEX = "__constructorBody";
    final static String CONSTRUCTOR_HEAD_REGEX = "__constructorHead";
    final static String DESTRUCTOR_BODY_REGEX = "__destructorBody";
    final static String DECLARATIONS_REGEX = "__declarations";
    final static String SERIALIZE_REGEX = "__serialize";
    final static String CLONE_REGEX = "__clone";
    final static String FILL_CLONE_REGEX = "__fillClone";
    final static String UNDERSCORED_PACK_NAME_REGEX = "__underscoredPackName";
    final static String PACKAGE_DECLARATION_REGEX = "__packageDeclaration";
    final static String PACKAGE_CLOSER_REGEX = "__packageCloser";
    final static String IMPORTS_REGEX = "__imports";
    final static String CLASS_COMMENT_REGEX = "__classComment";
    final static String SIZE_REGEX = "__size";
    private static String BASE_CLASS_NAME_REGEX = "__baseClassName";
    private static String CREATE_BODY_REGEX = "__createBody";

    private String projectDirectory;
    String createdFiles = "";

    public CppCompiler(String projectName) {
        super(projectName);
    }

    public void compileDataClasses(Vector<IDLClass> idlClasses, String projectDirectory)
    {
        try {
            this.projectDirectory = projectDirectory;
            for (IDLClass iDLClass : idlClasses) {
                compileIDLClass(iDLClass);
            }

            compileTypeSupport(_idlClasses, _projectName);

        } catch (IOException iOException) {
            System.out.println("Generating C++ failed with the following exception: " + iOException.getMessage());
        }
    }

    public void compileTypeSupport() {
        try {
            compileTypeSupport(_idlClasses, _projectName);
        } catch (IOException ioe) {
            System.out.println("Generating C++ Factory code failed with the following exception: " + ioe.getMessage());
        }
    }

    public void compileIDLClass(IDLClass idlClass)
    {
        try {
            if (idlClass.getType() == IDLClass.ENUM_TYPE) {
                // System.out.println("Compile enum");
                compileEnum(idlClass);
            } else {
                compileDataClass(idlClass);
                compileSubscriber(idlClass);
                compilePublisher(idlClass);
            }

            _idlClasses.add(idlClass);

        } catch (IOException ioe) {
            System.out.println("Failed to generate code for " + idlClass.getClassName());
            System.out.println("  Generating C++ failed with the following exception: " + ioe.getMessage());
        }
    }

    public void compileTopicConfig(Vector<TopicInfo> topics, String name, String packageString, String projectDirectory)
    {
    }

    public void compileDataClass(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();
        String baseClassName = "ops::OPSObject";
        if (idlClass.getBaseClassName() != null) {
            baseClassName = idlClass.getBaseClassName();
        }
        String packageName = idlClass.getPackageName();

        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + className + ".h");

        java.io.InputStream stream = findTemplateFile("cpptemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(CLASS_COMMENT_REGEX, "");
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(BASE_CLASS_NAME_REGEX, applyLanguagePackageSeparator(baseClassName));
        templateText = templateText.replace(UNDERSCORED_PACK_NAME_REGEX, getUnderscoredPackName(packageName));
        templateText = templateText.replace(PACKAGE_DECLARATION_REGEX, getPackageDeclaration(packageName));
        templateText = templateText.replace(PACKAGE_CLOSER_REGEX, getPackageCloser(packageName));
        templateText = templateText.replace(IMPORTS_REGEX, getImports(idlClass));
        templateText = templateText.replace(CONSTRUCTOR_HEAD_REGEX, getConstructorHead(idlClass));
        templateText = templateText.replace(CONSTRUCTOR_BODY_REGEX, getConstructorBody(idlClass));
        templateText = templateText.replace(DESTRUCTOR_BODY_REGEX, getDestructorBody(idlClass));
        templateText = templateText.replace(DECLARATIONS_REGEX, getDeclarations(idlClass));
        templateText = templateText.replace(SERIALIZE_REGEX, getSerialize(idlClass));
        templateText = templateText.replace(CLONE_REGEX, getClone(idlClass));
        templateText = templateText.replace(FILL_CLONE_REGEX, getFillClone(idlClass));

        //Save the modified text to the output file.
        saveOutputText(templateText);

        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    /**
     * @override
     * */
    protected void saveOutputText(String templateText)
    {
        FileOutputStream fos = null;
        try {
            //System.out.println(">>>>: " + outputFileName);
            File outFile = new File(outputFileName);
            //System.out.println(">>>>: " + outFile.getAbsolutePath());
            File outFilePath = new File(outputFileName.substring(0, outputFileName.lastIndexOf(File.separator)));
            //System.out.println(">>>>: " + outFilePath.getAbsolutePath());

            outFilePath.mkdirs();
            outFile.createNewFile();

            fos = new FileOutputStream(outFile);
            fos.write(templateText.getBytes());
        } catch (IOException ex) {
            //System.out.println(">>>>: IOException");
            Logger.getLogger(AbstractTemplateBasedIDLCompiler.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            try {
                fos.close();
            } catch (IOException ex) {
                //Logger.getLogger(CppFactoryIDLCompiler.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    protected void compileEnum(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();

        String packageName = idlClass.getPackageName();
        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + className + ".h");

        java.io.InputStream stream = findTemplateFile("cppenumtemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        String templateText = getTemplateText();
        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(CLASS_COMMENT_REGEX, "");
        templateText = templateText.replace(PACKAGE_DECLARATION_REGEX, getPackageDeclaration(packageName));
        templateText = templateText.replace(PACKAGE_CLOSER_REGEX, getPackageCloser(packageName));
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(UNDERSCORED_PACK_NAME_REGEX, getUnderscoredPackName(packageName));

        templateText = templateText.replace(DECLARATIONS_REGEX, getEnumDeclarations(idlClass));

        //Save the modified text to the output file.
        saveOutputText(templateText);
        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    private String getEnumDeclarations(IDLClass idlClass) {
        String ret = "";
        for (int i = 0; i < idlClass.getEnumNames().size(); i++) {
            ret += tab(1) + "const static int " + idlClass.getEnumNames().get(i) + " = " + i + ";" + endl();
        }
        ret += tab(1) + "const static int UNDEFINED = " + idlClass.getEnumNames().size() + ";" + endl();
        return ret;
    }

    public String getName() {
        return "CppCompiler";
    }

    protected void compilePublisher(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();
        String packageName = idlClass.getPackageName();

        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + className + "Publisher.h");

        java.io.InputStream stream = findTemplateFile("cpppublishertemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(UNDERSCORED_PACK_NAME_REGEX, getUnderscoredPackName(packageName));
        templateText = templateText.replace(PACKAGE_DECLARATION_REGEX, getPackageDeclaration(packageName));
        templateText = templateText.replace(PACKAGE_CLOSER_REGEX, getPackageCloser(packageName));

        //Save the modified text to the output file.
        saveOutputText(templateText);

        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    protected void compileSubscriber(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();
        String packageName = idlClass.getPackageName();

        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + className + "Subscriber.h");

        java.io.InputStream stream = findTemplateFile("cppsubscribertemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(UNDERSCORED_PACK_NAME_REGEX, getUnderscoredPackName(packageName));
        templateText = templateText.replace(PACKAGE_DECLARATION_REGEX, getPackageDeclaration(packageName));
        templateText = templateText.replace(PACKAGE_CLOSER_REGEX, getPackageCloser(packageName));

        //Save the modified text to the output file.
        saveOutputText(templateText);

        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    protected void compileTypeSupport(Vector<IDLClass> idlClasses, String projectName) throws IOException
    {
        String className = projectName + "TypeFactory";
        String packageName = projectName;

        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(_outputDir + File.separator + projectName + File.separator + className + ".h");

        java.io.InputStream stream = findTemplateFile("cpptypefactorytemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(UNDERSCORED_PACK_NAME_REGEX, getUnderscoredPackName(packageName));
        templateText = templateText.replace(PACKAGE_DECLARATION_REGEX, getPackageDeclaration(packageName));
        templateText = templateText.replace(PACKAGE_CLOSER_REGEX, getPackageCloser(packageName));

        //Save the modified text to the output file.

        String createBodyText = "";
        String includes = "";

        for (IDLClass iDLClass : idlClasses) {
            createBodyText += tab(2) + "if(type == \"" + iDLClass.getPackageName() + "." + iDLClass.getClassName() + "\")" + endl();
            createBodyText += tab(2) + "{" + endl();
            createBodyText += tab(3) +   "return new " + applyLanguagePackageSeparator(iDLClass.getPackageName()) + "::" + iDLClass.getClassName() + "();" + endl();
            createBodyText += tab(2) + "}" + endl();

            includes += tab(0) + "#include \"" + getSlashedType(iDLClass.getPackageName()) + "/" + getSlashedType(iDLClass.getClassName()) + ".h\"" + endl();
        }
        createBodyText += tab(2) + "return NULL;" + endl();

        templateText = templateText.replace(CREATE_BODY_REGEX, createBodyText);
        templateText = templateText.replace(IMPORTS_REGEX, includes);
        saveOutputText(templateText);

        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    private String elementType(String type)
    {
        return languageType(type.replace("[]", ""));
    }

    private String extractProjectName(String projectDirectory)
    {
        String projectName = projectDirectory.substring(0, projectDirectory.lastIndexOf("/Generated/"));
        projectName = projectDirectory.substring(projectName.lastIndexOf("/") + 1, projectName.length());
        return projectName;
    }

    private String getClone(IDLClass idlClass)
    {
        String ret = tab(2) + idlClass.getClassName() + "* ret = new " + languageType(idlClass.getClassName()) + ";" + endl();
        ret += tab(2) + "this->fillClone(ret);" + endl();
        ret += tab(2) + "return ret;" + endl();

        return ret;
    }

    private String getFillClone(IDLClass idlClass)
    {
        String ret = tab(2) + idlClass.getClassName() + "* narrRet = (" + languageType(idlClass.getClassName()) + "*)obj;" + endl();

        if (idlClass.getBaseClassName() != null) {
            ret += tab(2) + languageType(idlClass.getBaseClassName()) + "::fillClone(narrRet);" + endl();
        } else {
            ret += tab(2) + "ops::OPSObject::fillClone(narrRet);" + endl();
        }
        for (IDLField field : idlClass.getFields()) {
            if (field.isIdlType()) {
                if (!field.isArray()) {
                    if (field.isAbstract()) {
                        ret += tab(2) + "if(narrRet->" + field.getName() + ") delete narrRet->" + field.getName() + ";" + endl();
                        ret += tab(2) + "narrRet->" + field.getName() + " = (" + languageType(field.getType()) + "*)" + field.getName() + "->clone();" + endl();
                    } else {
                        ret += tab(2) + "narrRet->" + field.getName() + " = " + field.getName() + ";" + endl();
                    }
                } else {
                    // isArray()
                    if (!field.isAbstract()) {
                        if (field.getArraySize() > 0) {
                            ret += tab(2) + "for(unsigned int __i = 0; __i < " + field.getArraySize() + "; __i++) {" + endl();
                            ret += tab(3) +   "narrRet->" + field.getName() + "[__i] = " + field.getName() + "[__i];" + endl();
                            ret += tab(2) + "}" + endl();
                        } else {
                            ret += tab(2) + "narrRet->" + field.getName() + " = " + field.getName() + ";" + endl();
                        }
                    } else {
                        if (field.getArraySize() > 0) {
                            ret += tab(2) + "for(unsigned int __i = 0; __i < " + field.getArraySize() + "; __i++) {" + endl();
                            ret += tab(3) +   "if(" + "narrRet->" + field.getName() + "[__i])" + " delete " + "narrRet->" + field.getName() + "[__i];" + endl();
                            ret += tab(3) +   "narrRet->" + field.getName() + "[__i] = " + "(" + elementType(field.getType()) + "*)" + field.getName() + "[__i]->clone();" + endl();
                            ret += tab(2) + "}" + endl();
                        } else {
                            ret += tab(2) + "for(unsigned int __i = 0; __i < " + "" + field.getName() + ".size(); __i++) {" + endl();
                            ret += tab(3) +   "if(" + "narrRet->" + field.getName() + ".size() >= __i + 1) {" + endl();
                            ret += tab(4) +     "if(" + "narrRet->" + field.getName() + "[__i])" + " delete " + "narrRet->" + field.getName() + "[__i];" + endl();
                            ret += tab(4) +     "narrRet->" + field.getName() + "[__i] = " + "(" + elementType(field.getType()) + "*)" + field.getName() + "[__i]->clone();" + endl();
                            ret += tab(3) +   "} else {" + endl();
                            ret += tab(4) +     "narrRet->" + field.getName() + ".push_back((" + elementType(field.getType()) + "*)" + field.getName() + "[__i]->clone()); " + endl();
                            ret += tab(3) +   "}" + endl();
                            ret += tab(2) + "}" + endl();
                        }
                    }
                }
            } else {
                // core types
                if (field.isArray()) {
                    if (field.getArraySize() > 0) {
                        if (!field.getType().equals("string[]")) {
                            ret += tab(2) + "memcpy(&narrRet->" + field.getName() + "[0], &" + field.getName() + "[0], sizeof(" + field.getName() + "));" + endl();
                        } else {
                            ret += tab(2) + "for(unsigned int __i = 0; __i < " + field.getArraySize() + "; __i++) {" + endl();
                            ret += tab(3) +   "narrRet->" + field.getName() + "[__i] = " + field.getName() + "[__i];" + endl();
                            ret += tab(2) + "}" + endl();
                        }
                    } else {
                        ret += tab(2) + "narrRet->" + field.getName() + " = " + field.getName() + ";" + endl();
                    }
                } else {
                    ret += tab(2) + "narrRet->" + field.getName() + " = " + field.getName() + ";" + endl();
                }
            }
        }
        return ret;
    }

    protected String getConstructorBody(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields()) {
            if (field.isIdlType() && !field.isArray() && field.isAbstract()) {
                ret += tab(2) + field.getName() + " = new " + languageType(field.getType()).replace("*", "()") + ";" + endl();
            }
            if (field.isArray() && (field.getArraySize() > 0)) {
                if (!field.isIdlType()) {
                    if (!field.getType().equals("string[]")) {
                        // int kalle[356]; --> memset(&kalle[0], 0, sizeof(kalle));
                        ret += tab(2) + "memset(&" + field.getName() + "[0], 0, sizeof(" + field.getName() + "));" + endl();
                    }
                } else {
                    if (field.isAbstract()) {
                        ret += tab(2) + "for(unsigned int __i = 0; __i < " + field.getArraySize() + "; __i++) {" + endl();
                        ret += tab(3) +   field.getName() + "[__i] = new " + languageType(field.getType().replace("[]", "")).replace("*", "()") + ";" + endl();
                        ret += tab(2) + "}" + endl();
                    }
                }
            }
        }
        return ret;
    }

    private CharSequence getConstructorHead(IDLClass idlClass)
    {
        String ret = tab(2) + "";
        for (IDLField field : idlClass.getFields()) {
            if (field.getType().equals("boolean")) {
                ret += ", " + field.getName() + "(false)";
            } else {
                if (field.getType().equals("string") || field.isArray() || field.isIdlType()) {
                    //Do nothing in head
                } else {
                    //Numeric
                    ret += ", " + field.getName() + "(0)";
                }
            }
        }
        return ret;
    }

    protected String getDeclarations(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields()) {
            if (!field.getComment().equals("")) {
                String comment = field.getComment();
                int idx;
                while ((idx = comment.indexOf('\n')) >= 0) {
                  ret += tab(1) + "///" + comment.substring(0,idx).replace("/*", "").replace("*/", "") + endl();
                  comment = comment.substring(idx+1);
                }
                ret += tab(1) + "///" + comment.replace("/*", "").replace("*/", "") + endl();
            }
            if (field.isArray()) {
                ret += tab(1) + "" + getDeclareVector(field);
            } else {
                if (field.getType().equals("string")) {
                    ret += tab(1) + "" + languageType(field.getType()) + " " + field.getName() + ";" + endl();
                } else {
                    if (field.isIdlType()) {
                        if (field.isAbstract()) {
                            ret += tab(1) + "" + languageType(field.getType()) + "* " + field.getName() + ";" + endl();
                        } else {
                            ret += tab(1) + "" + languageType(field.getType()) + " " + field.getName() + ";" + endl();
                        }
                    } else {
                        //Simple primitive type
                        ret += tab(1) + "" + languageType(field.getType()) + " " + field.getName() + ";" + endl();
                    }
                }
            }
        }
        return ret;
    }

    protected String getDeclareVector(IDLField field)
    {
        String ret = "";
        if (field.getArraySize() == 0) {
          // idl = type[] name;
          ret += "std::vector<" + languageType(elementType(field.getType()));
          if (field.isAbstract()) ret += "*";
          ret += "> " + field.getName() + ";" + endl();
        } else {
          // idl = type[size] name;
          ret += languageType(elementType(field.getType()));
          if (field.isAbstract()) ret += "*";
          ret += " " + field.getName() + "[" + field.getArraySize() + "];" + endl();
        }
        return ret;
    }

    protected String languageType(String s)
    {
      if (s.equals("string"))    return "std::string";
      if (s.equals("boolean"))   return "bool";
      if (s.equals("int"))       return "int";
      if (s.equals("short"))     return "short";
      if (s.equals("long"))      return "__int64";
      if (s.equals("double"))    return "double";
      if (s.equals("float"))     return "float";
      if (s.equals("byte"))      return "char";
      if (s.equals("string[]"))  return "std::vector<std::string>";
      if (s.equals("int[]"))     return "std::vector<int>";
      if (s.equals("short[]"))   return "std::vector<short>";
      if (s.equals("long[]"))    return "std::vector<__int64>";
      if (s.equals("double[]"))  return "std::vector<double>";
      if (s.equals("float[]"))   return "std::vector<float>";
      if (s.equals("byte[]"))    return "std::vector<char>";
      if (s.equals("boolean[]")) return "std::vector<bool>";
      if (s.endsWith("[]"))      return "std::vector<" + applyLanguagePackageSeparator(s.substring(0, s.indexOf('['))) + "*>";
      return applyLanguagePackageSeparator(s) + "";
    }

    protected String applyLanguagePackageSeparator(String packageName)
    {
        return packageName.replace(".", "::");
    }

    private CharSequence getImports(IDLClass idlClass)
    {
        HashMap<String, String> typesToInclude = new HashMap();
        String ret = "";
        if (idlClass.getBaseClassName() != null) {
            typesToInclude.put(idlClass.getBaseClassName(), idlClass.getBaseClassName());
        }
        for (IDLField field : idlClass.getFields()) {
            if (field.isIdlType()) {
                String type = field.getType();
                if (field.isArray()) {
                    type = type.substring(0, type.length() - 2);
                }
                typesToInclude.put(type, type);
            }
        }
        for (String includeType : typesToInclude.values()) {
            ret += tab(0) + "#include \"" + getSlashedType(includeType) + ".h\"" + endl();
        }
        return ret;
    }

    private String getSlashedType(String type)
    {
        return type.replace(".", "/");
    }

    private String getUnderscoredPackName(String packageName)
    {
        return packageName.replaceAll("\\.", "_");
    }

    private CharSequence getDestructorBody(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields()) {
            if (field.isIdlType() && field.isAbstract()) {
                ret += tab(2);
                if (!field.isArray()) {
                    ret += "if(" + field.getName() + ") delete " + field.getName() + ";" + endl();
                } else {
                    ret += "for(unsigned int __i = 0; __i < ";
                    if (field.getArraySize() > 0) {
                        ret += field.getArraySize();
                    } else {
                        ret += field.getName() + ".size()";
                    }
                    ret += "; __i++){ if(" + field.getName() + "[__i]) delete " + field.getName() + "[__i];}" + endl();
                }
            }
        }
        return ret;
    }

    private String getPackageCloser(String packageName)
    {
        String ret = "";
        for (int i = 0; i < packageName.length(); i++) {
            if (packageName.charAt(i) == '.') {
                ret += "}";
            }
        }
        ret += "}\n";
        return ret;
    }

    private CharSequence getPackageDeclaration(String packageName)
    {
        String ret = "namespace " + packageName.replaceAll("\\.", " { namespace ");
        ret += " {";
        return ret;
    }

    protected String getSerialize(IDLClass idlClass)
    {
        String ret = tab(2);
        if (idlClass.getBaseClassName() != null) {
            ret += languageType(idlClass.getBaseClassName()).replace("*", "") + "::serialize(archive);" + endl();
        } else {
            ret += "ops::OPSObject::serialize(archive);" + endl();
        }
        for (IDLField field : idlClass.getFields()) {
            ret += tab(2);
            if (field.isIdlType()) {
                if (!field.isArray()) {
                    if (field.isAbstract()) {
                        ret += field.getName() + " = (" + languageType(field.getType()) + "*) ";
                    }
                    ret += "archive->inout(std::string(\"" + field.getName() + "\"), " + field.getName() + ");" + endl();
                } else {
                    // isArray()
                    if (field.getArraySize() > 0) {
                        // idl = type[size] name;
                        // template <class SerializableType> void inoutfixarr(const std::string& name, SerializableType** value, int numElements) // for virtual
                        // template <class SerializableType> void inoutfixarr(const std::string& name, SerializableType* value, int numElements)  // for non virtual
                        ret += "archive->inoutfixarr<" + languageType(elementType(field.getType())) +
                              ">(std::string(\"" + field.getName() + "\"), &" + field.getName() + "[0], " + field.getArraySize() + ");" + endl();
                    } else {
                        // idl = type[] name;
                        ret += "archive->inout<" + languageType(elementType(field.getType())) + ">(std::string(\"" + field.getName() + "\"), " + field.getName();
                        if (field.isAbstract()) {
                            ret += ");" + endl();
                        } else {
                            ret += ", " + languageType(elementType(field.getType())) + "());" + endl();
                        }
                    }
                }
            } else {
                // core types
                if (field.isArray()) {
                    if (field.getArraySize() > 0) {
                        // idl = type[size] name;
                        ret += "archive->inoutfixarr(std::string(\"" + field.getName() + "\"), ";
                        if (!field.getType().equals("string[]")) {
                            //void inoutfixarr(const std::string& name, void* value, int numElements, int totalSize);
                            ret += "(void*)&" + field.getName() + "[0], " + field.getArraySize() + ", sizeof(" + field.getName() + "));" + endl();
                        } else {
                            //void inoutfixarr(const std::string& name, std::string* value, int numElements)
                            ret += "&" + field.getName() + "[0], " + field.getArraySize() + ");" + endl();
                        }
                    } else {
                        // idl = type[] name;
                        ret += "archive->inout(std::string(\"" + field.getName() + "\"), " + field.getName() + ");" + endl();
                    }
                } else {
                    ret += "archive->inout(std::string(\"" + field.getName() + "\"), " + field.getName() + ");" + endl();
                }
            }
        }
        return ret;
    }
}
