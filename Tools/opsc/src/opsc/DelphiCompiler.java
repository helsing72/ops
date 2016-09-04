/*
 * DelphiCompiler.java
 *
 * Created on den 28 june 2016
 *
 */

package opsc;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;
import parsing.AbstractTemplateBasedIDLCompiler;
import parsing.IDLClass;
import parsing.IDLField;
import parsing.TopicInfo;

/**
 *
 * @author Lelle
 */
public class DelphiCompiler extends opsc.Compiler
{
    final static String UNIT_REGEX = "__unitName";
    final static String IMPORTS_REGEX = "__importUnits";
    final static String CONSTRUCTOR_BODY_REGEX = "__constructorBody";
    final static String DESTRUCTOR_HEAD_REGEX = "__destructorHead";
    final static String DESTRUCTOR_BODY_REGEX = "__destructorBody";
    final static String DECLARATIONS_REGEX = "__declarations";
    final static String SERIALIZE_REGEX = "__serialize";
    final static String FILL_CLONE_HEAD_REGEX = "__fillCloneHead";
    final static String FILL_CLONE_BODY_REGEX = "__fillCloneBody";
    final static String SIZE_REGEX = "__size";
    final static String CS_DIR = "Delphi";
    private String projectDirectory;
    private static String BASE_CLASS_NAME_REGEX = "__baseClassName";
    private static String CREATE_MAKE_BODY_REGEX = "__createMakeBody";

    String createdFiles = "";

    public DelphiCompiler(String projname) {
        super(projname);
        //tab is 2 spaces
        setTabString("  ");
    }

    public void compileDataClasses(Vector<IDLClass> idlClasses, String projectDirectory)
    {
        createdFiles = "";
        this._idlClasses = idlClasses;
        this.projectDirectory = projectDirectory;
        try
        {
            for (IDLClass iDLClass : idlClasses)
            {
                if (iDLClass.getType() == IDLClass.ENUM_TYPE)
                {
                    compileEnum(iDLClass);
                }
                else
                {
                  compileDataClass(iDLClass);
                  // We put the publisher and subscriber in the same file as the data class
                }
            }

            compileTypeSupport(idlClasses, _projectName);
        } catch (IOException iOException)  {
            System.out.println( "Error: Generating Delphi failed with the following exception: " + iOException.getMessage());
        }

    }

    public void compileTopicConfig(Vector<TopicInfo> topics, String name, String packageString, String projectDirectory)
    {
    }

    protected void compilePublisher(IDLClass cl) {}
    protected void compileSubscriber(IDLClass cl) {}

    protected void compileEnum(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();

        String packageName = idlClass.getPackageName();
        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + packageName + "." + className + ".pas");

        java.io.InputStream stream = findTemplateFile("delphienumtemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(DECLARATIONS_REGEX, getEnumDeclarations(idlClass));

        //Save the modified text to the output file.
        saveOutputText(templateText);
        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    public void compileDataClass(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();
        String baseClassName = "TOPSObject";
        if (idlClass.getBaseClassName() != null)
        {
            baseClassName = getLastPart(idlClass.getBaseClassName());
        }
        String packageName = idlClass.getPackageName();

        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + packageName + "." + className + ".pas");

        java.io.InputStream stream = findTemplateFile("delphitemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(IMPORTS_REGEX, getImports(idlClass));
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(BASE_CLASS_NAME_REGEX, baseClassName);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(CONSTRUCTOR_BODY_REGEX, getConstructorBody(idlClass));
        templateText = templateText.replace(DESTRUCTOR_HEAD_REGEX, getDestructorHead(idlClass));
        templateText = templateText.replace(DESTRUCTOR_BODY_REGEX, getDestructorBody(idlClass));
        templateText = templateText.replace(DECLARATIONS_REGEX, getDeclarations(idlClass));
        templateText = templateText.replace(SERIALIZE_REGEX, getSerialize(idlClass));
        templateText = templateText.replace(FILL_CLONE_HEAD_REGEX, getFillCloneHead(idlClass));
        templateText = templateText.replace(FILL_CLONE_BODY_REGEX, getFillCloneBody(idlClass));

        //Save the modified text to the output file.
        saveOutputText(templateText);
        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    public String getName()
    {
        return "DelphiFactoryIDLCompiler";
    }

    protected void compileTypeSupport(Vector<IDLClass> idlClasses, String projectName) throws IOException
    {
        String className = projectName + "TypeFactory";
        String packageName = projectName;

        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + packageName + "." + className + ".pas");

        java.io.InputStream stream = findTemplateFile("delphitypefactorytemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);

        String createBodyText = "";
        String includes = "";

        for (IDLClass iDLClass : idlClasses) {
            createBodyText += tab(1) + "if types = '" + iDLClass.getPackageName() + "." + iDLClass.getClassName() + "' then begin" + endl();
            createBodyText += tab(2) + "Result := " + getFullyQualifiedClassName(iDLClass.getClassName()) + ".Create;" + endl();
            createBodyText += tab(2) + "Exit;" + endl();
            createBodyText += tab(1) + "end;" + endl();

            includes += tab(1) + getUnitName(iDLClass.getClassName()) + "," + endl();
        }

        templateText = templateText.replace(CREATE_MAKE_BODY_REGEX, createBodyText);
        templateText = templateText.replace(IMPORTS_REGEX, includes);

        saveOutputText(templateText);

        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    protected String getLastPart(String name)
    {
      int idx;
      String s = name;
      while ((idx = s.indexOf('.')) > 0) {
        s = s.substring(idx+1);
      }
      return s;
    }

    protected String getUnitName(String className)
    {
      String s = getLastPart(className);
      for (IDLClass cl : this._idlClasses) {
        if (cl.getClassName().equals(s)) {
          return cl.getPackageName() + "." + cl.getClassName();
        }
      }
      // Didn't find class in list so it is from another "project"
      // Assume then that the 'className' contains <packagename>.<class>
      // and that the unit is <packagename>.<class>, i.e. the same
      return className;
    }

    protected String getFullyQualifiedClassName(String className)
    {
      // We return the unitname.classname
      return getUnitName(className) + "." + getLastPart(className);
    }

    protected String getConstructorBody(IDLClass idlClass)
    {
      String ret = "";
      for (IDLField field : idlClass.getFields())
      {
          if (field.isIdlType() && !field.isArray())
          {
              ret += tab(1) + field.getName() + " := " + getFullyQualifiedClassName(field.getType()) + ".Create;" + endl();
          }
      }
      return ret;
    }

    protected String getDestructorHead(IDLClass idlClass)
    {
      String ret = "";
      for (IDLField field : idlClass.getFields())
      {
          if (field.isIdlType()) {
              if (field.isArray()) {
                ret += tab(0) + "var" + endl();
                ret += tab(1) +   "i : Integer;" + endl();
                break;
              }
          }
      }
      return ret;
    }

    protected String getDestructorBody(IDLClass idlClass)
    {
      String ret = "";
      for (IDLField field : idlClass.getFields())
      {
          if (field.isIdlType()) {
              if (field.isArray()) {
                ret += tab(1) + "for i := 0 to High(" + field.getName() + ") do begin" + endl();
                ret += tab(2) +   "FreeAndNil(" + field.getName() + "[i]);" + endl();
                ret += tab(1) + "end;" + endl();
              } else {
                ret += tab(1) + "FreeAndNil(" + field.getName() + ");" + endl();
              }
          }
      }
      return ret;
    }

    protected String getFillCloneHead(IDLClass idlClass)
    {
      String ret = "";
      for (IDLField field : idlClass.getFields())
      {
          if (field.isIdlType()) {
              if (field.isArray()) {
                ret += tab(0) + "var" + endl();
                ret += tab(1) +   "__i__ : Integer;" + endl();
                break;
              }
          }
      }
      return ret;
    }

    private String getFillCloneBody(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields())
        {
            if (field.isIdlType())
            {
                if (!field.isArray())
                {
                    // Free existing object and clone the new one
                    ret += tab(2) + "FreeAndNil(" + field.getName() + ");" + endl();
                    ret += tab(2) + field.getName() + " := " + getFullyQualifiedClassName(field.getType()) + "(Self." + field.getName() + ".Clone());" + endl();
                }
                else
                {
                    String s = field.getType();
                    s = getFullyQualifiedClassName(s.substring(0, s.indexOf('[')));
                    ret += tab(2) + "for __i__ := 0 to High("+ field.getName() + ") do FreeAndNil(" + field.getName() + "[__i__]);" + endl();
                    ret += tab(2) + "SetLength(" + field.getName() + ", Length(Self." + field.getName() + "));" + endl();
                    ret += tab(2) + "for __i__ := 0 to High("+ field.getName() + ") do" + endl();
                    ret += tab(3) +   field.getName() + "[__i__] := " + s + "(Self." + field.getName() + "[__i__].Clone());" + endl();
                }
            }
            else if (field.isArray())
            {
                // Dynamic arrays of base types can be copied using the Delphi global Copy() function
                ret += tab(2) + field.getName() + " := Copy(Self." + field.getName() + ");" + endl();
            }
            else
            {
                // Base types can just be assigned
                ret += tab(2) + field.getName() + " := Self." + field.getName() + ";" + endl();
            }
        }
        return ret;
    }

    private CharSequence getImports(IDLClass idlClass)
    {
        HashMap<String, String> typesToInclude = new HashMap();
        String ret = "";
        if (idlClass.getBaseClassName() != null)
        {
            String unit = getUnitName(idlClass.getBaseClassName());
            typesToInclude.put(unit, unit);
        }
        for (IDLField field : idlClass.getFields())
        {
            if (field.isIdlType())
            {
                String type = field.getType();
                if (field.isArray())
                {
                    type = type.substring(0, type.length() - 2);
                }
                String unit = getUnitName(type);
                typesToInclude.put(unit, unit);
            }
        }
        for (String includeType : typesToInclude.values())
        {
            ret += tab(1) + includeType + "," + endl();
        }
        return ret;
    }

    private String getEnumDeclarations(IDLClass idlClass)
    {
        String ret = "";
        for (int i = 0; i < idlClass.getEnumNames().size(); i++)
        {
            ret += idlClass.getEnumNames().get(i);
            if (i < idlClass.getEnumNames().size()-1) {
              ret += ",";
            }
        }
        return ret;
    }

    protected String getDeclareVector(IDLField field) {
        return "";
    }

    protected String getDeclarations(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields())
        {
            if(!field.getComment().equals(""))
            {
                String comment = field.getComment();
                int idx;
                while ((idx = comment.indexOf('\n')) >= 0) {
                  ret += tab(2) + "///" + comment.substring(0,idx).replace("/*", "").replace("*/", "") + endl();
                  comment = comment.substring(idx+1);
                }
                ret += tab(2) + "///" + comment.replace("/*", "").replace("*/", "") + endl();
            }
            String fieldType = getLastPart(field.getType());
///TODO 'virtual': All fields that are objects are also virtual in DElphi!!
///TODO   How check that it's the correct object when field in IDL isn't 'virtual'??
            if (field.isArray())
            {
                ret += tab(2) + field.getName() + " : " + languageType(fieldType) + ";" + endl();
            }
            else if(field.getType().equals("string"))
            {
                ret += tab(2) + field.getName() + " : " + languageType(fieldType) + ";" + endl();
            }
            else if(field.isIdlType())
            {
                ret += tab(2) + field.getName() + " : " + languageType(fieldType) + ";" + endl();
            }
            else //Simple primitive type
            {
                ret += tab(2) + field.getName() + " : " + languageType(fieldType) + ";" + endl();
            }
        }
        return ret;
    }

    protected String languageType(String s)
    {
      if (s.equals("string"))    return "AnsiString";
      if (s.equals("boolean"))   return "Boolean";
      if (s.equals("int"))       return "Int32";
      if (s.equals("short"))     return "Int16";
      if (s.equals("long"))      return "Int64";
      if (s.equals("double"))    return "Double";
      if (s.equals("float"))     return "Single";
      if (s.equals("byte"))      return "Byte";
      if (s.equals("string[]"))  return "TDynAnsiStringArray";
      if (s.equals("int[]"))     return "TDynInt32Array";
      if (s.equals("short[]"))   return "TDynInt16Array";
      if (s.equals("long[]"))    return "TDynInt64Array";
      if (s.equals("double[]"))  return "TDynDoubleArray";
      if (s.equals("float[]"))   return "TDynSingleArray";
      if (s.equals("byte[]"))    return "TDynByteArray";
      if (s.equals("boolean[]")) return "TDynBooleanArray";
      if (s.endsWith("[]"))      return "array of " + s.substring(0, s.indexOf('['));
      return s;
    }

    protected String getSerialize(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields())
        {
            if(field.isIdlType())
            {
                if(!field.isArray())
                {
                    ret += tab(1) + field.getName() + " := " + getFullyQualifiedClassName(field.getType()) +
                          "(archiver.Inout2('" + field.getName() + "', TSerializable(" + field.getName() + ")));" + endl();
                }
                else
                {
                    //String s = field.getType();
                    //s = getFullyQualifiedClassName(s.substring(0, s.indexOf('[')));
                    ret += tab(1) + "archiver.inout('" + field.getName() + "', TDynSerializableArray(" + field.getName() + "));" + endl();
                }
            }
            else if(field.isArray())
            {
                ret += tab(1) + "archiver.Inout('" + field.getName() + "', " + field.getName() + ");" + endl();
            }
            else
            {
                ret += tab(1) + "archiver.Inout('" + field.getName() + "', " + field.getName() + ");" + endl();
            }

        }
        return ret;
    }
}
