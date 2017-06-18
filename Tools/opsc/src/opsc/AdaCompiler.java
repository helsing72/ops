/*
 * AdaCompiler.java
 *
 * Created on den 28 november 2016
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

/**
 *
 * @author Lelle
 */
public class AdaCompiler extends opsc.Compiler
{
    final static String UNIT_REGEX = "__unitName";
    final static String UNIT_PUB_REGEX = "__pubUnitName";
    final static String UNIT_SUB_REGEX = "__subUnitName";
    final static String IMPORTS_REGEX = "__importUnits";
    final static String CONSTRUCTOR_HEAD_REGEX = "__constructorHead";
    final static String CONSTRUCTOR_BODY_REGEX = "__constructorBody";
    final static String DESTRUCTOR_HEAD_REGEX = "__destructorHead";
    final static String DESTRUCTOR_BODY_REGEX = "__destructorBody";
    final static String DECLARATIONS_REGEX = "__declarations";
    final static String SERIALIZE_REGEX = "__serialize";
    final static String FILL_CLONE_HEAD_REGEX = "__fillCloneHead";
    final static String FILL_CLONE_BODY_REGEX = "__fillCloneBody";
    final static String VALIDATE_HEAD_REGEX = "__validateHead";
    final static String VALIDATE_BODY_REGEX = "__validateBody";
    final static String SIZE_REGEX = "__size";
    final static String CS_DIR = "Ada";
//    private String projectDirectory;
    private static String BASE_CLASS_NAME_REGEX = "__baseClassName";
    private static String CREATE_MAKE_BODY_REGEX = "__createMakeBody";
    private static String PROJNAME_REGEX = "__projName";

    private boolean alwaysDynArray = true;
    private boolean alwaysDynObject = true;

    String createdFiles = "";

    public AdaCompiler(String projname)
    {
        super(projname);
        //tab is 2 spaces
        setTabString("  ");
    }

    public String getName()
    {
        return "AdaFactoryIDLCompiler";
    }

    public void compileDataClasses(Vector<IDLClass> idlClasses, String projectDirectory)
    {
        createdFiles = "";
        this._idlClasses = idlClasses;
//        this.projectDirectory = projectDirectory;
        try {
            if (!_onlyGenTypeSupport) {
              for (IDLClass iDLClass : idlClasses) {
                if (iDLClass.getType() == IDLClass.ENUM_TYPE) {
                    compileEnum(iDLClass);
                } else {
                    compileDataClass(iDLClass);
                    compileSubscriber(iDLClass);
                    compilePublisher(iDLClass);
                }
              }
            }
            compileTypeSupport(idlClasses, _projectName);
            if (!_onlyGenTypeSupport) compileProjectFile(_projectName);
        } catch (IOException iOException)  {
            System.out.println( "Error: Generating Ada failed with the following exception: " + iOException.getMessage());
        }
    }

    protected void compilePubSubHelper(String className, String templateName, String fileName) throws IOException
    {
      setOutputFileName(fileName);

      java.io.InputStream stream = findTemplateFile(templateName);
      setTemplateTextFromResource(stream);

      //Get the template file as a String
      String templateText = getTemplateText();

      //Replace regular expressions in the template file.
      templateText = templateText.replace(UNIT_REGEX, getUnitName(className));
      templateText = templateText.replace(UNIT_PUB_REGEX, getPubUnitName(className));
      templateText = templateText.replace(UNIT_SUB_REGEX, getSubUnitName(className));
      templateText = templateText.replace(CLASS_NAME_REGEX, className);

      //Save the modified text to the output file.
      saveOutputText(templateText);
      createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    protected void compilePublisher(IDLClass idlClass) throws IOException
    {
      String className = idlClass.getClassName();
      String packageName = idlClass.getPackageName();
      String packageFilePart = packageName.replace(".", "/");
      String baseFileName = _outputDir + File.separator + packageFilePart + File.separator + getPubUnitName(className).replace(".", "-");
      compilePubSubHelper(className, "adaspecpublishertemplate.tpl", baseFileName + ".ads");
      compilePubSubHelper(className, "adabodypublishertemplate.tpl", baseFileName + ".adb");
    }

    protected void compileSubscriber(IDLClass idlClass) throws IOException
    {
      String className = idlClass.getClassName();
      String packageName = idlClass.getPackageName();
      String packageFilePart = packageName.replace(".", "/");
      String baseFileName = _outputDir + File.separator + packageFilePart + File.separator + getSubUnitName(className).replace(".", "-");
      compilePubSubHelper(className, "adaspecsubscribertemplate.tpl", baseFileName + ".ads");
      compilePubSubHelper(className, "adabodysubscribertemplate.tpl", baseFileName + ".adb");
    }

    protected void compileEnumHelper(IDLClass idlClass, String className, String packageName, String templateName, String fileName) throws IOException
    {
      setOutputFileName(fileName);

      java.io.InputStream stream = findTemplateFile(templateName);
      setTemplateTextFromResource(stream);

      //Get the template file as a String
      String templateText = getTemplateText();

      //Replace regular expressions in the template file.
      templateText = templateText.replace(UNIT_REGEX, getUnitName(className));
      templateText = templateText.replace(CLASS_NAME_REGEX, className);
      templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
      templateText = templateText.replace(DECLARATIONS_REGEX, getEnumDeclarations(idlClass));

      //Save the modified text to the output file.
      saveOutputText(templateText);
      createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    protected void compileEnum(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();
        String packageName = idlClass.getPackageName();
        String packageFilePart = packageName.replace(".", "/");
        String baseFileName = _outputDir + File.separator + packageFilePart + File.separator + getUnitName(className).replace(".", "-");

        compileEnumHelper(idlClass, className, packageName, "adaspecenumtemplate.tpl", baseFileName + ".ads");
        compileEnumHelper(idlClass, className, packageName, "adabodyenumtemplate.tpl", baseFileName + ".adb");
    }

    public void compileDataClass(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();
        String baseClassName = "OpsObject";
        if (idlClass.getBaseClassName() != null) {
          baseClassName = idlClass.getBaseClassName();

          // Change name for some internal Delphi units
          if (baseClassName.equals("ops.Reply")) baseClassName = "TReply";
          if (baseClassName.equals("ops.Request")) baseClassName = "TRequest";

          baseClassName = getLastPart(baseClassName);
        }
        String packageName = idlClass.getPackageName();
        String packageFilePart = packageName.replace(".", "/");
        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + getUnitName(className).replace(".", "-") + ".ads");

        java.io.InputStream stream = findTemplateFile("adaspectemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(UNIT_REGEX, getUnitName(className));                    //
        templateText = templateText.replace(IMPORTS_REGEX, getImports(idlClass));                   //
        templateText = templateText.replace(CLASS_NAME_REGEX, className);                           //
        templateText = templateText.replace(BASE_CLASS_NAME_REGEX, baseClassName);                  //
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);                       //
        templateText = templateText.replace(DECLARATIONS_REGEX, getDeclarations(idlClass));         //

        //Save the modified text to the output file.
        saveOutputText(templateText);
        createdFiles += "\"" + getOutputFileName() + "\"\n";

        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + getUnitName(className).replace(".", "-") + ".adb");

        stream = findTemplateFile("adabodytemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(UNIT_REGEX, getUnitName(idlClass.getClassName()));
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(BASE_CLASS_NAME_REGEX, baseClassName);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(CONSTRUCTOR_BODY_REGEX, getConstructorBody(idlClass));
        templateText = templateText.replace(DESTRUCTOR_BODY_REGEX, getDestructorBody(idlClass));
        templateText = templateText.replace(SERIALIZE_REGEX, getSerialize(idlClass));
        templateText = templateText.replace(FILL_CLONE_BODY_REGEX, getFillCloneBody(idlClass));

        //Save the modified text to the output file.
        saveOutputText(templateText);
        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    protected void compileTypeSupport(Vector<IDLClass> idlClasses, String projectName)
    {
      try {
        String className = projectName + "TypeFactory";
        String packageName = projectName;
        String unitNamePart = "Ops_Pa.SerializableFactory_Pa." + packageName + "_" + className;

        String packageFilePart = packageName.replace(".", "/");

        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + unitNamePart.replace(".", "-") + ".ads");

        java.io.InputStream stream = findTemplateFile("adaspectypefactorytemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(UNIT_REGEX, unitNamePart);
        templateText = templateText.replace(CLASS_NAME_REGEX, className);

        saveOutputText(templateText);
        createdFiles += "\"" + getOutputFileName() + "\"\n";

        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + unitNamePart.replace(".", "-") + ".adb");

        stream = findTemplateFile("adabodytypefactorytemplate.tpl");
        setTemplateTextFromResource(stream);

        //Get the template file as a String
        templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(UNIT_REGEX, unitNamePart);
        templateText = templateText.replace(CLASS_NAME_REGEX, className);

        String createBodyText = "";
        String includes = "";

        for (IDLClass iDLClass : idlClasses) {
            createBodyText += tab(2) + "if types = \"" + iDLClass.getPackageName() + "." + iDLClass.getClassName() + "\" then" + endl();
            createBodyText += tab(3) +   "return Serializable_Class_At(" + getUnitName(iDLClass.getClassName()) + ".Create);" + endl();
            createBodyText += tab(2) + "end if;" + endl();

            includes += tab(1) + getUnitName(iDLClass.getClassName()) + "," + endl();
        }

        templateText = templateText.replace(CREATE_MAKE_BODY_REGEX, createBodyText);
        templateText = templateText.replace(IMPORTS_REGEX, includes);

        saveOutputText(templateText);
        createdFiles += "\"" + getOutputFileName() + "\"\n";
      } catch (IOException iOException)  {
          System.out.println( "Error: Generating Ada Factory failed with the following exception: " + iOException.getMessage());
      }
    }

    protected void compileProjectFile(String projectName) throws IOException
    {
      setOutputFileName(_outputDir + File.separator + projectName + ".gpr");

      java.io.InputStream stream = findTemplateFile("adaprojectfiletemplate.tpl");
      setTemplateTextFromResource(stream);

      //Get the template file as a String
      String templateText = getTemplateText();

      //Replace regular expressions in the template file.
      templateText = templateText.replace(PROJNAME_REGEX, projectName);

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
      // Inheritance chain:
      //   OpsObject
      //   sds.MessageHeaderData
      //   pizza.PizzaData
      //   pizza.VessuvioData
      //   pizza.CapricosaData
      //   pizza.special.LHCData
      //   pizza.special.ExtraAllt

      // Put all units below Ops_Pa.OpsObject_Pa
      // Ops_Pa.OpsObject_Pa.sds_MessageHeaderData
      // Ops_Pa.OpsObject_Pa.pizza_PizzaData
      // Ops_Pa.OpsObject_Pa.pizza_special_ExtraAllt
      String baseUnit = "Ops_Pa.OpsObject_Pa.";

      String s = getLastPart(className);
      for (IDLClass cl : this._idlClasses) {
        if (cl.getClassName().equals(s)) {
          String unit = cl.getPackageName() + "." + cl.getClassName();
          return baseUnit + unit.replace(".", "_");
        }
      }
      // Didn't find class in list so it is from another "project"
      // Assume then that the 'className' contains <packagename>.<class>
      // and that the unit is <packagename>.<class>, i.e. the same
      return baseUnit + className.replace(".", "_");
    }

    protected String getPubUnitName(String className)
    {
      return getUnitName(className).replace("Ops_Pa.OpsObject_Pa.", "Ops_Pa.PublisherAbs_Pa.Publisher_Pa.");
    }

    protected String getSubUnitName(String className)
    {
      return getUnitName(className).replace("Ops_Pa.OpsObject_Pa.", "Ops_Pa.Subscriber_Pa.");
    }

    protected String getFullyQualifiedClassName(String className)
    {
      // We return the unitname.classname
      return getUnitName(className) + "." + getLastPart(className) + "_Class";
    }

//    protected String getConstructorHead(IDLClass idlClass)
//    {
//      String ret = "";
//      return ret;
//    }

    protected String getConstructorBody(IDLClass idlClass)
    {
      String ret = "";

      for (IDLField field : idlClass.getFields()) {
          if (field.isIdlType() && !field.isArray()) {
              if (alwaysDynObject || field.isAbstract()) {
                  ret += tab(2) + "Self." + getFieldName(field) + " := Create;" + endl();
              }
          }
      }
      return ret;
    }

//    protected String getDestructorHead(IDLClass idlClass)
//    {
//      String ret = "";
//      return ret;
//    }

    protected String genFree(int pos, String name)
    {
      String ret = "";
      ret += tab(pos) + "if " + name + " /= null then" + endl();
      ret += tab(pos+1) + "Free(" + name + ");" + endl();
      ret += tab(pos) + "end if;" + endl();
      return ret;
    }

    protected String getDestructorBody(IDLClass idlClass)
    {
      String ret = "";
      int pos = 2;
      for (IDLField field : idlClass.getFields()) {
          if (field.isIdlType()) {
              if (field.isArray()) {
                  if (alwaysDynArray || (field.getArraySize() == 0)) {
                      ret += tab(pos++) + "if Self." + getFieldName(field) + " /= null then" + endl();
                  }
                  if (alwaysDynObject || field.isAbstract()) {
                      ret += tab(pos) + "Clear(Self." + getFieldName(field) + ".all);" + endl();

                      //ret += tab(pos) + "for i in Self." + getFieldName(field) + "'Range loop" + endl();
                      //ret += genFree(pos+1, "Self." + getFieldName(field) + "(i)");
                      //ret += tab(pos) + "end loop;" + endl();
                  }
                  if (alwaysDynArray || (field.getArraySize() == 0)) {
                      ret += tab(pos) + "Dispose(Self." + getFieldName(field) + ");" + endl();
                      ret += tab(--pos) + "end if;" + endl();
                  }
              } else {
                  if (alwaysDynObject || field.isAbstract()) {
                      ret += genFree(pos, "Self." + getFieldName(field));
                  }
              }
          } else {
              // Core types
              if (field.isArray()) {
                  if (alwaysDynArray || (field.getArraySize() == 0)) {
                      ret += tab(pos++) + "if Self." + getFieldName(field) + " /= null then" + endl();
                  }
                  if (elementType(field.getType()) == "String_At") {
                      ret += tab(pos) + "Clear(Self." + getFieldName(field) + ".all);" + endl();

                      //ret += tab(pos) + "for i in Self." + getFieldName(field) + "'Range loop" + endl();
                      //ret += tab(pos+1) + "Dispose(Self." + getFieldName(field) + "(i));" + endl();
                      //ret += tab(pos) + "end loop;" + endl();
                  }
                  if (alwaysDynArray || (field.getArraySize() == 0)) {
                      ret += tab(pos) + "Dispose(Self." + getFieldName(field) + ");" + endl();
                      ret += tab(--pos) + "end if;" + endl();
                  }
              } else {
                  if (languageType(field.getType()) == "String_At") {
                      ret += tab(pos) + "if Self." + getFieldName(field) + " /= null then" + endl();
                      ret += tab(pos+1) +   "Dispose(Self." + getFieldName(field) + ");" + endl();
                      ret += tab(pos) + "end if;" + endl();
                  }
              }
          }
      }
      return ret;
    }

//    protected String getFillCloneHead(IDLClass idlClass)
//    {
//      String ret = "";
//      return ret;
//    }

    private String getFillCloneBody(IDLClass idlClass)
    {
        String ret = "";
        int pos = 3;
        String className = idlClass.getClassName() + "_Class";
        for (IDLField field : idlClass.getFields()) {
            String fieldName = getFieldName(field);
            if (field.isIdlType()) {
                // Idl types
                if (field.isArray()) {
                    String elemType = getLastPart(elementType(field.getType()));
                    // Remove ev old elements and array if dynamic
                    if (alwaysDynArray || (field.getArraySize() == 0)) {
                        ret += tab(pos) + "if " + className + "(obj.all)." + fieldName + " /= null then" + endl();
                        pos++;
                    }
                    if (alwaysDynObject || field.isAbstract()) {
                        ret += tab(pos) + "Clear(" + className + "(obj.all)." + fieldName + ".all);" + endl();

                        //ret += tab(pos) + "for i in " + className + "(obj.all)." + fieldName + "'Range loop" + endl();
                        //ret += genFree(pos+1, className + "(obj.all)." + fieldName + "(i)");
                        //ret += tab(pos) + "end loop;" + endl();
                    }
                    if (alwaysDynArray || (field.getArraySize() == 0)) {
                        ret += tab(pos) + "Dispose(" + className + "(obj.all)." + fieldName + ");" + endl();
                        pos--;
                        ret += tab(pos) + "end if;" + endl();
                    // Create new array if source not null
                        ret += tab(pos) + "if Self." + fieldName + " /= null then" + endl();
                        pos++;
                        if (alwaysDynObject || field.isAbstract()) {
                            ret += tab(pos) + className + "(obj.all)." + fieldName + " := new " + elemType + "_Class_At_Arr(Self." + fieldName + "'Range);" + endl();
                        } else {
                            ret += tab(pos) + className + "(obj.all)." + fieldName + " := new " + elemType + "_Class_Arr(Self." + fieldName + "'Range);" + endl();
                        }
                    }
                    // Clone source
                    ret += tab(pos) + "for i in Self." + fieldName + "'Range loop" + endl();
                    if (alwaysDynObject || field.isAbstract()) {
                        ret += tab(pos+1) + "if Self." + fieldName + "(i) /= null then" + endl();
                        ret += tab(pos+2) + className + "(obj.all)." + fieldName + "(i) := " + elemType + "_Class_At(Clone(Self." + fieldName + "(i).all));" + endl();
                        ret += tab(pos+1) + "end if;" + endl();
                    } else {
                        //FillClone(Self.test2s2(i), ChildData_Class(obj.all).test2s2(i)'Access);
                        ret += tab(pos+1) + "FillClone(Self." + fieldName + "(i), " + className + "(obj.all)." + fieldName + "(i)'Access);" + endl();
                    }
                    ret += tab(pos) + "end loop;" + endl();
                    if (alwaysDynArray || (field.getArraySize() == 0)) {
                        pos--;
                        ret += tab(pos) + "end if;" + endl();
                    }
                } else {
                    if (alwaysDynObject || field.isAbstract()) {
                        // Free existing object and clone the new one
                        ret += genFree(pos, className + "(obj.all)." + fieldName);
                        ret += tab(pos) + "if Self." + fieldName + " /= null then" + endl();
                        ret += tab(pos+1) + className + "(obj.all)." + fieldName + " := " + getFullyQualifiedClassName(field.getType()) + "_At(Clone(Self." + fieldName + ".all));" + endl();
                        ret += tab(pos) + "end if;" + endl();
                    } else {
                        //FillClone(Self.test2, ChildData_Class(obj.all).test2'Access);
                        ret += tab(pos) + "FillClone(Self." + fieldName + ", " + className + "(obj.all)." + fieldName + "'Access);" + endl();
                    }
                }
            } else {
                // Core types
                if (field.isArray()) {
                    if ((!alwaysDynArray) && (field.getArraySize() > 0)) {
                        // Fixed arrays of core types, just assign except for strings that need Replace
                        if (elementType(field.getType()) == "String_At") {
                            ret += tab(pos) + "for i in Self."+ fieldName + "'Range loop" + endl();
                            ret += tab(pos+1) +   "Replace(" + className + "(obj.all)." + fieldName + "(i), Self." + fieldName + "(i));" + endl();
                            ret += tab(pos) + "end loop;" + endl();
                        } else {
                            ret += tab(pos) + className + "(obj.all)." + fieldName + " := Self." + fieldName + ";" + endl();
                        }
                    } else {
                        // dynamic arrays of core types
                        if (elementType(field.getType()) == "String_At") {
                            ret += tab(pos) + "Clear(" + className + "(obj.all)." + fieldName + ");" + endl();
                            ret += tab(pos) + "Dispose(" + className + "(obj.all)." + fieldName + ");" + endl();
                            ret += tab(pos) + "if Self." + fieldName + " /= null then" + endl();
                            ret += tab(pos+1) + className + "(obj.all)." + fieldName + " := new String_Arr(Self." + fieldName + "'Range);" + endl();
                            ret += tab(pos+1) + "for i in Self." + fieldName + "'Range loop" + endl();
                            ret += tab(pos+2) +   className + "(obj.all)." + fieldName + "(i) := Copy(Self." + fieldName + "(i));" + endl();
                            ret += tab(pos+1) + "end loop;" + endl();
                            ret += tab(pos) + "end if;" + endl();
                        } else {
                            ret += tab(pos) + "Dispose(" + className + "(obj.all)." + fieldName + ");" + endl();
                            ret += tab(pos) + "if Self." + fieldName + " /= null then" + endl();
                            ret += tab(pos+1) + className + "(obj.all)." + fieldName + " := new " + elementType(field.getType()) + "_Arr'(Self." + fieldName + ".all);" + endl();
                            ret += tab(pos) + "end if;" + endl();
                        }
                    }
                } else {
                    // Non array core types, just assign except for strings that need Replace()
                    if (elementType(field.getType()) == "String_At") {
                        ret += tab(pos) + "Replace(" + className + "(obj.all)." + fieldName + ", " + "Self." + fieldName + ");" + endl();
                    } else {
                        ret += tab(pos) + className + "(obj.all)." + fieldName + " := Self." + fieldName + ";" + endl();
                    }
                }
            }
        }
        return ret;
    }

    private CharSequence getImports(IDLClass idlClass)
    {
        HashMap<String, String> typesToInclude = new HashMap();
        String ret = "";
        if (idlClass.getBaseClassName() != null) {
            String unit = getUnitName(idlClass.getBaseClassName());

///TODO            // Change name for some internal Delphi units
///            if (unit.equals("ops.Reply")) unit = "uOps.RequestReply.Reply";
///            if (unit.equals("ops.Request")) unit = "uOps.RequestReply.Request";

            typesToInclude.put(unit, unit);
        }
        for (IDLField field : idlClass.getFields()) {
            if (field.isIdlType()) {
                String type = field.getType();
                if (field.isArray()) {
                    type = type.substring(0, type.length() - 2);
                }
                String unit = getUnitName(type);
                typesToInclude.put(unit, unit);
            }
        }
        for (String includeType : typesToInclude.values()) {
            ret += tab(1) + includeType + "," + endl();
        }
        return ret;
    }

    private String getEnumDeclarations(IDLClass idlClass)
    {
        String ret = "";
        for (int i = 0; i < idlClass.getEnumNames().size(); i++) {
            ret += idlClass.getEnumNames().get(i);
            if (i < idlClass.getEnumNames().size()-1) {
              ret += ",";
            }
        }
        return ret;
    }

    protected String getFieldName(IDLField field)
    {
        // Ada don't allow leading and trailing "_" so change them
        String s = field.getName();
        if (s.charAt(0) == '_') s = "a" + s;
        if (s.charAt(s.length()-1) == '_') s = s + "a";
        return s;
    }

    protected String getInitValue(String s)
    {
      s = s.replace("[]", "");
      if (s.equals("string"))    return "null";
      if (s.equals("boolean"))   return "False";
      if (s.equals("double"))    return "0.0";
      if (s.equals("float"))     return "0.0";
      return "0";
    }

    protected String getDeclareVector(IDLField field)
    {
        String fieldType = getLastPart(field.getType());
        String ret = "";
        if (alwaysDynArray || (field.getArraySize() == 0)) {
            String typeStr, initStr = "", tickStr = "";
            // idl = type[] name;
            if (field.isIdlType() && (!field.isAbstract()) && (!alwaysDynObject)) {
                typeStr = elementType(fieldType) + "_Class_Arr";
            } else if (field.isIdlType()) {
                typeStr = elementType(fieldType) + "_Class_At_Arr";
                initStr = " => Create";
                tickStr = "'";
            } else {
                if (elementType(fieldType) == "String_At") {
                    typeStr = "String_Arr";
                    initStr = " => null";
                    tickStr = "'";
                } else {
                    typeStr = elementType(fieldType) + "_Arr";
                    initStr = " => " + getInitValue(fieldType);
                    tickStr = "'";
                }
            }
            if (field.getArraySize() == 0) {
                ret += typeStr + "_At := null;" + endl();
            } else {
                ret += typeStr + "_At := new " + typeStr + tickStr + "(0.." + (field.getArraySize() - 1) + initStr + ");" + endl();
            }
        } else {
            // idl = type[size] name;
            String arrDecl = "_Arr(0.." + (field.getArraySize() - 1) + ")";
            if (field.isIdlType() && (!field.isAbstract()) && (!alwaysDynObject)) {
                ret += elementType(fieldType) + "_Class" + arrDecl + ";" + endl();
            } else if(field.isIdlType()) {
                ret += elementType(fieldType) + "_Class_At" + arrDecl + " := (others => null);" + endl();
            } else {
                if (elementType(fieldType) == "String_At") {
                    ret += "String" + arrDecl + " := (others => " + getInitValue(field.getType()) + ");" + endl();
                } else {
                    ret += elementType(fieldType) + arrDecl + " := (others => " + getInitValue(field.getType()) + ");" + endl();
                }
            }
        }
        return ret;
    }

    protected String getDeclarations(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields()) {
            if(!field.getComment().equals("")) {
                String comment = field.getComment();
                int idx;
                while ((idx = comment.indexOf('\n')) >= 0) {
                  ret += tab(3) + "---" + comment.substring(0,idx).replace("/*", "").replace("*/", "") + endl();
                  comment = comment.substring(idx+1);
                }
                ret += tab(3) + "---" + comment.replace("/*", "").replace("*/", "") + endl();
            }
            String fieldType = getLastPart(field.getType());
            ret += tab(3) + getFieldName(field) + " : ";
            if (field.isArray()) {
                ret += getDeclareVector(field);
            } else if ((!alwaysDynObject) && field.isIdlType() && !field.isAbstract()) {
                ret += "aliased " + languageType(fieldType) + "_Class;" + endl();
            } else if (field.isIdlType()) {
                ret += languageType(fieldType) + "_Class_At := null;" + endl();
            } else {
                ret += languageType(fieldType) + " := " + getInitValue(fieldType) + ";" + endl();
            }
        }
        return ret;
    }

//    protected String getValidationHead(IDLClass idlClass)
//    {
//      String ret = "";
//      for (IDLField field : idlClass.getFields())
//      {
//          if (field.isIdlType() && !field.isAbstract()) {
//              if (field.isArray()) {
//                  ret += tab(0) + "var" + endl();
//                  ret += tab(1) +   "__i__ : Integer;" + endl();
//                  break;
//              }
//          }
//      }
//      return ret;
//    }

//    protected String getValidationBody(IDLClass idlClass)
//    {
//      String ret = "";
//      for (IDLField field : idlClass.getFields())
//      {
//          String fieldType = getLastPart(field.getType());
//          String fieldName = getFieldName(field);
//          if (field.isIdlType() && !field.isAbstract())
//          {
//              // 'virtual': All fields that are objects are also virtual in Delphi!!
//              // Need to validate that an object that isn't declared 'virtual' really
//              // is of the correct type
//              if (field.isArray()) {
//                String s = field.getType();
//                s = getLastPart(s.substring(0, s.indexOf('[')));
//                ret += tab(1) + "for __i__ := 0 to High(" + fieldName + ") do begin" + endl();
//                ret += tab(2) + "if not " + fieldName + "[__i__].ClassNameIs('" + s + "') then Result := False;" + endl();
//                ret += tab(1) + "end;" + endl();
//              } else {
//                ret += tab(1) + "if not " + fieldName + ".ClassNameIs('" + getLastPart(field.getType()) + "') then Result := False;" + endl();
//              }
//          }
//      }
//      return ret;
//    }

    private String elementType(String type)
    {
        return languageType(type.replace("[]", ""));
    }

    protected String languageType(String s)
    {
      if (s.equals("string"))    return "String_At";
      if (s.equals("boolean"))   return "Boolean";
      if (s.equals("int"))       return "Int32";
      if (s.equals("short"))     return "Int16";
      if (s.equals("long"))      return "Int64";
      if (s.equals("double"))    return "Float64";
      if (s.equals("float"))     return "Float32";
      if (s.equals("byte"))      return "Byte";
      if (s.equals("string[]"))  return "String_Arr";
      if (s.equals("int[]"))     return "Int32_Arr";
      if (s.equals("short[]"))   return "Int16_Arr";
      if (s.equals("long[]"))    return "Int64_Arr";
      if (s.equals("double[]"))  return "Float64_Arr";
      if (s.equals("float[]"))   return "Float32_Arr";
      if (s.equals("byte[]"))    return "Byte_Arr";
      if (s.equals("boolean[]")) return "Boolean_Arr";
      return s;
    }

    protected String getSerialize(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields()) {
            String fieldName = getFieldName(field);
            if (field.isIdlType()) {
                if (field.isArray()) {
                    ret += tab(2);
                    if ((!alwaysDynArray) && (field.getArraySize() > 0)) {
                        // idl = type[size] name;
                        // TestData_Class_InoutFixArr(archiver, "ftest2s2", Self.ftest2s2);
                        ret += elementType(getLastPart(field.getType())) + "_Class_InoutFixArr(archiver, \"" + field.getName() + "\", Self." + fieldName + ");" + endl();
                    } else {
                        ret += elementType(getLastPart(field.getType())) + "_Class_InoutDynArr(archiver, \"" + field.getName() + "\", Self." + fieldName + ");" + endl();
                    }
                } else {
                    if (alwaysDynObject || field.isAbstract()) {
                        ret += tab(2);
                        ret += "Self." + fieldName + " := " + getFullyQualifiedClassName(field.getType()) + "_At" +
                               "(archiver.Inout2(\"" + field.getName() + "\", Serializable_Class_At(Self." + fieldName + ")));" + endl();
                    } else {
                        ret += tab(2) + "declare" + endl();
                        ret += tab(3) +   "tmp : " + getFullyQualifiedClassName(field.getType()) + "_At := Self." + fieldName + "'Unchecked_Access;" + endl();
                        ret += tab(2) + "begin" + endl();
                        ret += tab(3) +   "archiver.Inout(\"" + field.getName() + "\", Serializable_Class_At(tmp));" + endl();
                        ret += tab(2) + "end;" + endl();
                    }
                }
            } else {
                ret += tab(2);
                // core types
                if (field.isArray()) {
                    if ((!alwaysDynArray) && (field.getArraySize() > 0)) {
                        // idl = type[size] name;
                        ret += "archiver.Inout(\"" + field.getName() + "\", Self." + fieldName + ");" + endl();
                    } else {
                        // idl = type[] name;
                        ret += "archiver.Inout(\"" + field.getName() + "\", Self." + fieldName + ");" + endl();
                    }
                } else {
                  ret += "archiver.Inout(\"" + field.getName() + "\", Self." + fieldName + ");" + endl();
                }
            }
        }
        return ret;
    }

}
