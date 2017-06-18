/*
 * JSONCompiler.java
 *
 * Created on den 24 september 2016
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
public class JSONCompiler extends CompilerSupport
{
    String res = "";

    public JSONCompiler(String projname) {
        super(projname);
        //tab is 2 spaces
        setTabString("  ");
    }

    public void compileDataClasses(Vector<IDLClass> idlClasses, String projectDirectory)
    {
        res = "";
        this._idlClasses = idlClasses;

        res += "[" + endl();
        res += Example_For_describing_alternative_interpretation_for_a_variable(1);
        res += tab(1) + "," + endl();
        res += generate_coretypes_JSON(1);
        res += tab(1) + "," + endl();
        res += generate_OPSObject_JSON(1);
        for (IDLClass iDLClass : idlClasses)
        {
            res += tab(1) + "," + endl();
            res += generateJSONobject(1, iDLClass);
        }
        res += "]" + endl();

        //Save the generated text to the output file.
        setOutputFileName(_outputDir + File.separator + _projectName + ".json");
        saveOutputText(res);
        System.out.println("Info: Saved JSON description to file: " + outputFileName);
    }

    protected String Example_For_describing_alternative_interpretation_for_a_variable(int t)
    {
      String res = "";
      res += tab(t) + "{" + endl();
      res += tab(t+1) + "\"an_idea\": {" + endl();
      res += tab(t+2) + "\"desc\": \"An idea of how to describe an alternative interpretation of a specific variable\"," + endl();
      res += tab(t+2) + "\"topic\": \"ExampleDomain::ExampleTopic\"," + endl();
      res += tab(t+2) + "\"variable\": \"sampledata\"," + endl();
      res += tab(t+2) + "\"interpret_as\": [" + endl();
      res += tab(t+3) + "{ \"name\": \"Kalle\", \"type\": \"double\" }," + endl();
      res += tab(t+3) + "{ \"name\": \"Olle\", \"type\": \"int\" }" + endl();
      res += tab(t+2) + "]" + endl();
      res += tab(t+1) + "}" + endl();
      res += tab(t) + "}" + endl();
      return res;
    }

    protected String generate_coretypes_JSON(int t)
    {
        String res = "";
        res += tab(t) + "{" + endl();
        res += tab(t+1) + "\"ops_internals\": {" + endl();
        res += tab(t+2) + "\"coretypes\": [" + endl();
        res += tab(t+3) + "{ \"type\": \"boolean\", \"size\": 1 }," + endl();
        res += tab(t+3) + "{ \"type\": \"byte\", \"size\": 1 }," + endl();
        res += tab(t+3) + "{ \"type\": \"short\", \"size\": 2 }," + endl();
        res += tab(t+3) + "{ \"type\": \"int\", \"size\": 4 }," + endl();
        res += tab(t+3) + "{ \"type\": \"long\", \"size\": 8 }," + endl();
        res += tab(t+3) + "{ \"type\": \"float\", \"size\": 4 }," + endl();
        res += tab(t+3) + "{ \"type\": \"double\", \"size\": 8 }," + endl();

        res += tab(t+3) + "{ \"type\": \"string\"," + endl();
        res += tab(t+4) + "\"composed_of\": [" + endl();
        res += tab(t+5) + "{ \"num_elements\": \"int\" }," + endl();
        res += tab(t+5) + "{ \"elements\": \"byte\" }" + endl();
        res += tab(t+4) + "]" + endl();
        res += tab(t+3) + "}," + endl();

        res += tab(t+3) + "{ \"type\": \"vector<T>\"," + endl();
        res += tab(t+4) + "\"composed_of\": [" + endl();
        res += tab(t+5) + "{ \"num_elements\": \"int\" }," + endl();
        res += tab(t+5) + "{ \"elements\": \"elementtype\" }" + endl();
        res += tab(t+4) + "]" + endl();
        res += tab(t+3) + "}" + endl();

        res += tab(t+2) + "]," + endl();

        res += tab(t+2) + "\"non-coretypes\": {" + endl();
        res += tab(t+3) + "\"composed_of\": [" + endl();
        res += tab(t+4) + "{ \"type_string\": \"string\" }," + endl();
        res += tab(t+4) + "{ \"fields\": \"according to type\" }" + endl();
        res += tab(t+3) + "]" + endl();
        res += tab(t+2) + "}," + endl();

        res += tab(t+2) + "\"alignment\": 1," + endl();
        res += tab(t+2) + "\"endianess\": \"little-endian\"" + endl();
        res += tab(t+1) + "}" + endl();
        res += tab(t) + "}" + endl();
        return res;
    }

    protected String generate_OPSObject_JSON(int t)
    {
        String res = "";
        res += tab(t) + "{" + endl();
        res += tab(t+1) + "\"type\": \"ops.OPSObject\"," + endl();

        res += tab(t+1) + "\"fields\": [" + endl();
        res += tab(t+2) + "{";
        res += " \"name\": \"key\",";
        res += " \"type\": \"string\"";
        res += " }" + endl();
        res += tab(t+1) + "]" + endl();

        res += tab(t) + "}" + endl();
        return res;
    }

    protected String makeType(IDLField field)
    {
      String ty = field.getType().replace("[]", "");
      if (field.isIdlType()) {
        ty = getFullyQualifiedClassName(ty);
      }
      if (field.isAbstract()) {
        return "virtual " + ty;
      } else {
        return ty;
      }
    }

    protected String generateJSONobject(int t, IDLClass idlClass)
    {
        String res = "";
        res += tab(t) + "{" + endl();
        res += tab(t+1) + "\"type\": \"" + idlClass.getPackageName() + "." + idlClass.getClassName() + "\"," + endl();
        String baseClass = "ops.OPSObject";
        if (idlClass.getBaseClassName() != null) {
          baseClass = idlClass.getBaseClassName();
        }
        res += tab(t+1) + "\"extends\": \"" + getFullyQualifiedClassName(baseClass) + "\"," + endl();

        if (idlClass.getType() == IDLClass.ENUM_TYPE) {
          res += tab(t+1) + "\"enum\": [";
          String pre = "";
          for (String str : idlClass.getEnumNames()) {
            res += pre + "\"" + str + "\"";
            pre = ", ";
          }
          res += "]" + endl();

        } else {
          res += tab(t+1) + "\"fields\": [" + endl();

          boolean first = true;
          for (IDLField field : idlClass.getFields()) {
            if (!first) res += "," + endl();
            first = false;
            res += tab(t+2) + "{";
            res += " \"name\": \"" + field.getName() + "\"";
            if (field.isArray()) {
              res += ", \"type\": \"vector<T>\", \"elementtype\": ";

            } else {
              res += ", \"type\": ";

            }
            res += "\"" + makeType(field) + "\"";

            String comment = field.getComment();
            comment = comment.replace("/*", "").replace("*/", "").replace("\n", " ").replace("\"", "'").trim();
            if (comment.length() > 0) {
              res += ", \"desc\": \"" + comment + "\"";
            }
            res += " }";
          }

          res += endl() + tab(t+1) + "]" + endl();
        }
        res += tab(t) + "}" + endl();
        return res;
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

    protected String getFullyQualifiedClassName(String className)
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

    public String getName()
    {
        return "JSONIDLCompiler";
    }

}
