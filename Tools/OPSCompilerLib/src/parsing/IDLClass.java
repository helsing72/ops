/*
 * IDLClass.java
 *
 * Created on den 12 november 2007, 08:18
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package parsing;

import java.util.ArrayList;

/**
 *
 * @author angr
 */
public class IDLClass
{
    private static final int CLASS_TYPE = 0;
    public static final int ENUM_TYPE = 1;


    private int type = CLASS_TYPE;
    private String packageName;
    private String className;
    private String baseClassName;
    private String comment;
    private String directive;
    private ArrayList<IDLField> fields;

    private ArrayList<String> enumNames = new ArrayList<String>();
    /** Creates a new instance of IDLClass */
    public IDLClass(String className, String packageName, ArrayList<IDLField> fields)
    {
        this.className = className;
        this.packageName = packageName;
        this.fields = fields;
    }

    public IDLClass(String className, String packageName)
    {
        this(className, packageName, new ArrayList<IDLField>());
    }
    public IDLClass()
    {
        this(null, null, new ArrayList<IDLField>());
    }

    public void addIDLField(IDLField f)
    {
        fields.add(f);
    }
    public String getPackageName()
    {
        return packageName;
    }

    public void setPackageName(String packageName)
    {
        this.packageName = packageName;
    }

    public String getClassName()
    {
        return className;
    }

    public void setClassName(String className)
    {
        this.className = className;
    }

    public String getDirective()
    {
        return directive;
    }

    public void setDirective(String directive)
    {
        this.directive = directive;
    }

    public String getComment()
    {
        return comment;
    }

    public void setComment(String comment)
    {
        this.comment = comment;
    }

    public ArrayList<IDLField> getFields()
    {
        return fields;
    }

    public void setFields(ArrayList<IDLField> fields)
    {
        this.fields = fields;
    }

    public void setBaseClassName(String bcName)
    {
        baseClassName = bcName;
    }
    public String getBaseClassName()
    {
        return baseClassName;
    }

    public int getType()
    {
        return type;
    }

    public void setType(int type)
    {
        this.type = type;
    }

    public ArrayList<String> getEnumNames()
    {
        return enumNames;
    }

    public void setEnumNames(ArrayList<String> enumNames)
    {
        this.enumNames = enumNames;
    }

}
