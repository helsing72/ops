/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package parsing;

import antlr.RecognitionException;
import antlr.TokenStreamException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.ws.jaxme.js.JavaConstructor;
import org.apache.ws.jaxme.js.JavaField;
import org.apache.ws.jaxme.js.JavaMethod;
import org.apache.ws.jaxme.js.JavaSource;
import org.apache.ws.jaxme.js.JavaSourceFactory;
import org.apache.ws.jaxme.js.util.JavaParser;

/**
 *
 * @author angr
 */
public class JAXMeIDLFileParser implements IDLFileParser
{

    public IDLClass parse(String content) throws ParseException
    {
        try
        {

            content = filterOutComments(content);
            content = trim(content);

            IDLClass idlClass = new IDLClass();
            JavaSourceFactory jsf = new JavaSourceFactory();
            JavaParser jp = new JavaParser(jsf);
            jp.parse(new StringReader(content));


            //We only parse one class per file.
            Iterator iter = jsf.getJavaSources();
            JavaSource js = (JavaSource) iter.next();

            idlClass.setClassName(js.getClassName());
            idlClass.setPackageName(js.getPackageName());
            idlClass.setBaseClassName(js.getExtends()[0].getClassName());

            JavaField[] fields = js.getFields();

            JavaMethod[] methods = js.getMethods();
            if(methods.length > 0 )
            {
                ParseException pe = new ParseException("Methods are not supported.");
                throw pe;
            }

            JavaConstructor[] constructors = js.getConstructors();
            if(constructors.length > 0 )
            {
                ParseException pe = new ParseException("Constructors are not supported.");
                throw pe;
            }

            for (JavaField field : fields)
            {
                if(field.getProtection().equals(JavaSource.DEFAULT_PROTECTION))
                {
                    IDLField newField = new IDLField(field.getName(), field.getType().getClassName());
                    

                    if(field.isStatic())
                    {
                        newField.setStatic(true);
                    }
                    if(isArray(field, content))
                    {
                        newField.setType(newField.getType() + "[]");
                    }

                    idlClass.addIDLField(newField);
                }
                else
                {
                    //This should result in error
                    ParseException pe = new ParseException("Protection level \"" + field.getProtection() + "\" is not supported.");
                    throw pe;
                }
            }

            return idlClass;
        } catch (RecognitionException ex)
        {
            throw new ParseException(ex.getMessage());
        } catch (TokenStreamException ex)
        {
            throw new ParseException(ex.getMessage());
        }
    }

    private String filterOutComments(String content)
    {
        String ret = null;
        try
        {
            ret = "";
            int index = 0;
            while (true)
            {
                int nextCommentStartIndex = content.indexOf("//", index);
                if(nextCommentStartIndex == -1)
                {
                    ret += content.substring(index, content.length());

                }
                ret += content.substring(index, nextCommentStartIndex);
                index = content.indexOf("\n", nextCommentStartIndex);

            }
        } catch (Exception e)
        {
            return ret;
        }

    }

    private boolean isArray(JavaField field, String context)
    {

            String fieldString = field.getType().getClassName() + "[] " + field.getName() + ";";
            int fieldStart = context.indexOf(fieldString.substring(0, fieldString.length() - 0));
            if(fieldStart == -1)
            {
                return false;
            }
            else
            {
                return true;
            }

    }

    private String trim(String content)
    {
        String ret = "";
        content = content.replaceAll("\t", " ");
        content = content.replaceAll("\r", " ");
        content = content.replaceAll("\n", " ");
        String[] strings = content.split(" ");

        String temp = "";
        for (String string : strings)
        {
            temp += " " + string.trim();
        }

        String[] strings2 = temp.split(";");

        boolean first = true;
        for (String string : strings2)
        {
            if(first)
            {
                ret += "" + string.trim();
                first = false;
            }
            else
                ret += ";" + string.trim();
        }

        return ret;
    }
}
