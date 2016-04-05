/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package parsing;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.logging.FileHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author angr
 */
public abstract class AbstractTemplateBasedIDLCompiler implements IDLCompiler
{

    public static final String CLASS_NAME_REGEX = "__className";
    public static final String INCLUDES_REGEX = "__includes";
    public static final String PACKAGE_NAME_REGEX = "__packageName";
    protected String outputFileName;
    protected String templateFileName = "";
    protected String templText = null;
    private String tabString = "\t";
    private String endlString = "\r\n";

    protected String getTemplateText()
    {
        if (templText == null)
        {
            FileInputStream fis = null;
            try
            {
                fis = new FileInputStream(templateFileName);
                byte[] inBytes = new byte[fis.available()];
                fis.read(inBytes);
                templText = new String(inBytes);
                return templText;
            } catch (IOException ex)
            {

                return null;
            } finally
            {
                try
                {
                    fis.close();
                } catch (IOException ex)
                {

                    return null;
                }
            }
        }
        return templText;
    }

    public void setTemplateText(String templText)
    {
        this.templText = templText;
    }



    protected void saveOutputText(String templateText)
    {
        FileOutputStream fos = null;
        try
        {
            File outFile = new File(outputFileName);
            File outFilePath = new File(outputFileName.substring(0, outputFileName.lastIndexOf("/")));

            outFilePath.mkdirs();
            outFile.createNewFile();


            fos = new FileOutputStream(outFile);
            fos.write(templateText.getBytes());
        } catch (IOException ex)
        {
            Logger.getLogger(AbstractTemplateBasedIDLCompiler.class.getName()).log(Level.SEVERE, null, ex);
        } finally
        {
            try
            {
                fos.close();
            } catch (IOException ex)
            {
                //Logger.getLogger(CppFactoryIDLCompiler.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    protected String tab(int i)
    {
        String ret = "";
        for (int j = 0; j < i; j++)
        {
            ret += tabString;
        }
        return ret;
    }

    public String getOutputFileName()
    {
        return outputFileName;
    }

    public void setOutputFileName(String outputFileName)
    {
        this.outputFileName = outputFileName;
    }

    public String getTabString()
    {
        return tabString;
    }

    public void setTabString(String tabString)
    {
        this.tabString = tabString;
    }

    public String getTemplateFileName()
    {
        return templateFileName;
    }

    public void setTemplateFileName(String templateFileName)
    {
        this.templateFileName = templateFileName;
    }

    public void setEndlString(String endlString)
    {
        this.endlString = endlString;
    }

    protected String endl()
    {
        return endlString;
    }
}
