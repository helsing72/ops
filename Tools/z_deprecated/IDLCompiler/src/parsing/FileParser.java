/*
 * FileParser.java
 *
 * Created on den 12 november 2007, 08:07
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package parsing;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

/**
 *
 * @author angr
 */
public class FileParser implements IDLFileParser
{
    BufferedReader reader;
    
    private int readingPackage = 1;
    private int readingClassName = 2;
    private int searchingForBody = 3;
    private int readingBody = 4;
    private int state = readingPackage;
    
    private int currentLineNr = 0;
    private String currentLine = "";
    
    private IDLClass currentClass;
    
    private boolean foundOpeningBracket = false;
    
    private final String supportedTypes = " boolean boolean[] byte byte[] int int[] long long[] float float[] double double[] string string[] idltype ";
    private final String supportedNameCharacters = "_1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    
    private final static int FIELD_COMMENT = 0;
    private final static int NUMERIC_FIELD = 1;
    private final static int STRING_FIELD = 2;
    private final static int IDLTYPE_FIELD = 3;
    private boolean enumClass;
    private boolean hasExtends = false;
    
    /**
     * Creates a new instance of FileParser
     */
    public FileParser()
    {
    }
    
    public static String filterOutComments(String text) throws IOException
    {
        BufferedReader br = setupReader(text);
        String out = "";
        while(true)
        {
            String temp = br.readLine();
            if(temp == null)
                break;
            if(!temp.trim().startsWith("//"))
                out += temp;
        }
        return out;
        
    }
    
    public IDLClass parse(String context) throws ParseException
    {
        try
        {
            reader = setupReader(context);
            currentClass = new IDLClass();
            currentLineNr = 0;
            currentLine = "";
            foundOpeningBracket = false;

            readPackage();
            readClassName();
            readBody();


        } catch (NullPointerException exception)
        {
            throw new ParseException("Caused by nullpointer.");
        }

        return currentClass;
        
    }
    
    
    private void readPackage() throws ParseException
    {
        try
        {
            String[] nextLine = null;
            do
            {
                nextLine = splitLine();
                
            }
            while(!validatePackageDeclaration(nextLine));
            currentClass.setPackageName(nextLine[1].replace(";", ""));
        }
        catch (IOException ex)
        {
            throw new ParseException("At line " + currentLine + ": " + ex.getMessage());
        }
        
    }
    
    private void readClassName() throws ParseException
    {
        try
        {
            String[] nextLine = null;
            do
            {
                nextLine = splitLine();
                
            }
            while(!validateClassDeclaration(nextLine));
            
            currentClass.setClassName(nextLine[1].replace("{", ""));
            if(hasExtends)
            {
                currentClass.setBaseClassName(nextLine[3].replace("{", ""));
            }
        }
        catch (IOException ex)
        {
            throw new ParseException("At line " + currentLine + ": " + ex.getMessage());
        }
    }
    
    private void readBody() throws ParseException
    {
        try
        {
            if(!foundOpeningBracket)
            {
                findOpeningBracket();
            }
            if(this.enumClass)
            {
                //TODO: Here should be put enum validation code.
                return;
            }
            
            String[] nextLine = splitLine();
            while(nextLine != null)
            {

                IDLField field = new IDLField();
                while(isComment(nextLine) || isEmptyLine(nextLine))
                {
                    field.setComment(field.getComment() + currentLine);
                    nextLine = splitLine();
                }
                
                if(endBody(nextLine))
                {
                    break;
                }
                
                validateFieldDeclaration(nextLine);
                if(nextLine[0].equals("idltype"))
                {
                    field.setType(nextLine[1]);
                    field.setName(nextLine[2].replace(";",""));
                    field.setIdlType(true);
                }
                else
                {
                    field.setType(nextLine[0]);
                    field.setName(nextLine[1].replace(";",""));
                    field.setIdlType(false);
                    
                }
                
                nextLine = splitLine();
                currentClass.addIDLField(field);
            }
            
        }
        catch (IOException ex)
        {
            throw new ParseException("At line " + currentLine + ": " + ex.getMessage());
        }
    }
    
    private static BufferedReader setupReader(String s)
    {
        ByteArrayInputStream bais = new ByteArrayInputStream(s.getBytes());
        
        return new BufferedReader(new InputStreamReader(bais));
        //return null;
    }
    
    private String[] splitLine() throws IOException
    {
        currentLineNr ++;
        while((currentLine = reader.readLine().trim()).startsWith("//"));
        
        return trimSplit(currentLine.split(" "));
    }
    
    private boolean validatePackageDeclaration(String[] nextLine) throws ParseException
    {
        if(isEmptyLine(nextLine))
        {
            return false;
        }
        else if(nextLine.length > 3)
        {
            throw new ParseException("At line " + currentLineNr + ": Invalid package declaration.");
        }
        else if(nextLine.length > 2 && !nextLine[nextLine.length -1].equals(";"))
        {
            throw new ParseException("At line " + currentLineNr + ": Invalid package declaration.");
        }
        else if(!nextLine[0].equals("package"))
        {
            throw new ParseException("At line " + currentLineNr + ": Package declaration must begin with keyword package.");
        }
        
        validateEndsWith(";", nextLine);
        
        return true;
    }
    private boolean validateClassDeclaration(String[] nextLine) throws ParseException
    {
        
        if(isEmptyLine(nextLine))
        {
            return false;
        }
        else if (nextLine.length > 5)
        {
            throw new ParseException("At line " + currentLineNr + ": Invalid class declaration.");
        }
        else if(nextLine.length > 3)
        {
            
            if(nextLine[2].equals("extends"))
            {
                hasExtends = true;
            }
            else
            {
                throw new ParseException("At line " + currentLineNr + ": Invalid class declaration.");
            }
        }
        else if(!hasExtends && nextLine.length > 2  && nextLine[1].endsWith("{") && !nextLine[1].equals("{"))
        {
            throw new ParseException("At line " + currentLineNr + ": Invalid class declaration.");
        }
        
        else if(!nextLine[0].equals("class") && !nextLine[0].equals("enum"))
        {
            throw new ParseException("At line " + currentLineNr + ": Class declaration must begin with keyword class.");
        }
        else if(endsWith("{", nextLine))
        {
            foundOpeningBracket = true;
        }
        if(nextLine[0].equals("enum"))
        {
            enumClass = true;
        }
        if(!hasExtends)
            validateName(nextLine[nextLine.length - 1].replace("{", ""));
        else
        {
            validateName(nextLine[1]);
        }
        
        return true;
    }
    private boolean endsWith(String regex, String[] nextLine)
    {
        return nextLine[nextLine.length - 1].endsWith(regex);
    }
    
    private void validateEndsWith(final String regex, final String[] nextLine) throws ParseException
    {
        if(!endsWith(regex, nextLine))
        {
            throw new ParseException("At line " + currentLineNr + ": Missing ; .");
        }
    }
    private boolean isEmptyLine(String[] nextLine)
    {
        if(nextLine.length < 1)
        {
            //empty line, ok.
            return true;
        }
        else if(nextLine.length == 1 && (nextLine[0].equals("") || nextLine[0].equals("\t")))
        {
            //empty line, ok.
            return true;
        }
        return false;
    }
    
    private String[] trimSplit(String[] string)
    {
        ArrayList<String> words = new ArrayList<String>();
        
        for (int i = 0; i < string.length; i++)
        {
            if(!string[i].equals(" ") && !string[i].equals("\t") && !string[i].equals(""))
            {
                words.add(string[i].trim());
            }
        }
        String[] temp = new String[words.size()];
        return (String[]) words.toArray(temp);
    }
    
    private boolean endBody(String[] nextLine)
    {
        if(nextLine[0].equals("}"))
            return true;
        else
            return false;
    }
    
    private boolean validateFieldDeclaration(String[] nextLine) throws ParseException
    {
        if(isEmptyLine(nextLine))
        {
            return false;
        }
        if(!fieldStartsWithSupportedType(nextLine))
        {
            throw new ParseException("At line " + currentLineNr + ": " + nextLine[0] + ", Type not supported.");
        }
        
        else if(nextLine.length > 4)
        {
            throw new ParseException("At line " + currentLineNr + ": Invalid field declaration.");
        }
        else if(nextLine.length > 3 && !nextLine[nextLine.length - 1].equals("idltype"))
        {
            throw new ParseException("At line " + currentLineNr + ": Invalid field declaration.");
        }
        
        validateName(nextLine[nextLine.length - 1].replace(";", ""));
        
        validateEndsWith(";", nextLine);
        return true;
    }
    
    private boolean isComment(String[] nextLine)
    {
        if(isEmptyLine(nextLine))
        {
            return false;
        }
        else if(nextLine[0].startsWith("//"))
            return true;
        else
            return false;
    }
    
    private void findOpeningBracket() throws IOException, ParseException
    {
        String[] nextLine = null;
        do
        {
            nextLine = splitLine();
        }
        while(!validateSearchForOpeningBracket(nextLine));
    }
    
    private boolean validateSearchForOpeningBracket(String[] nextLine) throws ParseException
    {
        if(isEmptyLine(nextLine))
        {
            return false;
        }
        else if(nextLine.length > 1 || !nextLine[0].equals("{"))
        {
            throw new ParseException("At line " + currentLineNr + ": Invalid body declaration.");
        }
        foundOpeningBracket = true;
        return true;
    }
    
    private boolean fieldStartsWithSupportedType(String[] nextLine)
    {
        String temp = " " + nextLine[0] + " ";
        if(supportedTypes.contains(temp))
        {
            return true;
        }
        else
        {
            return false;
        }
    }
    
    private void validateName(String string) throws ParseException
    {
        for(int i = 0 ; i < string.length() ; i++)
        {
   
            if(!supportedNameCharacters.contains(string.substring(i, i + 1)))
            {
                throw new ParseException("At line " + currentLineNr + ": Invalid name.");
            }
            
        }
        if(string.length() < 1 || string.equals(" ") || string.equals("\t"))
        {
            throw new ParseException("At line " + currentLineNr + ": Missing name.");
        }
        else if("1234567890".contains(string.substring(0,1)))
        {
            throw new ParseException("At line " + currentLineNr + ": Invalid name.");
        }
    }
    
    
    
    
}
