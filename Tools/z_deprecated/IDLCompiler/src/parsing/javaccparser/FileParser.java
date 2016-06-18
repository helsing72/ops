/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package parsing.javaccparser;

import java.io.ByteArrayInputStream;
import java.util.logging.Level;
import java.util.logging.Logger;
import parsing.IDLClass;
import parsing.IDLField;
import parsing.IDLFileParser;
import parsing.ParseException;

/**
 *
 * @author angr
 */
public class FileParser implements IDLFileParser
{
    boolean parseComplete = false;
    IDLClass idlClass = new IDLClass();
    private String pendingComment = "";
    public IDLClass parse(String content) throws ParseException
    {

        idlClass = new IDLClass();
        IDLParser parser = new IDLParser(new ByteArrayInputStream(content.getBytes()));

        parser.idlDeclareEvent.add(new ParserEventCallback<String>()
        {

            public void onEvent(String eventData, ParserEvent e)
            {

                idlClass.setClassName(eventData);
                pendingComment = "";
            }
        });
        parser.idlCloseEvent.add(new ParserEventCallback<String>()
        {

            public void onEvent(String eventData, ParserEvent e)
            {
                parseComplete = true;
                pendingComment = "";
            }
        });

        parser.packageDeclareEvent.add(new ParserEventCallback<String>()
        {

            public void onEvent(String eventData, ParserEvent e)
            {
                idlClass.setPackageName(eventData);
                pendingComment = "";
            }
        });

        parser.enumDeclareEvent.add(new ParserEventCallback<String>()
        {

            public void onEvent(String eventData, ParserEvent e)
            {
                System.out.println("enums are ignored.");
            }
        });
        parser.enumCloseEvent.add(new ParserEventCallback<String>()
        {

            public void onEvent(String eventData, ParserEvent e)
            {
                System.out.println("enums are ignored.");
            }
        });
        parser.enumElementEvent.add(new ParserEventCallback<String>()
        {

            public void onEvent(String eventData, ParserEvent e)
            {
                System.out.println("enums are ignored.");
            }
        });
        parser.fieldDeclareEvent.add(new ParserEventCallback<IDLField>()
        {

            public void onEvent(IDLField eventData, ParserEvent e)
            {
                eventData.setComment(pendingComment);
                idlClass.addIDLField(eventData);
                pendingComment = "";
            }
        });
        parser.extendsEvent.add(new ParserEventCallback<String>()
        {

            public void onEvent(String eventData, ParserEvent e)
            {
                idlClass.setBaseClassName(eventData);
            }
        });
        parser.commentEvent.add(new ParserEventCallback<String>()
        {


            public void onEvent(String eventData, ParserEvent e)
            {
                pendingComment = eventData;
            }
        });


        try
        {
            parser.specification();
            return idlClass;
        } 
        catch (parsing.javaccparser.ParseException parseException)
        {
            throw new ParseException(parseException.getMessage());
        }
        catch (TokenMgrError parseException)
        {
            throw new ParseException(parseException.getMessage());
        }

    }
    public static void main(String[] args)
    {
        try
        {
            FileParser fp = new FileParser();
            String filetext = "//lsdnknkldgfndkgldfngl\npackage testpack.parpack; //hehehhfjbnjbjbsv \nclass TestClass extends idltype test.TestBaseClass //jklnfasnkfonfdgh \n{ //hatte svat.char[] chattAr; ntratt\ndouble d ;\n double[] dubs ;idltype fnatt.cat.Tratt t; idltype svatt.chat[] chattAr;////fghfknlgfhn\n}";
            IDLClass idlClass = fp.parse(filetext);
            System.out.println("" + idlClass);
        } catch (ParseException ex)
        {
            Logger.getLogger(FileParser.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
