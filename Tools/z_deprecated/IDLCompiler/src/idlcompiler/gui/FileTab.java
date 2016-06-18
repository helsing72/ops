/*
 * FileTabPanel.java
 *
 * Created on den 31 juli 2007, 08:26
 */
package idlcompiler.gui;

import idlcompiler.files.TextFile;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.ByteArrayInputStream;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.TabSet;
import javax.swing.text.TabStop;
import org.javacc.parser.Token;
import parsing.javaccparser.IDLParser;
import parsing.javaccparser.IDLParserConstants;
import parsing.javaccparser.TokenMgrError;

/**
 *
 * @author  xhewe
 */
public class FileTab extends javax.swing.JPanel
{

    private TextFile textFile;
    private DefaultStyledDocument doc;
    private Style reservedStyle;
    private Style commentStyle;
    private Style mainStyle;
    private Style baseStyle;
    private JTextPane textPane;
    private JScrollPane scrollPane;

    /** Creates new form FileTabPanel */
    public FileTab(TextFile f)
    {
        textFile = f;
        //

        initComponents();

        // Create the StyleContext, the document and the pane
        StyleContext sc = new StyleContext();
        doc = new DefaultStyledDocument(sc);

        textPane = new JTextPane();
        textPane.setText(new String(f.getText().getBytes()));

        setLayout(new BorderLayout());
        JPanel noWrapPanel = new JPanel();
        noWrapPanel.setLayout(new BorderLayout());
        noWrapPanel.add(textPane);

        scrollPane = new JScrollPane(noWrapPanel);
        scrollPane.setPreferredSize(new Dimension(500, 400));
        add(scrollPane);

        textPane = new JTextPane()
        {

            public void setSize(Dimension d)
            {
                if (d.width < getParent().getSize().width)
                {
                    d.width = getParent().getSize().width;
                }

                super.setSize(d);
            }

            public boolean getScrollableTracksViewportWidth()
            {
                return false;
            }
        };

        scrollPane = new JScrollPane(textPane);
        scrollPane.setPreferredSize(new Dimension(500, 400));
        add(scrollPane);

        //jTextPane1 = new JTextPane(doc);
        textPane.setDocument(doc);


        //jEditorPane = new JEditorPane(

        // Create and add the main document style
        Style defaultStyle = sc.getStyle(StyleContext.DEFAULT_STYLE);
        mainStyle = sc.addStyle("mainStyle", null);
        //StyleConstants.setLeftIndent(mainStyle, 5);
        //StyleConstants.setRightIndent(mainStyle, 16);
        //StyleConstants.setFirstLineIndent(mainStyle, 16);
        StyleConstants.setFontFamily(mainStyle, "monospaced");
        StyleConstants.setForeground(mainStyle, Color.black);
        StyleConstants.setFontSize(mainStyle, 12);
        //StyleConstants.
        //TabStop[] tabStops = {new TabStop(20)};
        //StyleConstants.setTabSet(mainStyle, new TabSet(tabStops));

        // Create and add the constant width style
        reservedStyle = sc.addStyle("reservedStyle", null);
        StyleConstants.setFontFamily(reservedStyle, "monospaced");
        StyleConstants.setFontSize(reservedStyle, 12);
        StyleConstants.setForeground(reservedStyle, Color.blue);
        //StyleConstants.setTabSet(reservedStyle, new TabSet(tabStops));

        // Create and add the constant width style
        commentStyle = sc.addStyle("commentStyle", null);
        StyleConstants.setFontFamily(commentStyle, "monospaced");
        StyleConstants.setFontSize(commentStyle, 12);
        StyleConstants.setForeground(commentStyle, Color.orange);
        //StyleConstants.setTabSet(commentStyle, new TabSet(tabStops));

        // Create and add the constant width style
        baseStyle = sc.addStyle("baseStyle", null);
        StyleConstants.setFontFamily(baseStyle, "monospaced");
        StyleConstants.setFontSize(baseStyle, 12);
        StyleConstants.setForeground(baseStyle, Color.gray);
        //StyleConstants.setTabSet(baseStyle, new TabSet(tabStops));

        textPane.setText(new String(f.getText().getBytes()));
        

//        try
//        {
        doc.setLogicalStyle(0, mainStyle);
        doc.setCharacterAttributes(0, doc.getLength(), mainStyle, true);

        //doc.insertString(0, "", null);

        parseHighlight();

//        }
//        catch (BadLocationException ex)
//        {
//            ex.printStackTrace();
//        }

        textPane.addKeyListener(new KeyListener()
        {

            public void keyPressed(KeyEvent e)
            {
            }

            public void keyReleased(KeyEvent e)
            {
                //highlightAll();
                parseHighlight();
            }

            public void keyTyped(KeyEvent e)
            {



            }


        });
//        jTextPane1.addCaretListener(new CaretListener()
//        {
//            public void caretUpdate(CaretEvent e)
//            {
//                //highlightAll();
//            }
//        });
//        




    }

    private int getOffset(int beginLine, int beginColumn)
    {
        int offset = 0;
        String[] textLines = getText().split("\n");
        for(int i = 0; i < beginLine - 1; i ++ )
        {
            //textLines[i] = textLines[i].replace("\n", "");
            offset += textLines[i].length() + 1;
        }
        offset += beginColumn;
        return offset -1;

    }

    private void highlightToken(parsing.javaccparser.Token token, Style style)
    {
        
        int startPos = getOffset(token.beginLine, token.beginColumn);
        doc.setCharacterAttributes(startPos, token.image.length(), style, true);


    }

    private boolean isReservedWord(String image)
    {
        String reserved = "package class enum extends idltype byte short int long float double string boolean";
        String[] reservedWord = reserved.split(" ");

        for (String string : reservedWord)
        {
            if(image.equals(string))
            {
                return true;
            }
        }

        return false;
    }

    private void parseHighlight()
    {
        String text = getText().replaceAll("\t", " ");
        text = text.replaceAll("\r", "");
        
        ByteArrayInputStream bais = new ByteArrayInputStream(text.getBytes());
        IDLParser parser = new IDLParser(bais);

        doc.setCharacterAttributes(0, doc.getLength(), baseStyle, true);
        while(true)
        {
            try
            {
                parsing.javaccparser.Token token = parser.getNextToken();
                if (token.kind == IDLParserConstants.EOF)
                {
                    break;
                }
                if(token.kind == IDLParserConstants.ANOTATION)
                {
                    highlightToken(token, commentStyle);
                }
                else if (isReservedWord(token.image))
                {
                    highlightToken(token, reservedStyle);
                }
                else
                {
                    highlightToken(token, mainStyle);
                }
            } catch (Throwable e)
            {
                System.out.println(e.getClass().getName());
                break;
            }
        }
    }
    private void highlightAll()
    {
        doc.setCharacterAttributes(0, doc.getLength(), mainStyle, false);
        highlight("package ");
        highlight("class ");
        highlight("byte ");
        highlight("byte[] ");
        highlight("boolean ");
        highlight("boolean[] ");
        highlight("float ");
        highlight("float[] ");
        highlight("double ");
        highlight("double[] ");
        highlight("int ");
        highlight("int[] ");
        highlight("idltype ");
        highlight("string ");
        highlight("string[] ");
        highlight("long ");
        highlight("long[] ");
    }

    private void highlight(String word)
    {
        int classIndex = -1;
        while (getText().indexOf(word, classIndex + 1) != -1)
        {
//            classIndex = getText().indexOf(word, classIndex + 1);
//            if("\t\n ".contains("" + getText().charAt(classIndex)) && "\t\n; ".contains("" + getText().charAt(classIndex + word.length() + 1)))
//            {
//                doc.setCharacterAttributes(classIndex, word.length(), cwStyle, false);
//            }

            classIndex = getText().indexOf(word, classIndex + 1);
            if ("\t\n\r ".contains("" + getText().charAt(Math.max(classIndex - 1, 0))) || classIndex == 0)
            {
                doc.setCharacterAttributes(classIndex, word.length(), reservedStyle, false);
            }
        }
    }

    public String getText()
    {
        return textPane.getText();

    }

    public TextFile getTextFile()
    {
        return textFile;
    }

    public void save()
    {
        textFile.setText(getText());
        textFile.save();


    }

    public JEditorPane getJEditorPane()
    {
        return textPane;
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(0, 460, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(0, 339, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables
}
