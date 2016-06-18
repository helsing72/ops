/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package idlcompiler.gui;

import java.awt.Frame;
import javax.swing.JOptionPane;

/**
 *
 * @author angr
 */
public class GUIHelper
{
    
    public static void showErrorMessage(String s, Frame parent)
    {
        JOptionPane.showMessageDialog(parent, s);
    }
}
