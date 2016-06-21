/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package parsing;

/**
 *
 * @author angr
 */
public interface IDLFileParser
{
    IDLClass parse(String context) throws ParseException;
}
