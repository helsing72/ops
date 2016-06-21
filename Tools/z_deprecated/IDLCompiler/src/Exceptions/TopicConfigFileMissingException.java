/*
 * TopicConfigFileMissingException.java
 *
 * Created on den 28 november 2007, 10:55
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package Exceptions;

/**
 *
 * @author angr
 */
public class TopicConfigFileMissingException extends Exception
{
    
    /** Creates a new instance of TopicConfigFileMissingException */
    public TopicConfigFileMissingException()
    {
        super("TopicConfigFile is missing in project");
    }
    
}
