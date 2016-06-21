/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package parsing.javaccparser;

/**
 *
 * @author angr
 */
public interface ParserEventCallback<T>
{
    public void onEvent(T eventData, ParserEvent evt);

}
