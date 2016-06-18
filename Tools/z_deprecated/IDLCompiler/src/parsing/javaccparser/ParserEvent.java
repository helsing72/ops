/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package parsing.javaccparser;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author angr
 */
public class ParserEvent<T>
{
    List<ParserEventCallback<T>> callbacks = new ArrayList<ParserEventCallback<T>>();

    public boolean remove(Object o)
    {
        return callbacks.remove(o);
    }

    public boolean add(ParserEventCallback<T> e)
    {
        return callbacks.add(e);
    }

    public void fireEvent(T eventData)
    {
        for (ParserEventCallback<T> parserEventCallback : callbacks)
        {
            parserEventCallback.onEvent(eventData, this);
        }
    }

}
