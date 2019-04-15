/** -*-java-*- Copyright (C) 2014 Saab Dynamics AB
 ***************************************************************************
 *  @file   IDLParser.java
 *  @author Mattias Helsing <mattias.helsing@saabgroup.com>
 *          Updated/Modified by Lennart Andersson
 *
 * This file is based on:
 *   Tools/NBOPSIDLSupport/src/ops/netbeansmodules/idlsupport/ProjectIDLParser.java
 ***************************************************************************
 */

package opsc;

import java.io.IOException;
import java.util.Vector;
import parsing.IDLClass;
import parsing.ParseException;

/**
 *
 * @author helm
 */
public class IDLParser
{
    Vector<IDLClass> _idlClasses = new Vector<IDLClass>();
    // FileParser from IDLParser jar
    parsing.javaccparser.FileParser _parser = new parsing.javaccparser.FileParser();

    private int _nrErrors = 0;
    private int _nrWarnings = 0;

    public IDLParser()
    {
    }

    public void reset()
    {
        _idlClasses.clear();
        _nrErrors = 0;
        _nrWarnings = 0;
    }

    public IDLClass parse(String name, String fileText)
    {
        try
        {
            IDLClass newClass = _parser.parse(fileText);
            if(newClass.getClassName().equals(name))
            {
                _idlClasses.add(newClass);
                return newClass;
            }
            else
            {
                System.out.println("In " + name + ", Error: File " + name + " does not contain a class called " + name + ".");
                _nrErrors++;
            }
        }
        catch (ParseException ex)
        {
            System.out.println("In " + name + ", Error: " + ex.getMessage());
            _nrErrors++;
        }
        return null;
    }

    public Vector<IDLClass> getIdlClasses()
    {
        return _idlClasses;
    }

    public int getNrErrors()
    {
        return _nrErrors;
    }

    public int getNrWarnings()
    {
        return _nrWarnings;
    }
}
