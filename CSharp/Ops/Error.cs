///////////////////////////////////////////////////////////
//  Error.cs
//  Implementation of the Interface Error
//  Created on:      12-nov-2011 09:25:30
//  Author:
///////////////////////////////////////////////////////////

namespace Ops 
{
	public interface IError 
    {

        int GetErrorCode();

		string GetErrorMessage();

		string GetSource();
	}

}