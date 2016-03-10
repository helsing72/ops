///////////////////////////////////////////////////////////
//  ParticipantFactory.cs
//  Implementation of the Class ParticipantFactory
//  Created on:      12-nov-2011 09:25:33
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Collections.Generic;
using System.IO;

namespace Ops 
{
	internal class ParticipantFactory {

        private Dictionary<string, Participant> instances = new Dictionary<string, Participant>();

		/// 
		/// <param name="domainID"></param>
		/// <param name="participantID"></param>
		internal Participant GetParticipant(string domainID, string participantID)
        {
            return GetParticipant(domainID, participantID, null);
        }

		/// 
		/// <param name="domainID"></param>
		/// <param name="participantID"></param>
		/// <param name="file"></param>
		internal Participant GetParticipant(string domainID, string participantID, string configFile)
        {
            string hashKey = domainID + " " + participantID;

            if (!instances.ContainsKey(hashKey))
            {
                Participant newInst = new Participant(domainID, participantID, configFile);
                Domain tDomain = newInst.getDomain();

                if (tDomain != null)
                {
                    instances.Add(hashKey, newInst);
                }
                else
                {
                    return null;
                }
            }
            return instances[hashKey];
        }

		/// 
		/// <param name="domain"></param>
		/// <param name="participantID"></param>
		internal Participant GetParticipant(Domain domain, string participantID)
        {
            string hashKey = domain.GetDomainID() + " " + participantID;

            if (!instances.ContainsKey(hashKey))
            {
                Participant newInst = new Participant(domain, participantID);
                Domain tDomain = newInst.getDomain();

                if (tDomain != null)
                {
                    instances.Add(hashKey, newInst);
                }
                else
                {
                    return null;
                }
            }
            return instances[hashKey];
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="domainID"></param>
        /// <param name="participantID"></param>
        internal void RemoveParticipant(string domainID, string participantID)
        {
            string hashKey = domainID + " " + participantID;

            if (instances.ContainsKey(hashKey))
            {
                instances[hashKey] = null;
                instances.Remove(hashKey);
            }
        }

	}

}