/**
*
* Copyright (C) 2010-2012 Saab Dynamics AB
*   author Lennart Andersson <nnnn@saabgroup.com>
*
* Copyright (C) 2018-2019 Lennart Andersson.
*
* This file is part of OPS (Open Publish Subscribe).
*
* OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.

* OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
*/

#include <mutex>

#ifdef _WIN32
	#include "SEHException.h"
#endif

#include "CWorkerThread.h"

// ---------------------------------------------------------------------

std::mutex instanceMutex;

CWorkerThreadManager* CWorkerThreadManager::_instance = nullptr;

CWorkerThreadManager* CWorkerThreadManager::Instance()
{
	std::lock_guard<std::mutex> lck(instanceMutex);
	if (_instance == nullptr) { _instance = new CWorkerThreadManager(); }
	return _instance;
}

CWorkerThreadManager::CWorkerThreadManager()
{
	m_ErrorLogger = nullptr;
    m_shuttingDown = false;
}

CWorkerThreadManager::~CWorkerThreadManager()
{
	StopAll();
}

// Set CWorkerErrorLogger to use for error logging
void CWorkerThreadManager::SetErrorLogger(CWorkerErrorLogger* const logger)
{
	m_ErrorLogger = logger;
}

void CWorkerThreadManager::Add(CWorkerThread* const worker)
{
	std::lock_guard<std::mutex> lck(instanceMutex);
	if (m_shuttingDown) { worker->m_WorkItem->Terminate(); }
	m_vWorkers.push_back(worker);
}

// Creates a CWorkerThread with the given CWorkItem
void CWorkerThreadManager::Add(CWorkItem* workItem, bool ownsWorkItem)
{
	// Check that user don't try to add a CWorkItemEx() or derived class
	if (dynamic_cast<CWorkItemEx*>(workItem) == nullptr) {
		throw Invalid_Usage();
	}
	Add(new CWorkerThread(workItem, ownsWorkItem));
}

// Cleanup CWorkerThreads that have exited
void CWorkerThreadManager::CleanUp()
{
	std::lock_guard<std::mutex> lck(instanceMutex);
	int const num = (int)m_vWorkers.size();
    // Loop backwards so we can erase without changing index for not checked ones
    for (int i = (int)m_vWorkers.size() - 1; i >= 0; i--) {
        if (m_vWorkers[i]->IsFinished()) {
            delete m_vWorkers[i];
            m_vWorkers.erase(m_vWorkers.begin() + i);
        }
	}
	if (m_ErrorLogger != nullptr) { 
		m_ErrorLogger->onInformation("CWorkerThreadManager::CleanUp() Items: ", num - (int)m_vWorkers.size()); 
	}
}

void CWorkerThreadManager::StopAll()
{
    m_shuttingDown = true;
    do {
		std::vector<CWorkerThread*> list;
		{
			std::lock_guard<std::mutex> lck(instanceMutex);
			for (uint32_t i = 0; i < m_vWorkers.size(); i++) {
				m_vWorkers[i]->m_WorkItem->Terminate();
			}
			// Now all threads have been asked to terminate, even new ones added after this
			// due to the m_shuttingDown flag.
			// But we need to release the mutex in case some of the threads still running,
			// tries to add things to the list, otherwise we get a deadlock.
			// Copy list so we can delete items outside of mutex lock
			list.assign(m_vWorkers.begin(), m_vWorkers.end());
			m_vWorkers.clear();
		}

		if (m_ErrorLogger != nullptr) {
			m_ErrorLogger->onInformation("CWorkerThreadManager::StopAll() Items: ", (unsigned int)list.size());
		}

        // We have released the mutex, now delete all objects
        for (uint32_t i = 0; i < list.size(); i++) {
            try {
                delete list[i];
            }
            catch (...) {}
        }
		list.clear();
    } while (m_vWorkers.size() > 0);
}

// ---------------------------------------------------------------------

CWorkerThread::CWorkerThread(CWorkItem* const workItem, bool const ownsWorkItem) :
	m_WorkItem(workItem), m_ownsWorkItem(ownsWorkItem),
	m_running(true),
	m_thread(&CWorkerThread::run, this)
{
}

CWorkerThread::~CWorkerThread()
{
	// Signal thread to terminate
	m_WorkItem->Terminate();

	// Wait for thread to finish
	WaitFor();

	// Remove owned objects
	if (m_ownsWorkItem) { delete m_WorkItem; }
}

void CWorkerThread::WaitFor()
{
	// Wait for thread to finish
	if (m_thread.joinable()) { m_thread.join(); }
}

bool CWorkerThread::IsFinished()
{
    // Check if the thread has exited
	return !m_running;
}

// Thread
void CWorkerThread::run()
{
	try {
		m_WorkItem->Run();
	}
#ifdef _WIN32
	catch (utils::SEHException E) {
		if (CWorkerThreadManager::Instance()->m_ErrorLogger != nullptr) {
			CWorkerThreadManager::Instance()->m_ErrorLogger->onError(std::string(E.what()), E.GetExceptionID());
		}
	}
#endif
	catch (...) {
		if (CWorkerThreadManager::Instance()->m_ErrorLogger != nullptr) {
			CWorkerThreadManager::Instance()->m_ErrorLogger->onError("UNKNOWN EXCEPTION RAISED IN WorkItem::Run()", 0);
		}
	}
	m_running = false;
}

// ---------------------------------------------------------------------

CWorkItemEx::CWorkItemEx() : m_WorkerThread(nullptr)
{
}

CWorkItemEx::~CWorkItemEx()
{
	if (m_WorkerThread != nullptr) { delete m_WorkerThread; }
}

void CWorkItemEx::Start()
{
	m_terminateRequested = false;

	// Create a worker who don't own the work item to prevent it from deleting the work item. 
	m_WorkerThread = new CWorkerThread(this, false);
}

void CWorkItemEx::Stop()
{
	TerminateAndWaitFor();
	if (m_WorkerThread != nullptr) { delete m_WorkerThread; }
	m_WorkerThread = nullptr;
}

void CWorkItemEx::WaitFor()
{
	if (m_WorkerThread != nullptr) { m_WorkerThread->WaitFor(); }
}

void CWorkItemEx::TerminateAndWaitFor()
{
	Terminate();
	WaitFor();
}
