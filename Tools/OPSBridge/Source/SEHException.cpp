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

#include "SEHException.h"

#ifdef _WIN32

namespace utils {

_se_translator_function SEHException::oldSEHTranslator_ = _set_se_translator(SEHException::SEHTranslator);

SEHException::SEHException(SEHException const& exceptionCopee)
  : exceptionID_(exceptionCopee.exceptionID_), 
  exceptionPointers_(exceptionCopee.exceptionPointers_)
{
}

SEHException::SEHException(unsigned int exceptionID, EXCEPTION_POINTERS* exceptionPointers)
  : exceptionID_(exceptionID), exceptionPointers_(exceptionPointers)
{
}

SEHException::~SEHException()
{
}

unsigned int SEHException::GetExceptionID() const
{
  return exceptionID_;
}

EXCEPTION_POINTERS* SEHException::GetExceptionPointers() const
{
  return exceptionPointers_;
}

char const * SEHException::what() const
{
  return "Windows SEH exception.";
}

void SEHException::SEHTranslator(unsigned int exceptionID, 
  EXCEPTION_POINTERS* exceptionPointers)
{
  throw SEHException(exceptionID, exceptionPointers);
}

}
#endif
