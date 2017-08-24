/**
*
* Copyright (C) 2017 Lennart Andersson.
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

#pragma once

#include <string.h>
#ifndef FIXED_NO_STD_STRING
	#include <string>
#endif
#include <exception>

namespace ops {

	template <size_t N>
	class fixed_string
	{
	private:
		char _array[N + 1];
		size_t _size;

	public:
		typedef size_t size_type;

		// Exceptions:
		struct index_out_of_range : public std::exception {
			const char* what() const noexcept { return "Index too large"; }
		};
		struct size_out_of_range : public std::exception {
			const char* what() const noexcept { return "String too large"; }
		};

		// Constructors:
		fixed_string() : _size(0) { _array[0] = '\0'; }
		fixed_string(const char* s) : _size(0) { append(s, strlen(s)); }
		fixed_string(const char* s, size_t len) : _size(0) { size_t sz = strlen(s); append(s, (sz < len) ? sz : len); }
#ifndef FIXED_NO_STD_STRING
		fixed_string(const std::string s) : _size(0) { append(s.c_str(), s.size()); }
#endif

		template<size_t M>
		fixed_string(const fixed_string<M>& str) : _size(0) { append(str.c_str(), str.size()); }

		// all the special members can be defaulted
		fixed_string(fixed_string const&) = default;
		fixed_string(fixed_string&&) = default;
		fixed_string& operator=(fixed_string const&) = default;
		fixed_string& operator=(fixed_string&&) = default;
		~fixed_string() = default;

		// Iterators:
		// ...

		// Capacity:
		size_t size() const noexcept { return _size; }
		size_t length() const noexcept { return _size; }
		size_t max_size() const noexcept { return N; }
		void resize() 
		{
			_array[N] = '\0';	// make sure the array is null terminated
			_size = strlen(&_array[0]); 
		}
		void resize(size_t n) { resize(n, '\0'); }
		void resize(size_t n, char c) 
		{
			if (n > N) throw size_out_of_range();
			for (size_t i = _size; i < n; ++i) _array[i] = c;
			_size = n; 
			_array[_size] = '\0';
		}

		void clear() noexcept { _array[0] = '\0'; _size = 0; }
		bool empty() const noexcept { return _size == 0; }

		// Element access:
		char& operator[] (size_t pos)
		{
			// if pos is equal to size, return ref to null char (according to standard for std::string)
			if (pos > _size) throw index_out_of_range();
			return _array[pos];
		}
		char& at(size_t pos)
		{
			if (pos >= _size) throw index_out_of_range();
			return _array[pos];
		}

		// Modifiers:
		fixed_string& operator+= (const fixed_string& str) { return append(str.c_str(), str.size()); }
#ifndef FIXED_NO_STD_STRING
		fixed_string& operator+= (const std::string& str) { return append(str.c_str(), str.size()); }
#endif
		fixed_string& operator+= (const char* s) { return append(s, strlen(s)); }
		fixed_string& operator+= (char c)
		{
			if (_size == N) throw size_out_of_range();
			_array[_size] = c;
			_size++;
			_array[_size] = '\0';
			return *this;
		}

		fixed_string& append(const fixed_string& str) { return append(str.c_str(), str.size()); }
		fixed_string& append(const char* s) { return append(s, strlen(s)); }
		fixed_string& append(const char* s, size_t len)
		{
			if (len > 0) {
				if ((_size + len) > N) throw size_out_of_range();
				memcpy(&_array[_size], s, len);
				_size += len;
			}
			_array[_size] = '\0';	// Needed if if len == 0, since it can be from constructors with null string
			return *this;
		}

		template<size_t M>
		fixed_string& operator+= (const fixed_string<M>& str) { return append(str.c_str(), str.size()); }

		template<size_t M>
		fixed_string& append(const fixed_string<M>& str) { return append(str.c_str(), str.size()); }

#ifndef FIXED_NO_STD_STRING
		// Implicit conversion operator
		operator std::string() const { return std::string(_array); }
#endif

		// String operations
		const char* data() const noexcept { return &_array[0]; }
		const char* c_str() const noexcept { return &_array[0]; }

		size_t find(const fixed_string& str, size_t pos = 0) const 
		{
			return find(str.c_str(), pos);
		}
		size_t find(const char* s, size_t pos = 0) const 
		{
			if (pos > _size) throw index_out_of_range();
			const char* ptr = strstr(&_array[pos], s);
			if (ptr == nullptr) return npos;
			return (size_t)(ptr - &_array[0]);
		}
		size_t find(char c, size_t pos = 0) const 
		{
			if (pos > _size) throw index_out_of_range();
			const char* ptr = strchr(&_array[pos], c);
			if (ptr == nullptr) return npos;
			return (size_t)(ptr - &_array[0]);
		}

#ifndef FIXED_NO_STD_STRING
		std::string substr(size_t pos = 0, size_t len = npos) const
		{
			if (pos > _size) throw index_out_of_range();
			size_t avail = _size - pos;
			if (len > avail) len = avail;
			return std::string(&_array[pos], len);
		}
#else
		fixed_string substr(size_t pos = 0, size_t len = npos) const
		{
			if (pos > _size) throw index_out_of_range();
			size_t avail = _size - pos;
			if (len > avail) len = avail;
			return fixed_string(&_array[pos], len);
		}
#endif

		template<size_t M>
		fixed_string<M> substr(size_t pos = 0, size_t len = npos) const
		{
			if (pos >= _size) throw index_out_of_range();
			return fixed_string<M>(&_array[pos], len);
		}

		static const size_t npos = (size_t)(-1);
	};

	// Relational operators
	template<size_t M, size_t N>
	bool operator== (const fixed_string<M>& lhs, const fixed_string<N>& rhs) { return strcmp(lhs.c_str(), rhs.c_str()) == 0; }
	template<size_t M>
	bool operator== (const char*   lhs, const fixed_string<M>& rhs) { return strcmp(lhs, rhs.c_str()) == 0; }
	template<size_t M>
	bool operator== (const fixed_string<M>& lhs, const char*   rhs) { return strcmp(lhs.c_str(), rhs) == 0; }

	template<size_t M, size_t N>
	bool operator!= (const fixed_string<M>& lhs, const fixed_string<N>& rhs) { return strcmp(lhs.c_str(), rhs.c_str()) != 0; }
	template<size_t M>
	bool operator!= (const char*   lhs, const fixed_string<M>& rhs) { return strcmp(lhs, rhs.c_str()) != 0; }
	template<size_t M>
	bool operator!= (const fixed_string<M>& lhs, const char*   rhs) { return strcmp(lhs.c_str(), rhs) != 0; }

	template <size_t M, size_t N>
	bool operator<  (const fixed_string<M>& lhs, const fixed_string<N>& rhs) { return strcmp(lhs.c_str(), rhs.c_str()) < 0; }
	template <size_t M>
	bool operator<  (const char*   lhs, const fixed_string<M>& rhs) { return strcmp(lhs, rhs.c_str()) < 0; }
	template <size_t M>
	bool operator<  (const fixed_string<M>& lhs, const char*   rhs) { return strcmp(lhs.c_str(), rhs) < 0; }

	template <size_t M, size_t N>
	bool operator<=  (const fixed_string<M>& lhs, const fixed_string<N>& rhs) { return strcmp(lhs.c_str(), rhs.c_str()) <= 0; }
	template <size_t M>
	bool operator<=  (const char*   lhs, const fixed_string<M>& rhs) { return strcmp(lhs, rhs.c_str()) <= 0; }
	template <size_t M>
	bool operator<=  (const fixed_string<M>& lhs, const char*   rhs) { return strcmp(lhs.c_str(), rhs) <= 0; }

	template <size_t M, size_t N>
	bool operator>  (const fixed_string<M>& lhs, const fixed_string<N>& rhs) { return strcmp(lhs.c_str(), rhs.c_str()) > 0; }
	template <size_t M>
	bool operator>  (const char*   lhs, const fixed_string<M>& rhs) { return strcmp(lhs, rhs.c_str()) > 0; }
	template <size_t M>
	bool operator>  (const fixed_string<M>& lhs, const char*   rhs) { return strcmp(lhs.c_str(), rhs) > 0; }

	template <size_t M, size_t N>
	bool operator>=  (const fixed_string<M>& lhs, const fixed_string<N>& rhs) { return strcmp(lhs.c_str(), rhs.c_str()) >= 0; }
	template <size_t M>
	bool operator>=  (const char*   lhs, const fixed_string<M>& rhs) { return strcmp(lhs, rhs.c_str()) >= 0; }
	template <size_t M>
	bool operator>=  (const fixed_string<M>& lhs, const char*   rhs) { return strcmp(lhs.c_str(), rhs) >= 0; }

	// operator +
	template <size_t M, size_t N>
	fixed_string<M + N> operator+ (const fixed_string<M>& lhs, const fixed_string<N>& rhs)
	{
		fixed_string<M + N> res;
		res += lhs;
		res += rhs;
		return res;
	}

#ifndef FIXED_NO_STD_STRING
	template <size_t M>
	std::string operator+ (const std::string& lhs, const fixed_string<M>& rhs) { return lhs + rhs.substr(); }
	template <size_t M>
	std::string operator+ (const fixed_string<M>& lhs, const std::string& rhs) { return lhs.substr() + rhs; }
#endif

} //namespace
