//    This file is part of dvi2bitmap.
//    Copyright 1999--2002, Council for the Central Laboratory of the Research Councils
//    
//    This program is part of the Starlink Software Distribution: see
//    http://www.starlink.ac.uk 
//
//    dvi2bitmap is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    dvi2bitmap is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with dvi2bitmap; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//    The General Public License is distributed along with this
//    program in the file LICENCE.
//
//    Author: Norman Gray <norman@astro.gla.ac.uk>
//    $Id$


#include <config.h>

#include "InputByteStream.h"

#include <iostream>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>		// for strerror
#ifdef HAVE_CSTD_INCLUDE
#include <cstdio>
#include <cerrno>
#else
#include <stdio.h>
#include <errno.h>
#endif

#ifdef HAVE_STD_NAMESPACE
using std::cerr;
using std::sprintf;
#endif

// Static debug switch
verbosities InputByteStream::verbosity_ = normal;

/**
 * Opens the requested file.  If preload is true, then open the file and
 * read it entire into memory, since the client will be seeking a lot.
 * If the file can't be opened, then try adding <code>tryext</code> to the end of
 * it.  Certain methods below can be called only on files which have
 * been preloaded.
 *
 * @param filename the file to be opened
 * @param preload if true, then the file is read entirely into memory
 * @param tryext a file extension which should be added to the end of
 * <code>filename</code> if that cannot be opened
 */
InputByteStream::InputByteStream (string& filename, bool preload,
                                  string tryext)
    : eof_(true), preloaded_(preload)
{
    fname_ = filename;
    fd_ = open (fname_.c_str(), O_RDONLY);
    if (fd_ < 0 && tryext.length() > 0)
    {
	fname_ += tryext;
	fd_ = open (fname_.c_str(), O_RDONLY);
	if (fd_ >= 0)
            // If this succeeds in opening the file, then modify the filename.
	    filename = fname_;
    }
    if (fd_ < 0)
    {
	string errstr = strerror (errno);
	throw InputByteStreamError ("can\'t open file " + filename
                                    + " to read (" + errstr + ")");
    }

    eof_ = false;

    struct stat S;
    if (fstat (fd_, &S))
    {
	string errstr = strerror (errno);
	throw InputByteStreamError ("Can't stat open file ("
				    +errstr+")");
    }
    filesize_ = S.st_size;

    if (preload)
    {
	buflen_ = filesize_;
	buf_ = new Byte[buflen_];
	size_t bufcontents = read (fd_, buf_, buflen_);
	if (bufcontents != buflen_)
	{
	    string errstr = strerror (errno);
	    throw InputByteStreamError ("Couldn't preload file "+fname_
					+" ("+errstr+")");
	}
	eob_ = buf_ + bufcontents;

	close (fd_);
	fd_ = -1;

	if (verbosity_ > normal)
	    cerr << "InputByteStream: preloaded " << fname_
		 << ", " << filesize_ << " bytes\n";
    }
    else
    {
	buflen_ = 512;
	buf_ = new Byte[buflen_];
	eob_ = buf_;	// nothing read in yet - eob at start

	if (verbosity_ > normal)
	    cerr << "InputByteStream: name=" << fname_
		 << " filesize=" << filesize_
		 << " buflen=" << buflen_ << '\n';
    }
    p_ = buf_;
}

/**
 * Closes the file and reclaims any buffers.
 */
InputByteStream::~InputByteStream ()
{
    if (fd_ >= 0)
	close (fd_);
    delete[] buf_;
}

/**
 * Reads one or more bytes from the file
 *
 * @param n the number of bytes to read
 * @return the byte read
 */
Byte InputByteStream::getByte(int n)
{
    if (eof_)
	return 0;

    if (p_ < buf_)
	throw DviBug ("InputByteStream:"+fname_+": pointer before buffer start");
    if (p_ > eob_)
	throw DviBug ("InputByteStream:"+fname_+": pointer beyond EOF");

    if (p_ == eob_)
    {
	if (preloaded_)		// end of buffer means end of file
	    eof_ = true;
	else
	{
	    read_buf_();
	    p_ = buf_;
	}
    }
    Byte result = eof_ ? static_cast<Byte>(0) : *p_;
    p_ += n;
    return result;
}

/**
 * Reads a block of data from anywhere in the file.
 *
 * @param pos the offset from the beginning of the file from which the
 * block should be read.  If <code>pos</code> is negative, then read the block from
 * an offset <code>-pos</code> from the <em>end</em> of the file.
 * @param length the number of bytes to read
 * @return a pointer to an array of bytes
 */
const Byte *InputByteStream::getBlock (int pos, unsigned int length)
{
    if (eof_)
	return 0;

    Byte *blockp;

    if (pos < 0)		// retrieve last `length' bytes from
				// end of file
    {
	if (preloaded_)
	{
	    blockp = buf_ + buflen_ - length;
	    if (blockp < buf_)
		throw DviBug
		  ("InputByteStream::getBlock:"+fname_+": requested more than bufferful");
	}
	else
	{
	    if (length > buflen_)
		throw InputByteStreamError ("getBlock:"+fname_+": length too large");
	    if (lseek (fd_, filesize_-length, SEEK_SET) < 0)
	    {
		string errstr = strerror (errno);
		throw InputByteStreamError ("getBlock:"+fname_+
					    ": can\'t seek to EOF ("
					    +errstr+")");
	    }
	    read_buf_();
	    blockp = buf_;
	}
    }
    else
    {
	if (preloaded_)
	{
	    blockp = buf_ + pos;
	    if (blockp < buf_)
		throw DviBug
		    ("InputByteStream::getBlock:"+fname_+": pointer before buffer start");
	    if (blockp > eob_)
	    {
		char buf[100];
		sprintf (buf,
		      "InputByteStream::getBlock:%s: pointer beyond EOF (%d,%d)",
			 fname_.c_str(), pos, length);
		throw DviBug (buf);
	    }
	    if (blockp+length > eob_)
	    {
		char buf[100];
		sprintf (buf,
		"InputByteStream::getBlock:%s: requested block too large (%d,%d)",
			 fname_.c_str(), pos,length);
		throw DviBug (buf);
	    }
	}
	else
	    throw DviBug
		("InputByteStream::getBlock:"+fname_+": implemented only for preloaded");
    }

    return blockp;
}

/**
 * Indicate whether we are at the end of the file
 * @return true if we are at the end of the file
 */
bool InputByteStream::eof()
{
    return eof_;
}

/** 
 * Skip to a specified place in the file.
 *
 * @param pos the offset from the beginning of the file where the file
 * pointer should be placed
 * @throws DviBug if argument <code>pos</code> is beyond the end of
 * the file
 */
void InputByteStream::seek (unsigned int pos)
{
    if (preloaded_)
    {
	if (pos > buflen_)	// pos unsigned, so can't be negative
	    throw DviBug ("InputByteStream::seek:"+fname_+": out of range");
	p_ = buf_ + pos;
    }
    else
    {
	if (lseek (fd_, pos, SEEK_SET) < 0)
	{
	    string errstr = strerror(errno);
	    throw DviBug ("InputByteStream::seek:"+fname_+": can\'t seek ("
			  +errstr+")");
	}
	read_buf_();
	p_ = buf_;
    }
}

/**
 * Indicates the position within the file.  This information can be
 * reported only for preloaded files.
 *
 * @return the offset from the beginning of the file
 * @throws DviBug if the file was not preloaded
 */
int InputByteStream::pos ()
{
    if (!preloaded_)
	throw DviBug ("InputByteStream:"+fname_+": Can't get pos in non-preloaded file");
    return static_cast<int>(p_ - buf_);
}

/**
 * Skips a number of bytes forward in the file.  This can be used only
 * for preloaded files.
 *
 * @param skipsize the number of bytes to move forward in the file
 * @throws DviBug if the file was not preloaded
 */
void InputByteStream::skip (unsigned int skipsize)
{
    if (!preloaded_)
	throw DviBug ("InputByteStream:"+fname_+": Can't skip in non-preloaded file");
    p_ += skipsize;
}

void InputByteStream::read_buf_ ()
{
    ssize_t bufcontents = read (fd_, buf_, buflen_);
    if (bufcontents < 0)
    {
	string errmsg = strerror(errno);
	throw DviBug ("InputByteStream::read_buf_:"+fname_+": read error ("
		      +errmsg+")");
    }
    eof_ = (bufcontents == 0);
    eob_ = buf_ + bufcontents;
}
/*
#if (sizeof(unsigned int) != 4)
// The code here is intended to deal with the case where (un)signed
// ints are 4 bytes long.  It's actually simpler on machines where
// ints are longer, because we wouldn't have to do the two-stage
// subtraction (below) when n==4.  I can't test this, however, so
// simply cause an error here.
#error "InputByteStream.cc assumes sizeof(unsigned int)==4"
#endif
*/

// Assumes: DVI file is big-endian - ie, MSB first (I can't find this
// explicitly stated in the spec); DVI integers are 2-complement
// (explicit in the spec).
// I _believe_ that the following code is independent of the host
// machine's bytesex and integer representation (2's-complement or
// not), but....

/**
 * Obtains an n-byte signed integer from the stream, as a signed
 * int.
 *
 * @param n the number of bytes to read, in the range 1--4 inclusive
 * @return the next integer from the input stream, as a signed int
 * @throws DviBug if parameter <code>n</code> was out of range
 */
signed int InputByteStream::getSIS(int n)
{
    if (n<0 || n>4)
	throw DviBug("InputByteStream:"+fname_+": bad argument to getSIS");
    unsigned int t = getByte();
    unsigned int pow2 = 128;	// 2^7-1   is largest one-byte signed int
				// 2^7=128 is most negative signed int
				// 2^8-1   is -1
    for (int i=n-1; i>0; i--)
    {
	pow2 *= 256;
	t *= 256;
	t += getByte();
    }
    signed int result;
    if (t < pow2)
	result = t;
    else
	// pow2 <= t < 2*pow2
	// t is actually a negative number - subtract off 2*pow2
	if (n < 4)
	{
	    // t and 2*pow2 will both fit into a signed int
	    // so explicitly convert them to that _first_.
	    // I'm not sure if the result of subtracting an unsigned from 
	    // a smaller unsigned (ie, whether it's a signed negative number,
	    // or, say, zero) is implementation-dependent or not.
	    result = static_cast<signed int>(t)
		- static_cast<signed int>(2*pow2);
	    //result -= 2*pow2;
	}
	else
	{
	    // n==4: t won't fit into a signed int, and 2*pow2 would
	    // overflow, so do the subtraction in two stages.  A
	    // careless optimiser could mess this up.
	    t -= pow2;		// now it'll fit
	    // 0 <= t < pow2
	    result = pow2 - t;	// a positive result in a signed variable
	    result = -result;
	}
    return result;
}

/**
 * Obtains an n-byte unsigned integer from the stream, 
 * as a signed int.
 *
 * @param n the number of bytes to read, in the range 1--3 inclusive
 * (there are no 4-byte unsigned quantities in DVI files)
 * @return the next integer from the input stream, as a signed int
 * @throws DviBug if parameter <code>n</code> was out of range
 */
signed int InputByteStream::getSIU(int n)
{
    // disallow n==4 - there are no unsigned 4-byte quantities in the DVI file
    if (n<0 || n>3)
	throw DviBug("InputByteStream:"+fname_+": bad argument to getSIU");
    unsigned int t = 0;
    for (; n>0; n--)
    {
	t *= 256;
	t += getByte();
    }
    return static_cast<signed int>(t);
}

/**
 * Obtains an n-byte unsigned integer from the stream, as an
 * unsigned int
 *
 * @param n the number of bytes to read, in the range 1--4 inclusive
 * @return the next integer from the input stream, as an unsigned int
 * @throws DviBug if parameter <code>n</code> was out of range
 */
unsigned int InputByteStream::getUIU(int n)
{
    if (n<0 || n>4)
	throw DviBug("InputByteStream:"+fname_+": bad argument to getUIU");
    unsigned int t = 0;
    for (; n>0; n--)
    {
	t *= 256;
	t += getByte();
    }
    return t;
}

/**
 * Obtains an n-byte unsigned integer from the beginning of a
 * <code>Byte</code> array, as an unsigned int.  This has little
 * specifically to do with Input streams, and is here as a convenience
 * method.
 *
 * @param n the number of bytes to read, in the range 1--4 inclusive
 * @param p a pointer to an array of <code>Byte</code> values
 * @return the integer at the beginning of the given array, as an unsigned int
 * @throws DviBug if parameter <code>n</code> was out of range
 */
unsigned int InputByteStream::getUIU(int n, const Byte *p)
{
    if (n<0 || n>4)
	throw DviBug("InputByteStream: bad argument to getUIU(int,Byte*)");
    unsigned int t = 0;
    for (const Byte *b=p; n>0; n--, b++)
	t = t*256 + static_cast<unsigned int>(*b);
    return t;
}

