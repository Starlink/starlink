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
using std::endl;
#endif

// Static debug switch
verbosities InputByteStream::verbosity_ = normal;
unsigned int InputByteStream::buffer_length_ = 512;

/**
 * Opens the requested file.  If preload is true, then open the file
 * and read it entire into memory, since the client will be seeking a
 * lot.  If the file can't be opened, then try adding
 * <code>tryext</code> to the end of it.  Certain methods below can be
 * called only on files which have been preloaded.
 *
 * @param filename the file to be opened
 * @param preload if true, then the file is read entirely into memory
 * @param tryext a file extension which should be added to the end of
 * <code>filename</code> if that cannot be opened
 */
InputByteStream::InputByteStream (string& filename, bool preload,
                                  string tryext)
    : eof_(true), preloaded_(preload), seekable_(true)
{
    //fname_ = filename;
    cerr << "InputByteStream(" << filename << "," << preload << ","
	 << tryext << ")" << endl;
    int newfd = open (filename.c_str(), O_RDONLY);
    if (newfd < 0 && tryext.length() > 0)
    {
	string tfilename = filename + tryext;
	newfd = open (tfilename.c_str(), O_RDONLY);
	if (newfd >= 0)
            // If this succeeds in opening the file, then modify the filename.
	    filename = tfilename;
    }
    if (newfd < 0)
    {
	string errstr = strerror (errno);
	throw InputByteStreamError ("can\'t open file " + filename
                                    + " to read (" + errstr + ")");
    }

    struct stat S;
    if (fstat (newfd, &S))
    {
	string errstr = strerror (errno);
	throw InputByteStreamError ("Can't stat open file (" + errstr + ")");
    }
    filesize_ = S.st_size;

    if (preload)
    {
	buflen_ = filesize_;
	buf_ = new Byte[buflen_];
	size_t bufcontents = read(newfd, buf_, buflen_);
	if (bufcontents != buflen_)
	{
	    string errstr = strerror (errno);
	    throw InputByteStreamError ("Couldn't preload file "+filename
					+" ("+errstr+")");
	}
	eob_ = buf_ + bufcontents;
	p_ = buf_;
	eof_ = false;

	close (newfd);
	fd_ = -1;

	if (verbosity_ > normal)
	    cerr << "InputByteStream: preloaded " << fname_
		 << ", " << filesize_ << " bytes" << endl;
    } else {
	bindToFileDescriptor(newfd);
    }
    fname_ = filename;
}

/**
 * Prepare to read from the requested file descriptor.  The
 * descriptor must be open, and can refer to a non-seekable file such
 * as a pipe.
 *
 * @param fileno the file descriptor to read from
 */
InputByteStream::InputByteStream(int fileno)
    : eof_(true), preloaded_(false), seekable_(false)
{
    bindToFileDescriptor(fileno);
}

bool InputByteStream::bindToFileDescriptor(int fileno)
{
    fd_ = fileno;
    char filenamebuf[20]; // just in case fileno is a _large_ number...
    sprintf(filenamebuf, "fd:%d", fileno);
    fname_ = filenamebuf;

    eof_ = false;

    buflen_ = buffer_length_;
    buf_ = new Byte[buflen_];
    p_ = eob_ = buf_;		// nothing read in yet -- eob at start
    if (verbosity_ > normal)
	cerr << "InputByteStream: reading from fd " << fileno
	     << ", name=" << fname_
	     << endl;

    return true;
}


/**
 * Sets the buffer size to be used for reading files.
 *
 * @param length the size, in bytes, of the input buffer
 */
void InputByteStream::setBufferSize(unsigned int length)
{
    buffer_length_ = length;
}


/**
 * Closes the file and reclaims any buffers.
 */
InputByteStream::~InputByteStream ()
{
    if (fd_ >= 0)
	close (fd_);
    if (buf_ != 0)
	delete[] buf_;
    buf_ = 0;
}

/**
 * Reads a byte from the stream.  This method does not signal
 * an error at end-of-file; use the {@ #eof} method to detect if the
 * stream is at an end.
 *
 * @return the byte read, or zero if we are at the end of the file
 * @throws DviBug if an internal inconsistency is found
 */
Byte InputByteStream::getByte(void)
    throw (DviBug)
{
    if (eof_)
	return 0;

    if (p_ < buf_)
	throw DviBug ("InputByteStream:" + fname_
		      + ": pointer before buffer start");
    if (p_ > eob_)
	throw DviBug ("InputByteStream:" + fname_
		      + ": pointer beyond EOF");

    if (p_ == eob_)
    {
	if (preloaded_)		// end of buffer means end of file
	    eof_ = true;
	else
	    read_buf_();
    }
    Byte result = eof_ ? static_cast<Byte>(0) : *p_;
    ++p_;
    return result;
}

/**
 * Reads a block of data from anywhere in the file.  The stream
 * pointer is placed at the beginning of this block.
 *
 * @param pos the offset from the beginning of the file from which the
 * block should be read.  If <code>pos</code> is negative, then read
 * the block from an offset <code>-pos</code> from the <em>end</em> of
 * the file.
 *
 * @param length the number of bytes to read, which must be no greater
 * than the buffer size of the file, as obtained from {@link #getBufferSize}
 *
 * @return a pointer to an array of bytes, or a null pointer if we
 * are at the end of the file
 *
 * @throws InputByteStreamError if the stream is not seekable, if
 * <code>|pos|</code> is larger than the size of the file, or if
 * <code>length</code> is greater than the buffer size of the file
 */
const Byte *InputByteStream::getBlock (int pos, unsigned int length)
    throw (InputByteStreamError)
{
    if (eof_)
	return 0;

    if (!seekable_)
 	throw InputByteStreamError("InputByteStream::getblock: stream "
				   + fname_ + " is not seekable");

    if (length > buffer_length_) {
	char errbuf[100];
	sprintf(errbuf,
		"InputByteStream::getblock: length %d is greater than the stream buffer size, %d",
		length, buffer_length_);
 	throw InputByteStreamError(errbuf);
    }

    seek(pos);
    Byte *blockp;
    if (preloaded_)
	blockp = p_;
    else {
	read_buf_();
	blockp = p_;
    }

    return blockp;
}

/** 
 * Skip to a specified place in the file.
 *
 * @param pos the offset from the beginning of the file from which the
 * block should be read.  If <code>pos</code> is negative, then read
 * the block from an offset <code>-pos</code> from the <em>end</em> of
 * the file.
 *
 * @throws InputByteStreamError if argument <code>|pos|</code> is
 * larger than the size of the file
 */
void InputByteStream::seek (int pos)
    throw (InputByteStreamError)
{
    if (!seekable_)
	throw InputByteStreamError("getblock: stream " + fname_
				   + " is not seekable");

    if (pos > 0) {
	if (pos > filesize_) 
	    throw InputByteStreamError
		    ("InputByteStream::seek:"+fname_+": out of range");
    } else {
	if (-pos > filesize_)
	    throw InputByteStreamError
		    ("InputByteStream::seek:"+fname_+": out of range");
    }

    if (preloaded_) {
	if (pos > 0) {
	    p_ = buf_ + pos;
	} else {
	    p_ = buf_ + buflen_ + pos;
	}
	if (verbosity_ > normal)
	    cerr << "seek(" << pos << "),preloaded: p_-buf_=" << p_-buf_
		 << endl;
    } else {
	off_t off;
	
	if (pos >= 0)
	    off = pos;
	else
	    off = filesize_ + pos;
	
	if (lseek (fd_, off, SEEK_SET) < 0) {
	    string errstr = strerror(errno);
	    throw DviBug ("InputByteStream::seek:"+fname_+": can\'t seek ("
			  +errstr+")");
	}
	if (verbosity_ > normal)
	    cerr << "seek(" << pos << "): off=" << off << endl;
	p_ = eob_;
    }
}

/**
 * Skips a number of bytes forward in the file.  This can be used only
 * for preloaded files.
 *
 * @param skipsize the number of bytes to move forward in the file
 * @throws InputByteStreamError if the file was not preloaded
 */
void InputByteStream::skip (unsigned int skipsize)
    throw (InputByteStreamError)
{
    Byte *tp = p_ + skipsize;
    if (preloaded_) {
	if (tp > eob_)
	    throw InputByteStreamError ("File " + fname_
					+ ": skip past end of file");
	p_ = tp;
    } else {
	if (tp <= eob_)
	    p_ = tp;
	else {
	    off_t newpos = lseek(fd_, skipsize, SEEK_CUR);
	    if (newpos < 0) {
		string errstr = strerror(errno);
		throw InputByteStreamError
			("File " + fname_
			 + ": error seeking (" + errstr + ")");
	    } else if (newpos > filesize_) {
		throw InputByteStreamError
			("File " + fname_
			 + ": seek past end of file");
	    }
	    p_ = eob_;		// prompts re-reading
	}
    }
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
 * Indicates the position within the file.  This information can be
 * reported only for preloaded files.
 *
 * @return the offset from the beginning of the file
 * @throws InputByteStreamError if the file was not preloaded
 */
int InputByteStream::pos ()
    throw (InputByteStreamError)
{
    if (!preloaded_)
	throw InputByteStreamError
		("InputByteStream:" + fname_
		 + ": Can't get pos in non-preloaded file");
    return static_cast<int>(p_ - buf_);
}

void InputByteStream::read_buf_ ()
    throw (InputByteStreamError)
{
    ssize_t bufcontents = read(fd_, buf_, buflen_);
    if (bufcontents < 0)
    {
	string errmsg = strerror(errno);
	throw InputByteStreamError
		("InputByteStream::read_buf_:"+fname_+": read error ("
		 +errmsg+")");
    }
    eof_ = (bufcontents == 0);
    eob_ = buf_ + bufcontents;
    if (verbosity_ > normal)
	cerr << "InputByteStream::read_buf_: read "
	     << bufcontents << '/' << buflen_ << " from fd " << fd_
	     << "; eof=" << eof_
	     << endl;
    p_ = buf_;
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
 * @throws InputByteStreamError if parameter <code>n</code> was out of range
 * @throws DviBug if an internal inconsistency is found
 */
signed int InputByteStream::getSIS(int n)
    throw (InputByteStreamError,DviBug)
{
    if (n<0 || n>4)
	throw InputByteStreamError
		("InputByteStream:"+fname_+": bad argument to getSIS");
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
 * @throws InputByteStreamError if parameter <code>n</code> was out of range
 * @throws DviBug if an internal inconsistency is found
 */
signed int InputByteStream::getSIU(int n)
    throw (InputByteStreamError,DviBug)
{
    // disallow n==4 - there are no unsigned 4-byte quantities in the DVI file
    if (n<0 || n>3)
	throw InputByteStreamError
		("InputByteStream:"+fname_+": bad argument to getSIU");
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
 * @throws InputByteStreamError if parameter <code>n</code> was out of range
 * @throws DviBug if an internal inconsistency is found
 */
unsigned int InputByteStream::getUIU(int n)
    throw (InputByteStreamError,DviBug)
{
    if (n<0 || n>4)
	throw InputByteStreamError
		("InputByteStream:"+fname_+": bad argument to getUIU");
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
 * @throws InputByteStreamError if parameter <code>n</code> was out of range
 * @throws DviBug if an internal inconsistency is found
 */
unsigned int InputByteStream::getUIU(int n, const Byte *p)
    throw (InputByteStreamError,DviBug)
{
    if (n<0 || n>4)
	throw InputByteStreamError
		("InputByteStream: bad argument to getUIU(int,Byte*)");
    unsigned int t = 0;
    for (const Byte *b=p; n>0; n--, b++)
	t = t*256 + static_cast<unsigned int>(*b);
    return t;
}

