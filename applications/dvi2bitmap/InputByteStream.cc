//    This file is part of dvi2bitmap.
//    Copyright 1999--2004, Council for the Central Laboratory of the Research Councils
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

#include <InputByteStream.h>

#include <iostream>
#include <fcntl.h>

#ifdef HAVE_CSTD_INCLUDE
#  include <cstdio>
#  include <cstdlib>
#  include <cassert>
#  include <cerrno>
#else
#  include <stdio.h>
#  include <stdlib.h>
#  include <assert.h>
#  include <errno.h>
#endif

#if HAVE_SYS_ERRNO_H
/* If it's available, explicitly include sys/errno.h as well as
 * <cerrno> or <errno.h> above.  If we're compiling in a strict-ansi
 * mode, the compiler may well have carefully avoided defining errors
 * which are specific to Unix/POSIX, which are, of course, precisely
 * the ones we're hoping to use.
 */
#  include <sys/errno.h>
#endif

#include <unistd.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>		// for stat
#endif

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

using STD::cerr;		// these functions are used many times
using STD::sprintf;
using STD::endl;
using STD::strerror;

// Static debug switch
verbosities InputByteStream::verbosity_ = normal;
unsigned int InputByteStream::default_buffer_length_ = 0;

/**
 * No-argument constructor creates a new InputByteStream object, but
 * does not associate it with any source of bytes.  To associate it
 * with a source, use {@link #bindToFileDescriptor} or the
 * convenience method {@link #openSourceSpec}.
 */
InputByteStream::InputByteStream()
    : eof_(true), fd_(-1), mappedfd_(-1), buf_(0)
{
    // empty
}

/**
 * Prepares to read a stream from the specified file descriptor, which
 * must be open.
 *
 * @param fd an open file descriptor
 * @throws InputByteStreamError if there is a problem binding to the
 * descriptor
 */
InputByteStream::InputByteStream(int fd)
    throw (InputByteStreamError)
    : eof_(true), fd_(-1), mappedfd_(-1), buf_(0)
{
    bindToFileDescriptor(fd, "", 0, false, false);
}

/**
 * Prepares to read a stream from the specified source.
 *
 * <p>The source may be
 * <ul>
 * <li>a file name, which should be readable
 * <li>a specifier of the form <code>&lt;osfile&gt;filename</code>:
 * the specified file is opened
 * <li>a specifier of the form <code>&lt;osfd&gt;integer</code>: the integer
 * specifyies an (open) OS file descriptor; thus
 * <code>&lt;osfd&gt;0</code> opens the standard input
 * </ul>
 *
 * @param srcspec a source specification as described above
 * @throws InputByteStreamError if there is a problem opening or
 * binding to the descriptor
 */
InputByteStream::InputByteStream(string srcspec)
    throw (InputByteStreamError)
    : eof_(true), fd_(-1), mappedfd_(-1), buf_(0)
{
    int fd = openSourceSpec(srcspec);
    // Note that, since we don't know that the srcspec isn't a fd or
    // a pipe, we cannot say that it's definitely seekable
    bindToFileDescriptor(fd, srcspec, 0, false, false);
}

/**
 * Closes the file and reclaims any buffers.
 */
InputByteStream::~InputByteStream ()
{
    close();
}

/**
 * Indicates whether we are at the end of the file.  This method does
 * not return true until <em>after</em> a failed attempt to read past
 * the end of file; that is, it does not return true immediately
 * after the last byte has been read from the file.
 *
 * @return true if we are at the end of the file
 */
bool InputByteStream::eof()
{
    return eof_;
}

/**
 * Binds this stream to a given file descriptor.
 *
 * <p>If the parameter <code>fillBufferAndClose</code> is true, then
 * this method will keep reading from the file descriptor until it
 * reaches either the end of the newly-allocated buffer, or
 * end-of-file, whichever comes first.  It will then close the file
 * descriptor.  In this case (and in this case alone), the method
 * {@link #bufferSeek} becomes useful, and can be used by an extending
 * class to implement an efficient <code>seek</code> operation on the file.
 *
 * @param fileno the file descriptor to be handled by this object
 * @param filename the file name associated with this descriptor; this may be
 * the empty string
 * @param bufsize a size suggested for the input buffer, or zero to
 * accept the default
 * @param fillBufferAndClose if true, read the entire contents of the
 * file into memory
 * @param assertIsSeekable if true, the caller is asserting that the
 * given file descriptor points to a seekable object (presumably because
 * it will depend on it being so).  If this is not the case (the
 * descriptor refers to a pipe or other non-seekable, and
 * non-mappable, object), then throw an exception.  If this is false,
 * no assertion is being made; it is not being asserted that the
 * object is non-seekable.
 *
 * @throws InputByteStreamError if there is some other problem reading
 * the file; or if a negative buffer size is given; or if the
 * assertion described for parameter <code>assertIsSeekable</code>
 * turns out to be false.
 * 
 * @return true on success
 */
bool InputByteStream::bindToFileDescriptor(int fileno,
					   string filename,
					   int bufsize,
					   bool fillBufferAndClose,
					   bool assertIsSeekable)
    throw (InputByteStreamError)
{
    fd_ = fileno;
    if (filename.length() == 0) {
	char filenamebuf[20]; // just in case fileno is a _large_ number...
	sprintf(filenamebuf, "fd:%d", fileno);
	fname_ = filenamebuf;
    } else {
	fname_ = filename;
    }

    eof_ = false;

    if (bufsize < 0)
	throw InputByteStreamError("InputByteStream: negative bufsize");

    buflen_ = 0;
    if (buflen_ == 0 && bufsize != 0)
	buflen_ = bufsize;
    if (buflen_ == 0 && default_buffer_length_ != 0)
	buflen_ = default_buffer_length_;

    bool isSeekable;
#ifdef HAVE_SYS_STAT_H
    struct stat S;
    if (fstat(fd_, &S) == 0) {
	if (verbosity_ > normal)
	    cerr << "File descriptor " << fd_ << " OK:"
		 << " st_rdev=" << S.st_rdev
		 << " st_mode=" << S.st_mode
		 << " pipe=" << (S_ISFIFO(S.st_mode) ? "yes" : "no")
		 << " st_size=" << S.st_size
		 << " st_blksize=" << S.st_blksize
		 << endl;
	isSeekable = S_ISREG(S.st_mode);
	// This mode can't be S_ISLNK, since this wasn't lstat().
	// The mode could be a FIFO.  Anything else is probably a
	// user error, which will be caught with normal processing in
	// the caller.
	if (buflen_ == 0)
	    buflen_ = S.st_blksize;
    } else {
	string errmsg = strerror(errno);
	throw InputByteStreamError("File descriptor not open: " + errmsg);
    }
#else
    isSeekable = false;		// we don't know it isn't; but we
				// can't be sure it is, so be safe
#endif	/* HAVE_SYS_STAT_H */

    if (buflen_ == 0)
	buflen_ = 1024;

    if (assertIsSeekable && !isSeekable)
	throw InputByteStreamError
		("File " + fname_ + " is not seekable, contrary to assertion");

#ifdef HAVE_MMAP
    if (isSeekable) {

#  if !defined(HAVE_SYS_STAT_H)
	// we shouldn't have got here!
	assert(false);
#  endif  /* !defined(HAVE_SYS_STAT_H) */

	buflen_ = S.st_size;
        errno = 0;
	buf_ = reinterpret_cast<Byte*>(mmap(0, buflen_,
                                       PROT_READ, MAP_SHARED,
                                       fd_, 0));
#  if defined(MAP_FAILED)
	if (buf_ == static_cast<Byte*>(MAP_FAILED))
#  else
        // Some systems have mman.h, but don't declare the MAP_FAILED
        // macro variable, though it's mandated by POSIX.  In this case,
        // just rely on errno being set
        if (errno)
#  endif
        {
	    string errmsg = strerror(errno);
	    throw InputByteStreamError
		    ("Failed to map file " + fname_ + " (" + errmsg + ")");
	}
	// Don't ::close(fd_) here: though we don't need it any more,
	// leaving it open means that the file won't be reclaimed if
	// it's unlinked while we're mapping it (that would be Bad).
	// However, we do want fd_ to be negative, as a flag that the
	// fd is now inaccessible, both in this class and, via
	// getFD(), to extending classes.  So save the mapped fd, so
	// ::close(mappedfd_) can be closed explicitly within
	// InputByteStream::close().
	//
	// Now, according to the single-unix spec: ``The mmap() function
	// adds an extra reference to the file associated with the
	// file descriptor fildes which is not removed by a subsequent
	// close() on that file descriptor.  This reference is removed
	// when there are no more mappings to the file.''  However the
	// OS X man page (for example) makes no such guarantee.
	// Therefore don't close it now, postponing the close until
	// we munmap the file.  This may be belt-and-braces, but it
	// won't create a problem.
	mappedfd_ = fd_;
	fd_ = -1;

	p_ = buf_;
	eob_ = buf_ + buflen_;
	
    }
    else
#endif  /* HAVE_MMAP */ 
    {
        assert(buflen_ > 0);
	buf_ = new Byte[buflen_];
	if (fillBufferAndClose) {
	    size_t real_length = certainly_read_(fd_, buf_, buflen_);
	    p_ = buf_;
	    eob_ = buf_ + real_length;
            // Do not use close() here, since that also deallocates buf_,
            // and sets eof_ true.
            ::close(fd_);
            fd_ = -1;
            if (verbosity_ >= debug) {
                cerr << "InputByteStream: read "
                     << real_length << '/' << buflen_
                     << " from fd: buf_ is non zero? "
                     << (buf_ != 0 ? "yes" : "no")
                     << "; actual length=" << (eob_-buf_)
                     << ".  Closed: fd=" << fd_
                     << endl;
            }
	} else {
	    eob_ = p_ = buf_;
	    assert(p_ == buf_);
	}
    }

    assert(buf_ <= p_ && p_ <= eob_);
    // Since from the client's point of view we haven't read anything yet,
    // we can't be at EOF.
    assert(!eof_);
    
    if (verbosity_ >= debug)
	cerr << "InputByteStream: reading from fd " << fileno
	     << ", name=" << fname_
	     << ", buffer length=" << buflen_
	     << ", seekable=" << (isSeekable ? "yes" : "no")
	     << ", mapped=" << (mappedfd_ >= 0 ? "yes" : "no")
             << (fd_ < 0 ? " (read completed and fd closed)" : "")
	     << endl;

    return true;
}


/**
 * Opens a source.  The source is specified as in {@link
 * #InputByteStream(string)}.  Throws an exception on any problems,
 * so that if it returns, it has successfully opened the file, or determined
 * that the file descriptor is (syntactically) valid.
 *
 * @param srcspec a source specification
 * @return an open file descriptor
 * @throws InputByteStreamError if there is any problem opening the file
 */    
int InputByteStream::openSourceSpec(string srcspec)
    throw (InputByteStreamError)
{
    int fd = -1;
    string srcfn;
    if (srcspec.substr(0,8).compare("<osfile>") == 0) {
        srcfn = srcspec.substr(8);
    } else if (srcspec.substr(0,6).compare("<osfd>") == 0) {
	string fdstr = srcspec.substr(6);
	errno = 0;
	fd = STD::strtol(fdstr.c_str(), 0, 10);
	if (errno != 0) {
	    string errmsg = "InputByteStream: Invalid source fd:";
	    errmsg += fdstr;
	    errmsg += " (";
	    errmsg += strerror(errno);
	    errmsg += ")";
	    throw InputByteStreamError(errmsg);
	}
	if (fd < 0) {
	    throw InputByteStreamError
		    ("InputByteStream: negative source fd");
	}
    } else {
	// simple file name
	srcfn = srcspec;
    }
    
    if (fd < 0 && srcfn.size() == 0)
	throw InputByteStreamError("InputByteStream: no source spec!");

    assert(!(fd >= 0 && srcfn.size() > 0)); // not _both_ specified

    if (srcfn.size() > 0) {
	assert(fd < 0);
        fd = open(srcfn.c_str(), O_RDONLY);
        if (verbosity_ > normal)
            cerr << "InputByteStream: opening osfile:" << srcfn 
                 << (fd >= 0 ? " OK" : " failed") << endl;
        if (fd < 0) {
            string errstr = strerror (errno);
            throw InputByteStreamError
                ("InputByteStream: can't open file " + srcfn
                 + " to read (" + errstr + ")");
	}
    }
    assert(fd >= 0);

    return fd;
}

/**
 * Read a given number of bytes, stopping when it has read the
 * required number, or at end of file, whichever comes first.
 *
 * @param fd file descriptor
 * @param b pointer to buffer to be filled
 * @param len number of bytes to be read
 * @return the number of bytes actually read
 * 
 * @throws InputByteStreamError if there was an error reading the file
 */
size_t InputByteStream::certainly_read_(int fd, Byte* b, size_t len)
    throw (InputByteStreamError)
{
    size_t totread = 0;
    ssize_t thisread;

    assert(fd >= 0);

    while (len > 0
	   && (thisread = read(fd, (void*)b, len)) > 0) {
	b += thisread;
	len -= thisread;
	totread += thisread;
    }
    if (thisread < 0) {
	string errmsg = strerror(errno);
	throw InputByteStreamError
		("InputByteStream: can't read file " + fname_ + ": " + errmsg);
    }
    return totread;
}

/**
 * Reads a byte from the stream.  Increments the reading pointer.
 * This method does not signal an error at end-of-file; if {@link
 * #eof} is true or <em>becomes</em> true as a result of this attempt
 * to read past the end of the file, then we return zero.  That is,
 * <code>eof()</code> does not return true immediately the last byte
 * in the file has been read.
 *
 * @return the byte read, or zero if we are at the end of the file
 * @throws InputByteStreamError if there is some problem reading the stream
 */
Byte InputByteStream::getByte(void)
    throw (InputByteStreamError)
{
    if (eof())
	return 0;

    if (buf_ == 0)
	throw InputByteStreamError
		("InputByteStream::getByte: called after stream closed");

    assert(buf_ != 0 && buf_ <= p_ && p_ <= eob_);

    if (p_ == eob_)
	read_buf_();		// alters p_ and eob_, and possibly eof_

    Byte result;
    
    if (eof_)
	result = 0;
    else
	result = *p_++;

    assert(buf_ <= p_ && p_ <= eob_);

    return result;
}

/**
 * Retrieves a block from the current position in the stream.   Leaves
 * the pointer pointing after the block returned.
 *
 * @param length the size of block desired
 *
 * @return a pointer to a block of bytes
 * @throws InputByteStreamError if the requested number of bytes
 * cannot be read, which includes the case of this method being
 * called when <code>eof()</code> is true
 */
const Byte *InputByteStream::getBlock(unsigned int length)
    throw (InputByteStreamError)
{
    Byte *ret;

    if (eof())			// slightly redundant check, but gives
				// distinct report from
				// not-enough-space error below
	throw InputByteStreamError
		("InputByteStream::getBlock called after EOF");

    if (buf_ == 0)
	throw InputByteStreamError
		("InputByteStream::getBlock: called after stream closed");

    assert(buf_ != 0 && buf_ <= p_ && p_ <= eob_);

    if (length <= eob_-p_) {
	ret = p_;
	p_ += length;
	//cerr << "small: p_@" << p_-buf_ << endl;
    } else {
	// it's not all in the buffer: we'll need to read some more
	if (fd_ < 0) {
	    // nothing more to read
	    char errmsg[100];
	    sprintf(errmsg, "InputByteStream::getBlock: requested %d, but that's past EOF", length);
	    throw InputByteStreamError(errmsg);
	}
	bool read_ok;
	size_t inbufalready = eob_-p_;
	int mustread = length-inbufalready; // so (p_=(buf_+length)) <= eob_
	if (length <= buflen_) {
	    // whole block will fit in current buffer
	    STD::memmove((void*)buf_, (void*)p_, inbufalready);
	    read_ok = (certainly_read_(fd_,
				       buf_+inbufalready,
				       mustread)
		       == mustread);
	    //cerr << "medium: read_ok=" << (read_ok?"true":"false");
	} else {
	    // must expand buffer
	    int newbuflen = length * 3 / 2; // decent size
	    Byte* newbuf = new Byte[newbuflen];
	    STD::memcpy((void*)newbuf, (void*)p_, inbufalready);
	    read_ok = (certainly_read_(fd_,
				       newbuf+inbufalready,
				       mustread)
		       == mustread);
	    if (read_ok) {
		delete[] buf_;
		buf_ = newbuf;
		buflen_ = newbuflen;
	    }
	    //cerr << "large: read_ok=" << (read_ok?"true":"false");
	}
	if (! read_ok) {
	    char errmsg[100];
	    sprintf(errmsg, "InputByteStream::getBlock: requested %d, but that's past EOF", length);
	    throw InputByteStreamError(errmsg);
	}
	ret = buf_;
	eob_ = p_ = buf_ + length;
	//cerr << "; p=buf+" << p_-buf_ << ", eob=buf+" << eob_-buf_ << endl;
    }

    assert(buf_ != 0 && buf_ <= p_ && p_ <= eob_);

    return ret;
}

void InputByteStream::read_buf_()
    throw (InputByteStreamError)
{
    if (fd_ < 0) {
	eof_ = true;
	return;
    }
    
    assert(fd_ >= 0 && buf_ != 0);

 read_again:
    ssize_t bufcontents = read(fd_, buf_, buflen_);
    if (bufcontents < 0)
    {
        if (errno == EINTR)     // interrupted system call
            goto read_again;
        
	string errmsg = strerror(errno);
	throw InputByteStreamError
		("InputByteStream::read_buf_:"+fname_+": read error ("
		 +errmsg+")");
    }
    eof_ = (bufcontents == 0);
    eob_ = buf_ + bufcontents;
    if (verbosity_ >= debug)
	cerr << "InputByteStream::read_buf_: read "
	     << bufcontents << '/' << buflen_ << " from fd " << fd_
	     << "; eof=" << (eof_ ? "true" : "false")
	     << ", eob=buf+" << eob_-buf_
	     << endl;
    p_ = buf_;

    assert(buf_ != 0 && buf_ <= p_ && (eof_ || buf_ < eob_) && p_ <= eob_);
}

/**
 * Skips a given number of bytes forward in the stream.
 *
 * @param increment the number of bytes to move forward in the stream
 * @throws InputByteStreamError if we skip past the end of file
 */
void InputByteStream::skip (unsigned int increment)
    throw (InputByteStreamError)
{
    if (eof())
	throw InputByteStreamError("Skip while at EOF");

    assert(buf_ != 0 && buf_ <= p_ && p_ <= eob_);

    if (fd_ < 0) {
	// better be in the buffer already
	p_ += increment;
	if (p_ >= eob_) {
	    p_ = eob_;
	    eof_ = true;
	    throw InputByteStreamError("skip past end of file");
	}
    } else {
	if (increment >= eob_-p_) {
	    increment -= (eob_-p_);
	    read_buf_();		// changes p_ and eob_
	    while (increment >= (eob_-p_)) {
		if (eof_)
		    throw InputByteStreamError("skip past end of file");
		read_buf_();	// changes p_ and eob_
		increment -= (eob_-p_);
	    }
	}
	p_ += increment;
    }
    
    assert(buf_ != 0 && buf_ <= p_ && p_ <= eob_);
}

/**
 * Seeks to a specific point in the buffer.  This is only useful when
 * the buffer holds the complete file, that is, when
 * <code>bindToFileDescriptor</code> was called with parameter
 * <code>fillBufferAndClose</code> true.
 *
 * @param offset the offset from the beginning of the buffer, where the
 * current position is relocated to
 *
 * @throws InputByteStreamError if the offset would take the pointer
 * outside the buffer, or if the stream has been closed
 */
void InputByteStream::bufferSeek(unsigned int offset)
    throw (InputByteStreamError)
{
    if (buf_ == 0)
	throw InputByteStreamError
		("Call to bufferSeek when stream has been closed");
    if (offset >= (buflen_))	// unsigned offset can't be negative
	throw InputByteStreamError
		("Call to protected bufferSeek too large for buffer");

    assert(buf_ != 0 && buf_ <= p_ && p_ <= eob_);

    p_ = buf_ + offset;
    eof_ = (p_ == eob_);
    if (verbosity_ >= debug)
	cerr << "bufferSeek to " << offset
	     << "; eof=" << (eof_ ? "true" : "false")
	     << ", p=buf+" << p_-buf_ << ", eob=buf+" << eob_-buf_
	     << endl;

    assert(buf_ != 0 && buf_ <= p_ && p_ <= eob_);
}

/**
 * Reloads the buffer, presumably after the file descriptor has been
 * adjusted by an extending class.
 */
void InputByteStream::reloadBuffer(void)
{
    p_ = eob_;			// triggers reading of buffer
    eof_ = false;		// we guess -- harmless if wrong
    assert(fd_ >= 0 && buf_ != 0); // We don't rely on this
				   // assertion in this method, but
				   // if it is false, there's a
				   // programming error somewhere
    if (verbosity_ >= debug)
	cerr << "InputByteStream::reloadBuffer: p=buf+" << p_-buf_
	     << " eob=buf+" << eob_-buf_
	     << " eof=" << (eof_ ? "true" : "false")
	     << endl;
    assert(buf_ != 0 && buf_ <= p_ && p_ <= eob_);
}

/**
 * Closes the stream, releasing all resources.
 */
void InputByteStream::close(void)
{
    if (verbosity_ > normal)
	cerr << "InputByteStream::close" << endl;

    if (fd_ >= 0)
	::close(fd_);
    fd_ = -1;

#ifdef HAVE_MMAP
    if (mappedfd_ >= 0) {
	if (buf_ == 0)
	    cerr << "InputByteStream::close: -- odd, already deallocated"
		 << endl;
	else {
	    if (munmap(reinterpret_cast<MMAP_TYPE>(buf_), buflen_) == -1) {
		string errstr = strerror(errno);
		cerr << "InputByteStream: close: can't unmap file: "
		     << errstr << endl;
	    }
	    buf_ = 0;
	}
	::close(mappedfd_);
	mappedfd_ = -1;
    }
#endif

    if (buf_ != 0) {
	delete[] buf_;
	buf_ = 0;
    }

    eof_ = true;

    assert(fd_ < 0 && mappedfd_ < 0 && buf_ == 0);
}

/**
 * Sets the default buffer size to be used for reading files.
 *
 * @param length the size, in bytes, of the input buffer
 */
void InputByteStream::setDefaultBufferSize(unsigned int length)
{
    default_buffer_length_ = length;
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
 */
signed int InputByteStream::getSIS(int n)
    throw (InputByteStreamError)
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
 */
signed int InputByteStream::getSIU(int n)
    throw (InputByteStreamError)
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
 */
unsigned int InputByteStream::getUIU(int n)
    throw (InputByteStreamError)
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
 */
unsigned int InputByteStream::getUIU(int n, const Byte *p)
    throw (InputByteStreamError)
{
    if (n<0 || n>4)
	throw InputByteStreamError
		("InputByteStream: bad argument to getUIU(int,Byte*)");
    unsigned int t = 0;
    for (const Byte *b=p; n>0; n--, b++)
	t = t*256 + static_cast<unsigned int>(*b);
    return t;
}
