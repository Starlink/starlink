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

#include <FileByteStream.h>

#include <iostream>
//#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>		// for strerror
#ifdef HAVE_CSTD_INCLUDE
#include <cstdio>
#include <cerrno>
#include <cassert>
#else
#include <stdio.h>
#include <errno.h>
#include <assert.h>
#endif

#ifdef HAVE_STD_NAMESPACE
using std::cerr;
using std::sprintf;
using std::endl;
#endif


/**
 * Opens the requested file.  If preload is true, then open the file
 * and read it entire into memory (possibly helpful if the client will
 * be seeking a lot).  If the file can't be opened, then try adding
 * <code>tryext</code> to the end of it.
 *
 * @param filename the file to be opened
 * @param tryext a file extension which should be added to the end of
 * <code>filename</code> if that cannot be opened; default
 * <code>""</code> suppresses this
 * @param preload if true, then the file is read entirely into memory
 */
FileByteStream::FileByteStream(string& filename,
			       string tryext,
			       bool preload)
    throw (InputByteStreamError)
{
    if (getVerbosity() > normal)
	cerr << "FileByteStream(name=" << filename
	     << ",ext=" << tryext
	     << ",preload=" << (preload?"yes":"no") << ")" << endl;
    int newfd = -1;
    try {
	newfd = openSourceSpec("<osfile>" + filename);
    } catch (InputByteStreamError& e) {
	newfd = -1;
    }
    if (newfd < 0) {
	newfd = openSourceSpec("<osfile>" + filename + tryext);
	// Let it throw the exception, this time
	// If it succeeds, modify the argument
	filename += tryext;
    }
    assert(newfd >= 0);

    struct stat S;
    if (fstat (newfd, &S))
    {
	string errstr = strerror (errno);
	throw InputByteStreamError ("Can't stat open file (" + errstr + ")");
    }
    filesize_ = S.st_size;

    bindToFileDescriptor(newfd,
			 filename,
			 (preload ? filesize_ : 0),
			 preload);
}

FileByteStream::~FileByteStream()
{
    // nothing
}

/**
 * Sets the position of the file pointer.
 *
 * @param pos if positive, the offset from the beginning of the file;
 * if negative, the offset from the end of the file
 *
 * @throws InputByteStreamError if the offset is larger
 * than the size of the file
 */
void FileByteStream::seek(int pos)
    throw (InputByteStreamError)
{
    int fd = getFD();
    int offset = (pos >= 0 ? pos : filesize_ + pos);
    if (offset < 0 || offset > filesize_)
	throw InputByteStreamError("FileByteStream: seek out of bounds");

    if (fd < 0) {
	// preloaded
	bufferSeek(offset);
	// no need to reload buffer
    } else {
	if (lseek(fd, offset, SEEK_SET) < 0) {
	    string errmsg = strerror(errno);
	    throw InputByteStreamError("Can't seek (" + errmsg + ")");
	}
	if (getVerbosity() > normal)
	    cerr << "FileByteStream::seek(" << pos << "=>" << offset
		 << ") ok" << endl;
	reloadBuffer();
    }
}
