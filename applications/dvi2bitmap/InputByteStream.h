/*
 *    This file is part of dvi2bitmap.
 *    Copyright 1999--2002, Council for the Central Laboratory of the Research Councils
 *    
 *    This program is part of the Starlink Software Distribution: see
 *    http://www.starlink.ac.uk 
 *
 *    dvi2bitmap is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    dvi2bitmap is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with dvi2bitmap; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    The General Public License is distributed along with this
 *    program in the file LICENCE.
 *
 *    Author: Norman Gray <norman@astro.gla.ac.uk>
 *    $Id$
 */


#ifndef INPUT_BYTE_STREAM_HEADER_READ
#define INPUT_BYTE_STREAM_HEADER_READ 1

#include <Byte.h>
#include <DviError.h>
#include <verbosity.h>

#include <string>

class InputByteStreamError : public DviError {
 public:
    InputByteStreamError(string s) : DviError(s) { }
};

/**
 * Abstracts a file as a stream of bytes.  Functions are provided to
 * read individual bytes from the file and blocks of contiguous
 * bytes.
 *
 * <p>Since this class is intended to help reading TeX DVI and PK
 * files, we also provide methods to read signed and unsigned
 * integers from the stream, encoded as described in the DVI driver
 * standard.
 *
 * <p>This class is designed to be extended (and is extended in fact
 * in classes {@link FileByteStream} and {@link PipeStream}).  The
 * subclassing interface consists of a no-argument constructor, a
 * method to associate the class with a given file descriptor ({@link
 * #bindToFileDescriptor}), and a convenience method to help opening
 * files, using the same specification syntax supported by this class
 * ({@link #openSourceSpec}).  Since one of the main purposes of this
 * subclassing is to support a class which handles a seekable input
 * stream, we also have {@link #getFD} to get the file descriptor
 * being handled, and {@link #reloadBuffer} to indicate to the parent
 * class that the underlying stream has been modified (typically by a
 * <code>seek</code> operation) so that the input buffer should be
 * invalidated.  If <code>bindToFileDescriptor</code> was invoked
 * with the <code>fillBufferAndClose</code> flag true, then the
 * {@link #bufferSeek} method allows rapid seeking by simply
 * adjusting the buffer pointer; though this is useless unless the
 * whole stream is in the buffer, the class makes no check on this,
 * and it is the extending class's responsibility to make sure that
 * this is not called inappropriately.
 */
class InputByteStream {

 public:
    InputByteStream(int fileno)
	    throw (InputByteStreamError);
    InputByteStream(string srcspec)
	    throw (InputByteStreamError);
    ~InputByteStream();

    bool eof();
    virtual void close();
    Byte getByte(void)
	    throw (InputByteStreamError);
    const Byte *getBlock(unsigned int length)
	    throw (InputByteStreamError);
    void skip (unsigned int)
	    throw (InputByteStreamError);

    signed int getSIU(int)
	    throw (InputByteStreamError);
    signed int getSIS(int)
	    throw (InputByteStreamError);
    unsigned int getUIU(int)
	    throw (InputByteStreamError);
    /*
     * static getUIU reads from an array, rather than the file (ie,
     * it's really nothing to do with InputByteStream, but it's here
     * for consistency.
     */
    static unsigned int getUIU(int, const Byte *)
	    throw (InputByteStreamError);
    static void setDefaultBufferSize(unsigned int length);
    /**
     * Sets the verbosity of this module.
     * @param level the required verbosity
     */
    static inline void verbosity (const verbosities level) {
	verbosity_ = level;
    }
    /**
     * Returns the verbosity setting of this class
     */
    static verbosities getVerbosity(void) { return verbosity_; };

 protected:
    InputByteStream();
    bool bindToFileDescriptor(int fileno,
			      string filename="",
			      int bufsize=0,
			      bool fillBufferAndClose=false,
			      bool assertIsSeekable=false)
	    throw (InputByteStreamError);
    int openSourceSpec(string srcspec)
	    throw (InputByteStreamError);
    /**
     * Returns the file descriptor this stream is bound to.  If there
     * is no open descriptor (see the <code>fillBufferAndClose</code>
     * parameter to {@link #bindToFileDescriptor}), returns negative.
     *
     * @return the file descriptor, or negative if the descriptor has
     * been closed
     */
    int getFD(void) const { return fd_; }
    void bufferSeek(unsigned int pos)
	    throw (InputByteStreamError);
    void reloadBuffer(void);

 private:
    int fd_;			/* file descriptor of handled file/pipe */
    int mappedfd_;		/* saved file descriptor of mapped file */
    string fname_;		/* name of file, or string rep'n of fd */
    Byte *buf_;			/* buffer */
    size_t buflen_;		/* size of buffer */
    Byte *p_;			/* current position in buffer */
    Byte *eob_;			/* end of buffer */
    bool eof_;			/* true on end of file (note this is from
                                   the client's point of view, not the class's:
                                   if we have `preloaded' a file, the
                                   underlying file will be closed, but there
                                   will still be more to read from the 
                                   stream) */
    void read_buf_(void)
	    throw (InputByteStreamError);
    size_t certainly_read_(int fd, Byte *b, size_t len)
	    throw (InputByteStreamError);

    static verbosities verbosity_;
    static unsigned int default_buffer_length_;
};

#endif /* INPUT_BYTE_STREAM_HEADER_READ */
