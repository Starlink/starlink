/*
 *    This file is part of dvi2bitmap.
 *    Copyright 2003, Council for the Central Laboratory of the Research Councils
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

#ifndef PIPE_STREAM_HEADER_READ
#define PIPE_STREAM_HEADER_READ 1

#include <InputByteStream.h>
#include <sys/types.h>

class PipeStream : public InputByteStream {
 public:
    PipeStream (string cmd, string envs="")
	    throw (InputByteStreamError);
    ~PipeStream();
    string getResult(void)
	    throw (InputByteStreamError);
    virtual void close(void);
    int getStatus(void);
    /**
     * Returns the PID of the running process.  After the process has
     * finished, this will return zero.
     *
     * @return the process's PID, or zero if it has finished
     */
    pid_t getPid(void) const { return pid_; };
 private:
    int pipe_status_;
    pid_t pid_;
    const string orig_command_;
};


#endif /* PIPE_STREAM_HEADER_READ */
