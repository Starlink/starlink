//    This file is part of dvi2bitmap.
//    Copyright 2003, Council for the Central Laboratory of the Research Councils
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

#include <PipeStream.h>
#include <Util.h>

#include <iostream>

#ifdef HAVE_CSTD_INCLUDE
#include <cstdio>
#include <cctype>
#include <cerrno>
#else
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#endif
#include <unistd.h>		// this is standard according to single-unix, 
				// how POSIXy is that?  How C++?
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#include <map>

#ifdef HAVE_STD_NAMESPACE
using std::ostream;
using std::ends;
using std::endl;
using std::cerr;
#define STD std
#else
#define STD
#endif

#include <stringstream.h>


/**
 * Creates a stream which returns the output of a given command.  The
 * constructor open a pipe with the given command, and prepares for
 * the results to be read by this class's accessor methods.
 * 
 * <p>If the second argument is omitted, then a default list of
 * environment variables, namely <code>PATH HOME LOGNAME SHELL TMPDIR</code>,
 * are inherited from the current environment.  If present, it should
 * be a space-separated list of environment variables with optional
 * values: if no equal sign is present, the variable's value is
 * inherited from the current environment.  For example, if this list
 * is given as in the form <code>env1=val1 env2= env3</code>, then
 * <code>env1</code> would receive the value <code>val1</code>,
 * <code>env2</code> would receive a null value, and <code>env3</code>
 * would be inherited.
 *
 * <p>If the command exits with a non-zero exit status, this is noted
 * on <code>stderr</code>, but any output from the command is still returned.
 *
 * <p>If there's no way of doing this on a particular platform, throw
 * <code>InputByteStreamError</code> always.
 *
 * @param cmd the command to be run
 *
 * @param envs if specified, this is a list of environment variables;
 * if it is an empty string, <code>""</code> (the default), then a
 * default list of variables is inherited
 *
 * @throws InputByteStreamError on any errors
*/
PipeStream::PipeStream (string cmd, string envs)
    throw (InputByteStreamError)
    : pipe_status_(-1), pid_(0), orig_command_(cmd)
{
#ifdef HAVE_PIPE
    /*
     * Can't use popen, because we want to control the environment
     * more.  Initialisation of kpathsea adds settings to the
     * environment which confuse an invocation of mktexpk.
     *
     * Implementation here follows useful example in
     * <http://www.erack.de/download/pipe-fork.c> 
     */
    int fd[2];

    if (pipe(fd))
	throw new DviError("PipeStream: failed to create pipe");

    pid_ = fork();
    if (pid_ < 0) {
	throw new DviError("PipeStream: failed to fork");
    } else if (pid_ == 0) {
	// ...in child.  We will write to the pipe, so immediately
	// close the reading end
	::close(fd[0]);
	// And we want our stdout to go down the writing end
	dup2(fd[1], STDOUT_FILENO);
	::close(fd[1]);

	char **argv = Util::string_list_to_array(Util::tokenise_string(cmd));

 	string defenvs = "PATH HOME LOGNAME SHELL TMPDIR";
	string_list envlist;
	if (envs == "") {
            envs = "+";         // triggers processing below
	}
        STD::map<string,string> env;
        for (string_list e = Util::tokenise_string(envs);
             ! e.empty();
             e.pop_front()) {
            if (e.front() == "+") {
                e.pop_front(); // the "+"
                string_list de = Util::tokenise_string(defenvs);
                while (! de.empty()) {
                    e.push_front(de.front());
                    de.pop_front();
                }
                if (getVerbosity() > normal) {
                    cerr << "Added: now";
                    for (string_list::const_iterator ci = e.begin();
                         ci != e.end();
                         ci++) {
                        cerr << " " << *ci;
                    }
                    cerr << endl;
                }
            } 
            string& s = e.front();
            int si = s.find('=');
            if (si == string::npos) {
                // no setting found
                char *envval = getenv(s.c_str());
                if (envval != 0) {
                    //cerr << "set [" << s << "]=" << envval << endl;
                    env[s] = envval;
                }
            } else {
                // value found
                env[s.substr(0,si)] = s.substr(si+1);
                //cerr << "substr: [" << s.substr(0,si) << "]=" << s.substr(si+1) << endl;
            }
        }
        for (STD::map<string,string>::const_iterator mi = env.begin();
             mi != env.end();
             mi++) {
            string v = mi->first;
            v.append("=");
            v.append(mi->second);
            envlist.push_back(v);
        }

	if (getVerbosity() > normal) {
            cerr << "Final envlist:";
            for (string_list::const_iterator si = envlist.begin();
                 si != envlist.end();
                 si++) {
                cerr << "  " << *si << endl;
            }
        }

	char **envp = Util::string_list_to_array(envlist);

	execve(argv[0], argv, envp);

	// This is an error
        if (getVerbosity() > normal)
            cerr << "Error executing " << argv[0]
                 << ": " << strerror(errno) << endl;

        // Children should exit with _exit rather than exit.  See, eg.,
        // <http://www.erlenstar.demon.co.uk/unix/faq_toc.html#TOC6> 
	_exit(EXIT_FAILURE);

    } else {

	// ... in parent.  Close the writing end.
	::close(fd[1]);

	bindToFileDescriptor(fd[0]);
    }
#else  /* HAVE_PIPE */
    throw new InputByteStreamError("Have no pipe() function");
#endif /* HAVE_PIPE */

}

PipeStream::~PipeStream()
{
    close();
}

/**
 * Closes the stream, and reaps the process status.  
 *
 * <p>If the process has not already terminated, this method attempts
 * to kill it, by sending first HUP.  If the process appears reluctant to
 * die, we abandon it, and do not reap its status.  If there is some
 * reason why we can't reap the status -- because it has not died or
 * for, say, some permissions reason -- we set the returned status to
 * -1.
 */
void PipeStream::close(void)
{
    InputByteStream::close();

    if (getVerbosity() > normal)
        cerr << "PipeStream::close" << endl;
    if (pid_ > 0) {
	errno = 0;
	if (kill(pid_, SIGHUP)) {
	    // didn't work
	    if (errno == ESRCH) {
		// no such process -- already dead -- just reap
		waitpid(pid_, &pipe_status_, 0);
	    } else {
		// something odd happened
		if (getVerbosity() >= normal) {
		    cerr << "PipeStream::close couldn't signal subprocess "
			 << pid_ << " -- not reaping status" << endl;
		}
		pipe_status_ = -1;
		// don't wait any more
	    }
	} else {
	    // It worked; thus there was a process to signal to; wait
	    // to check it's gone
	    sleep(1);
	    errno = 0;
	    if (kill(pid_, SIGHUP)) {
		if (errno == ESRCH) {
		    // good -- no such process
		    waitpid(pid_, &pipe_status_, 0);
		} else {
		    // ooops
		    if (getVerbosity() >= normal)
			cerr << "PipeStream::close: can't signal to process "
			     << pid_
			     << " -- not reaping status"
			     << endl;
		    pipe_status_ = -1;
		}
	    } else {
		// Oh dear, we were able to signal to the process, so
		// it's still there.  Abandon it, to avoid getting
		// trapped waiting for it.  Could we try sending KILL
		// to it?  Dangerous if it's ended up in
		// uninterruptable sleep.
		if (getVerbosity() >= normal)
		    cerr << "PipeStream::close: process "
			 << pid_
			 << " hasn't died (quickly) -- not reaping status"
			 << endl;
		pipe_status_ = -1;
	    }
	}
	pid_ = 0;
    }
}

/**
 * Returns the status of the command at the end of the pipe.  Since
 * this is only available after the command has run to completion,
 * this invokes {@link #close} on the stream, and waits until the command has
 * finished.  Thus, you should <em>not</em> call this until
 * <em>after</em> you have extracted all of the command's output that
 * you want.  Note that the status could be inaccurate if
 * <code>close</code> was unable to terminate the process.
 *
 * @return the exit status of the process.
 */
int PipeStream::getStatus(void)
{
    close();
    return pipe_status_;
};

/**
 * Returns the contents of the stream as a string.  If some of the
 * stream has already been read by other methods of this class or its
 * parent, it cannot be re-read.  Any trailing whitespace, including
 * end-of-line characters, is stripped.
 *
 * <p>If the current platform cannot support pipes, then return
 * <code>InputByteStreamError</code> immediately.
 *
 * @param allOfFile if true, then all of the file is read into the
 * string; if false (the default), only the first line is returned,
 * <em>not</em> including the newline or carriage-return which ends
 * the line
 *
 * @param gobbleRest if true (the default) then gobble the rest of
 * the file; has an effect only if <code>allOfFile</code> was false
 *
 * @return the stream as a string
 * @throws InputByteStreamError if there is some problem reading the stream
 */
 string PipeStream::getResult(bool allOfFile, bool gobbleRest)
    throw (InputByteStreamError)
{
    SSTREAM resp;
    if (getVerbosity() > normal)
        cerr << "PipeStream::getResult(" << (allOfFile?"true":"false")
             << ',' << (gobbleRest?"true":"false") << ")" << endl;
    if (allOfFile) {
	for (Byte b=getByte(); !eof(); b=getByte())
	    resp << b;
    } else {
	for (Byte b=getByte(); !(eof() || b=='\n' || b=='\r'); b=getByte())
	    resp << b;
	if (gobbleRest)
	    while (!eof())
		getByte();
    }
    if (getVerbosity() > normal)
        cerr << "PipeStream::getResult... got <" << SS_STRING(resp)
             << ">" << endl;

    string response;
    int status = getStatus();
    if (WIFEXITED(status)) {
	if (getVerbosity() > normal)
	    cerr << "exit status=" << WEXITSTATUS(status) << endl;
	if (WEXITSTATUS(status) != 0 && getVerbosity() >= normal) {
	    cerr << "Command <" << orig_command_
		 << "> exited with non-zero status "
		 << WEXITSTATUS(status) << endl;
	}
	response = SS_STRING(resp);
    } else if (WIFSIGNALED(status)) {
	// if the command fails we may come here
	if (getVerbosity() >= normal)
	    cerr << "Signalled with signal number " << WTERMSIG(status)
		 << (WCOREDUMP(status) ? " (coredump)" : " (no coredump)")
		 << endl;
	response = "";
    } else if (WIFSTOPPED(status)) {
	if (getVerbosity() >= normal)
	    cerr << "Stopped on signal " << WSTOPSIG(status) << endl;
	response = "";
    } else {
	char msg[100];
	sprintf(msg, "Impossible status %d from child %d", status, getPid());
	throw InputByteStreamError(msg);
    }

    return response;
}
