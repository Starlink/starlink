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
#  include <cstdio>
#  include <cctype>
#  include <cerrno>
#else
#  include <stdio.h>
#  include <ctype.h>
#  include <errno.h>
#endif

// the assert headers appear to be slightly less predictable...
#if HAVE_CASSERT 
#  include <cassert>
#elif HAVE_ASSERT_H
#  include <assert.h>
#endif

#if HAVE_SYS_SIGNAL_H
/* If it's available, use sys/signal.h rather than <csignal> or <signal.h>.
 * If we're compiling in a strict-ansi mode, these will _not_ have signals
 * which are specific to Unix/POSIX, which are, of course, precisely the
 * ones we're hoping to use.
 */
#  include <sys/signal.h>
#elif HAVE_SIGNAL_H
/* Let's hope these do the job */
#  include <signal.h>
#endif

#include <unistd.h>		// this is standard according to single-unix, 
				// how POSIXy is that?  How C++?
#ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
#endif

#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif

#include <map>

using STD::ostream;
using STD::ends;
using STD::endl;
using STD::cerr;

#include <stringstream.h>

/**
 * Takes care of the signal handling necessary for the functions in
 * the {@link PipeStream} class.  If your application for whatever
 * reason has to catch <code>SIGCHLD</code>, then to avoid interference
 * between these, you should do so using functions {@link
 * #expectAnother} and {@link #got_status}.  These two functions are the 
 * only `public' API in this namespace.
 */	
namespace PipeStreamSignalHandling {
    /* Handle SIGCHLDs by maintaining a set of pid/caught-status pairs. */
    struct process_status {
	sig_atomic_t pid;
	sig_atomic_t status;
	process_status() : pid(0), status(-1) { }
	void clear() { pid=0; status=-1; }
    };
    struct process_status* procs = 0; // initial value triggers initialisation
    sig_atomic_t nprocs;	// max no of children we can wait for
    sig_atomic_t nprocs_used;	// number of slots used
    bool got_status(pid_t pid, int* status);
    void expectAnother() throw (InputByteStreamError);
    extern "C" void childcatcher(int);
    extern "C" void alarmcatcher(int);
}


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
    PipeStreamSignalHandling::expectAnother();

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
                 << ": " << STD::strerror(errno) << endl;

        // Children should exit with _exit rather than exit.  See, eg.,
        // <http://www.erlenstar.demon.co.uk/unix/faq_toc.html#TOC6> 
	_exit(EXIT_FAILURE);

    } else {

	// ... in parent.  Close the writing end.
	::close(fd[1]);

	bindToFileDescriptor(fd[0]);

	if (getVerbosity() > normal)
	    cerr << "PipeStream: pid=" << pid_ << endl;
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
 * to kill it, by sending first HUP then KILL.  If
 * there is some reason why we can't reap the status -- because it has
 * not died or for, say, some permissions reason -- we set the
 * returned status to -1.  Any data not read from the pipe is discarded.
 */
void PipeStream::close(void)
{
    InputByteStream::close();

    if (pid_ > 0) {
	// make sure that SIGALRM and SIGCHLD aren't blocked
	sigset_t alrm_and_chld, oldmask;
	sigemptyset(&alrm_and_chld);
	sigaddset(&alrm_and_chld, SIGALRM);
	sigaddset(&alrm_and_chld, SIGCHLD);
	sigprocmask(SIG_UNBLOCK, &alrm_and_chld, &oldmask);
	
	int status = -1;
	bool keep_waiting
	    = !PipeStreamSignalHandling::got_status(pid_, &status);
	bool process_gone = false;
	for (int i=0; keep_waiting; i++) {

	    int sigtosend;

	    if (process_gone) {
		// We weren't able to signal to the process, but we still
		// can't get its status.  How has this happened?  Give up.
		keep_waiting = false;
	    } else {
		switch (i) {
                  case 0:
                    sigtosend = 0; // check process (really, just wait)
                    break;
		  case 1:
		    sigtosend = SIGINT;
		    break;
		  case 2:
		    sigtosend = SIGKILL;
		    break;
		  default:
		    keep_waiting = false; // give up
		    break;
		}
	    }

	    if (keep_waiting) {
		// temporarily block SIGALRM and SIGCHLD, until we can set
		// an alarm for ourself
		sigprocmask(SIG_BLOCK, &alrm_and_chld, 0);
		if (kill(pid_, sigtosend) == 0) {
		    sigset_t zeromask;
		    sigemptyset(&zeromask);
#if defined(HAVE_SETITIMER)
		    struct itimerval timer;
		    timer.it_value.tv_sec = 1;
		    timer.it_value.tv_usec = 0;
		    timer.it_interval.tv_sec = 0;
		    timer.it_interval.tv_usec = 0;
		    setitimer(ITIMER_REAL, &timer, 0);
#elif defined(HAVE_ALARM)
		    alarm(1);
#else
#error "Have neither setitimer nor alarm.  Can't proceed!"
#endif
		    sigsuspend(&zeromask); // unblock and pause
		    if (getVerbosity() > normal)
			cerr << "PipeStream::close sent signal " << sigtosend
			     << " and waited for child" << endl;
		} else {
		    // process wasn't there
		    process_gone = true;
		    if (getVerbosity() > normal)
			cerr << "PipeStream::close: can't send signal "
			     << sigtosend << " to process " << pid_
			     << " (" << STD::strerror(errno) << ")"
			     << endl;
		}

		keep_waiting
		    = !PipeStreamSignalHandling::got_status(pid_, &status);
	    }
	}
	pipe_status_ = status;
	pid_ = 0;

	// restore original signals mask
	sigprocmask(SIG_SETMASK, &oldmask, 0);
    }
}

/**
 * Returns the status of the command at the end of the pipe.  Since
 * this is only available after the process has completed, this 
 * invokes {@link #close} on the stream first.  Thus you should not
 * invoke this method until after you have extracted all of the
 * command's output that you want.  If <code>close</code> is unable
 * to terminate the process for some reason, this returns -1.
 *
 * @return the exit status of the process.
 */
int PipeStream::getTerminationStatus(void)
{
    close();
    return pipe_status_;
}

/**
 * Returns the contents of the stream as a string.  If some of the
 * stream has already been read by other methods of this class or its
 * parent, it cannot be re-read.  Any trailing whitespace, including
 * end-of-line characters, is stripped.
 *
 * It's possible that the returned string will be incomplete, if the
 * process failed somehow; irrespective of whether this happened, this
 * returns what it can, and if this condition matters to you, you
 * should check the termination status with
 * <code>getTerminationStatus</code>.
 *
 * @param allOfFile if true, then all of the file is read into the
 * string; if false (the default), only the first line is returned,
 * <em>not</em> including the newline or carriage-return which ends
 * the line
 *
 * @param gobbleRest if true (the default) then gobble the rest of
 * the file; has an effect only if <code>allOfFile</code> was false
 *
 * @return the stream contents (or some of it, depending on the
 * parameter values) as a string
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
    string response = SS_STRING(resp);
    if (getVerbosity() > normal)
        cerr << "PipeStream::getResult: returning <" << response
             << ">; eof=" << (eof() ? "true" : "false") << endl;

//     string response;
//     int status = getTerminationStatus();
//     if (WIFEXITED(status)) {
// 	if (getVerbosity() > normal)
// 	    cerr << "exit status=" << WEXITSTATUS(status) << endl;
// 	if (WEXITSTATUS(status) != 0 && getVerbosity() >= normal) {
// 	    cerr << "Command <" << orig_command_
// 		 << "> exited with non-zero status "
// 		 << WEXITSTATUS(status) << endl;
// 	}
// 	response = SS_STRING(resp);
//     } else if (WIFSIGNALED(status)) {
// 	// if the command fails we may come here
// 	if (getVerbosity() >= normal)
// 	    cerr << "Signalled with signal number " << WTERMSIG(status)
// 		 << (WCOREDUMP(status) ? " (coredump)" : " (no coredump)")
// 		 << endl;
// 	response = "";
//     } else if (WIFSTOPPED(status)) {
// 	if (getVerbosity() >= normal)
// 	    cerr << "Stopped on signal " << WSTOPSIG(status) << endl;
// 	response = "";
//     } else {
// 	char msg[100];
// 	sprintf(msg, "Impossible status %d from child %d", status, getPid());
// 	throw InputByteStreamError(msg);
//     }

    return response;
}

/*
 * All this is a rather simple-minded version of the pattern described in 
 * <http://www.cs.wustl.edu/~schmidt/signal-patterns.html>
 */

/**
 * Note that we expect to start another child.  This function must
 * be called before doing each <code>fork()</code>, as it does the
 * initialisation of the structures used by this set of functions, and
 * subsequently ensures that there is enough space in the list of
 * statuses to hold another one.
 *
 * @throws InputByteStreamError if we can't do this for some reason
 */
void PipeStreamSignalHandling::expectAnother()
    throw (InputByteStreamError)
{
    sigset_t chld, oldmask;
    sigemptyset(&chld);
    sigaddset(&chld, SIGCHLD);
    if (sigprocmask(SIG_BLOCK, &chld, &oldmask) < 0)
	throw InputByteStreamError("Can't set signal mask");
    
    if (procs == 0) {
	// first time
	nprocs = 8;
	nprocs_used = 0;
	procs = new struct process_status[nprocs];

	struct sigaction sa_chld;
	sa_chld.sa_handler = &PipeStreamSignalHandling::childcatcher;
	sigemptyset(&sa_chld.sa_mask);
#ifdef SA_RESTART
	sa_chld.sa_flags = SA_RESTART;
#else
	sa_chld.sa_flags = 0;
#endif
	if (sigaction(SIGCHLD, &sa_chld, 0) < 0)
	    throw new InputByteStreamError("Can't install CHLD handler");

	struct sigaction sa_alrm;
	sa_alrm.sa_handler = &PipeStreamSignalHandling::alarmcatcher;
	sigemptyset(&sa_alrm.sa_mask);
#ifdef SA_INTERRUPT
	sa_alrm.sa_flags = SA_INTERRUPT; // some SunOS
#else
	sa_alrm.sa_flags = 0;
#endif
	if (sigaction(SIGALRM, &sa_alrm, 0) < 0)
	    throw new InputByteStreamError("Can't install ALRM handler");

        if (InputByteStream::getVerbosity() > normal) {
            cerr << "expectAnother: initialised to " << nprocs << "; set now";
            for (int i=0; i<nprocs; i++)
                cerr << " " << procs[i].pid << '/' << procs[i].status;
            cerr << endl;
        }

    } else if (nprocs_used >= nprocs-1) {
        assert(nprocs > 0);
	int newnprocs = nprocs * 2;
	struct process_status* newprocs = new struct process_status[nprocs];
	for (int i=0; i<nprocs; i++)
	    newprocs[i] = procs[i];
	nprocs = newnprocs;
	delete[] procs;
	procs = newprocs;
        if (InputByteStream::getVerbosity() > normal) {
            cerr << "expectAnother: expanded to " << nprocs << "; set now";
            for (int i=0; i<nprocs; i++)
                cerr << " " << procs[i].pid << '/' << procs[i].status;
            cerr << endl;
        }
    }
    if (sigprocmask(SIG_SETMASK, &oldmask, 0) < 0)
	throw InputByteStreamError("Can't reset set signal mask");
}

/**
 * Checks to see if the given process has exited.  If there's an entry
 * for this PID in the list, then put the corresponding status in the
 * <code>status</code> parameter, and return true.  If not, return false; 
 * <code>*status</code> is then unchanged.
 *
 * @param pid the PID to check
 * @param status a pointer to the status to be returned
 * @return true if the status was in fact available, and is now in
 * <code>*status</code>; false otherwise
 */
bool PipeStreamSignalHandling::got_status(pid_t pid, int* status)
{
    assert(status != 0);
    for (int i=0; i<nprocs; i++) {
	if (procs[i].pid == pid) {
	    // found a slot with the required PID in it
	    *status = static_cast<int>(procs[i].status);
	    procs[i].clear();
	    nprocs_used--;
            if (InputByteStream::getVerbosity() > normal)
                cerr << "got_status: pid=" << pid << " status=" << *status
                     << " ("
                     << i << "/" << nprocs_used << "/" << nprocs
                     << ")" << endl;
	    return true;
	}
    }
    if (InputByteStream::getVerbosity() > normal)
        cerr << "got_status: no status for pid " << pid << endl;
    return false;
}

void PipeStreamSignalHandling::childcatcher(int signum)
{
    assert(signum == SIGCHLD);

    int i;
    int status;
    pid_t caughtpid = waitpid(0, &status, 0);
    for (i=0; i<nprocs; i++) {
	if (procs[i].pid == 0) { // free slot
	    procs[i].pid = static_cast<sig_atomic_t>(caughtpid);
	    procs[i].status = static_cast<sig_atomic_t>(status);
	    nprocs_used++;
            break;
	}
    }
    // If i=nprocs, then there's no space to store this signal.
    // Nothing much we can do -- but at least it isn't a zombie.
    return;
}
void PipeStreamSignalHandling::alarmcatcher(int signum)
{
    assert(signum == SIGALRM);
    return;			// nothing to do
}

