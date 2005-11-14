/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: ShellCommand.C,v 1.5 2005/04/04 19:01:06 brighton Exp $" 
 *
 * ShellCommand.C - methods for class ShellCommand, util class for
 *                  running a shell command and getting the output.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  12 Jun 96  Created
 * pbiereic        17/02/03  Added 'using namespace std'.
 */
static const char* const rcsId="@(#) $Id: ShellCommand.C,v 1.5 2005/04/04 19:01:06 brighton Exp $";


using namespace std;
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <unistd.h>
#include <cstdlib>
#include <cstdio>
#include <csignal>
#include <iostream>
#include <cstring>
#include "error.h"
#include "define.h"
#include "ShellCommand.h"


/*
 * local util to read a file desc into memory and return an
 * allocated buffer for it. 
 */
static char* read_pipe(int fd) 
{
    struct stat stat_buf;
    if (fstat(fd, &stat_buf) != 0) {
	sys_error("stat");
	return NULL;
    }

    char* buf = new char[stat_buf.st_size+1];
    buf[0] = '\0';
    if (read(fd, buf, stat_buf.st_size) != stat_buf.st_size) {
	sys_error("read failed");
	return NULL;
    }
    buf[stat_buf.st_size] = '\0';
    return buf;
}


/*
 * constructor: runs the command and saves the results
 */
ShellCommand::ShellCommand(const char* cmd)
    : status_(0),
      stdOut_(NULL),
      stdErr_(NULL)
{
    pid_t pid;
    int stdout_fds[2], stderr_fds[2];	// stdout and stderr pipes
    
    if (pipe(stdout_fds) || pipe(stderr_fds)) {
	status_ = sys_error("coudn't create pipe");
    }

    pid = fork ();
    if (pid < 0) {
        status_ = sys_error("could not fork process");
	return;
    }

    if (pid == 0) {
	// child process.
	dup2(stdout_fds[1], 1);
	dup2(stderr_fds[1], 2);
	close(stdout_fds[0]);
	close(stderr_fds[0]);
        execl ("/bin/sh", "sh", "-c", cmd, NULL);
        _exit (256);
    }

    // parent process.
    if (waitpid (pid, &status_, 0) == -1) {
	status_ = sys_error("error waiting for process");
	kill(pid, SIGTERM);  // allan: 6.3.98: by request
	kill(pid, SIGKILL);  // in case first kill didn't work
        return;
    }

    int status = status_;  // workaround for bug in egcs-1.0.3?
    status_ = WEXITSTATUS(status);
    stdOut_ = read_pipe(stdout_fds[0]);
    stdErr_ = read_pipe(stderr_fds[0]);
    close(stdout_fds[0]);
    close(stderr_fds[0]);
    close(stdout_fds[1]);
    close(stderr_fds[1]);

    if (status_ != 0 && stdErr_ && strlen(stdErr_)) {
	error(stdErr_);
    }
}

    
/*
 * destructor - free the command results
 */
ShellCommand::~ShellCommand()
{
    if (stdOut_)
	delete stdOut_;
    if (stdErr_)
	delete stdErr_;

}

