// -*-c++-*-
#ifndef _ShellCommand_h_
#define _ShellCommand_h_
/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: ShellCommand.h,v 1.1 1997/11/28 01:38:53 abrighto Exp $" 
 *
 * ShellCommand.h - class to exec a shell command and save the status,
 *                  stdout and stderr
 * 
 * who             when      what
 * --------------  ---------  ----------------------------------------
 * Allan Brighton  12 Jun 96  Created
 */


class ShellCommand {
private:
    int status_;		// command exit status
    char* stdOut_;		// command output
    char* stdErr_;		// error output

public:
    // constructor: runs the command and saves the results.
    ShellCommand(const char* cmd);
    
    // destructor
    ~ShellCommand();

    int status() {return status_;}

    // return ptr to the stdout or stderr of the command
    // Note: these will be deleted automatically with this object
    const char* stdOut() {return stdOut_;}
    const char* stdErr() {return stdErr_;}
};

#endif /* _ShellCommand_h_ */
