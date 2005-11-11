// -*-c++-*-
#ifndef _TclCommand_H_
#define _TclCommand_H_
/* 
 * TclCommand.h - base class for tcl commands implemented in C++.
 * 
 * This base class assumes that each derived class has a virtual "call" 
 * member function that will call a method by name to implement a Tcl 
 * subcommand. The search for a member function starts at the bottom
 * and ends at the top of the class hierarchy - in this class.
 *
 * The tcl command is assumed to have an [incr Tcl] like syntax:
 *
 *   cmdName instName ?args?
 *   or: cmdName #auto ?args?
 *   and then: instName subCmd ?args?
 *
 * See the man page for a complete description.
 * -----------------------------------------------------------------------------
 * "@(#) $Id: TclCommand.h,v 1.2 2005/02/02 01:43:02 brighton Exp $" 
 */

#include <tcl.h>


/* 
 * This is the base class for classes defining tcl commands from
 * C++ classes
 */
class TclCommand {
protected:
    // tcl interpreter 
    Tcl_Interp* interp_;
    static Tcl_Interp* maininterp_;

    // status after constructor
    int status_;

    // class of tcl command (same as prefix for instname_)
    const char* cmdname_;

    // name of tcl command created for this object
    char* instname_;
    
    // used to generate unique tcl command name if name is specified as '#auto'
    static int seq_;

protected:
    // tcl command proc, called by tcl, calls the correct member function
    static int tclCmdProc(ClientData, Tcl_Interp* interp, int argc, char* argv[]);

    // tcl delete proc, called when tcl object is deleted
    static void tclDeleteProc(ClientData);

    // check the arg count for a subcommand
    int check_args(const char* name, int argc, int min_args, int max_args);

    // call a member function by name
    virtual int call(const char* name, int len, int argc, char* argv[]);

    // return a value (or a pair of values) in tcl
    int set_result(int);
    int set_result(int, int);
    int set_result(double);
    int set_result(double, double);
    int set_result(const char*);

    // append a string to the tcl result
    int append_result(const char*);
 
    // append a value (or a pair of values) to the tcl result list
    int append_element(int);
    int append_element(int, int);
    int append_element(double);
    int append_element(double, double);
    int append_element(const char*);

    // Reset the Tcl result to empty
    int reset_result();

    // evaluate Tcl code
    int eval(const char* cmd) {return Tcl_Eval(interp_, (char*)cmd);}

    // report an error in tcl
    int error(const char* msg1, const char* msg2="");
    int more_error(const char* msg1, const char* msg2="");
    static void tcl_error(const char* msg);

public:

    // constructor
    TclCommand(Tcl_Interp*, const char* cmdname, const char* instname);

    // destructor
    virtual ~TclCommand();

    // tcl delete sub command (always the same)
    virtual int deleteCmd(int argc, char** argv);


    // member access
    Tcl_Interp* interp() {return interp_;}
    int status() const { return status_; }
    char* instname() {return instname_;}

};


#endif /* _TclCommand_H_ */


