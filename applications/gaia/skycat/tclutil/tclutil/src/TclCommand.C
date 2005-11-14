/*
 * TclCommand.C - base class definitions for tcl command classes
 * "@(#) $Id: TclCommand.C,v 1.7 2005/02/02 01:43:02 brighton Exp $"
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * pbiereic        17/02/03  Added 'using namespace std'. Removed ::std specs.
 *
 */
static const char* const rcsId="@(#) $Id: TclCommand.C,v 1.7 2005/02/02 01:43:02 brighton Exp $";

using namespace std;
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <cerrno>
#include <cstring>
#include "config.h"
#include "error.h"
#include "TclCommand.h"


// static member: used to generate unique names
int TclCommand::seq_ = 0;

// static member: interp used for error reporting
Tcl_Interp* TclCommand::maininterp_ = NULL;

/*
 * Constructor - install the named tcl command in the interpreter
 * and initialize a hash table of tcl subcommands for this command.
 */
TclCommand::TclCommand(Tcl_Interp* interp, const char* cmdname, const char* instname)
: interp_(interp),
  status_(TCL_OK)
{
    // set the error routine to use for error reporting (see error.C)
    maininterp_ = interp;
    set_error_handler(tcl_error);

    // save the command name
    cmdname_ = strdup(cmdname);

    // generate a name if necessary
    if (strcmp(instname, "#auto") == 0) {
	instname_ = new char[strlen(cmdname_)+16];
	sprintf(instname_, "%s%d", cmdname_, seq_++);
    } else {
	instname_ = new char[strlen(instname)+1];
	strcpy(instname_, instname);
    }

    // create the basic tcl command and return its name
    Tcl_CreateCommand(interp, instname_, (Tcl_CmdProc*)TclCommand::tclCmdProc, (ClientData)this, 
		      TclCommand::tclDeleteProc);

    // The result of the comamnd is its name
    // Note: if you access the tcl interpreter in a subclass constructor,
    // you will have to reset the result again before returning.
    Tcl_SetResult(interp, instname_, TCL_STATIC);
}


/* 
 * Dxestructor - called when tcl command is deleted
 */
TclCommand::~TclCommand() 
{
    free((char*)cmdname_);
    delete[] instname_; 
    instname_ = NULL;
}


/*
 * check the arg count for a subcommand and return an error in
 * Tcl if it is out of range
 */
int TclCommand::check_args(const char* name, int argc, int min_args, int max_args)
{
    if (argc >= min_args && argc <= max_args) 
	return TCL_OK;
    Tcl_AppendResult(interp_, "wrong number of args, should be \"",
		     instname_, " ", name, " ?args?\"", NULL);
    return TCL_ERROR;
}


/*
 * Call the given method in this class with the given arguments
 * (in this case there is only one method defined: "delete"
 */
int TclCommand::call(const char* name, int len, int argc, char* argv[])
{
    if (strncmp(name, "delete", len) == 0) {
	return deleteCmd(argc, argv);
    }
    Tcl_AppendResult(interp_, "unknown ", cmdname_, 
		     " subcommand: \"", name, "\"", NULL);
    return TCL_ERROR;
}


/*
 * This method is called for operations on the tcl objects created above
 * - just call a member function to be defined in the subclass
 */
int TclCommand::tclCmdProc(ClientData thisPtr, Tcl_Interp* interp, int argc, char* argv[])
{
    // saved this ptr for command
    TclCommand* tclcmd = (TclCommand*)thisPtr;

    // must be at least "cmd subcmd"
    if (argc >= 2) {
	Tcl_ResetResult(tclcmd->interp_);
	int len = strlen(argv[1]);
	if (len)
	    return tclcmd->call(argv[1], len, argc-2, argv+2);
    }

    // error: arg count wrong
    Tcl_AppendResult(interp, "wrong number of args, should be \"",
		     argv[0], " command ?args?\"", NULL);
    return TCL_ERROR;
}


/*
 * This static method is called by Tk when a tcl object is deleted.
 * Delete the C++ object passed as client data.
 */
void TclCommand::tclDeleteProc(ClientData thisPtr)
{
    // pointer to current object
    TclCommand* tclcmd = (TclCommand*)thisPtr;
    delete tclcmd;
}


/*
 * delete subcommand - Remove the Tcl command from the interpreter
 * (the C++ object is deleted when the tclDeleteProc is called)
 */
int TclCommand::deleteCmd(int, char**) {
    return Tcl_DeleteCommand(interp_, instname_);
}



/* 
 * return an integer value in tcl
 */
int TclCommand::set_result(int i) 
{
    char buf[32];
    sprintf(buf, "%d", i);
    Tcl_SetResult(interp_, buf, TCL_VOLATILE);
    return TCL_OK;
}


/* 
 * return 2 integer values in tcl
 */
int TclCommand::set_result(int i, int j) 
{
    char buf[64];
    sprintf(buf, "%d %d", i, j);
    Tcl_SetResult(interp_, buf, TCL_VOLATILE);
    return TCL_OK;
}


/* 
 * return 2 double values in tcl
 */
int TclCommand::set_result(double i, double j) 
{
    // PWD: use Tcl_PrintDouble to get tcl_precision encoded doubles.
    char buf[TCL_DOUBLE_SPACE + 1];

    Tcl_ResetResult(interp_);
    Tcl_PrintDouble(interp_, i, buf );
    Tcl_AppendResult(interp_, buf, (char *)NULL);
    buf[0] = ' ';
    Tcl_PrintDouble(interp_, j, buf + 1 );
    Tcl_AppendResult(interp_, buf, (char *)NULL);
    return TCL_OK;
}


/* 
 * return a double value in tcl
 */
int TclCommand::set_result(double d) 
{
    // PWD: use Tcl_PrintDouble to get tcl_precision encoded doubles.
    char buf[TCL_DOUBLE_SPACE];
    Tcl_PrintDouble(interp_, d, buf );
    Tcl_SetResult(interp_, buf, TCL_VOLATILE);
    return TCL_OK;
}


/* 
 * return a string value in tcl
 */
int TclCommand::set_result(const char* s) 
{
    Tcl_SetResult(interp_, (char*)s, TCL_VOLATILE);
    return TCL_OK;
}


/* 
 * Reset the Tcl result to empty
 */
int TclCommand::reset_result() 
{
    Tcl_ResetResult(interp_);
    return TCL_OK;
}


/* 
 * append a string value to the tcl result
 */
int TclCommand::append_result(const char* s) 
{
    Tcl_AppendResult(interp_, (char*)s, NULL);
    return TCL_OK;
}


/* 
 * append an integer value to the tcl result list
 */
int TclCommand::append_element(int i) 
{
    char buf[32];
    sprintf(buf, "%d", i);
    Tcl_AppendElement(interp_, buf);
    return TCL_OK;
}


/* 
 * append 2 integer values to the tcl result list
 */
int TclCommand::append_element(int i, int j) 
{
    char buf[64];
    sprintf(buf, "%d %d", i, j);
    Tcl_AppendElement(interp_, buf);
    return TCL_OK;
}


/* 
 * append 2 double values to the tcl result list
 */
int TclCommand::append_element(double i, double j) 
{
    // PWD: use Tcl_PrintDouble to get tcl_precision encoded doubles.
    char buf[TCL_DOUBLE_SPACE + 1];
    Tcl_PrintDouble(interp_, i, buf );
    Tcl_AppendElement(interp_, buf);
    buf[0] = ' ';
    Tcl_PrintDouble(interp_, j, buf + 1 );
    Tcl_AppendElement(interp_, buf);
    return TCL_OK;
}


/* 
 * append a double value to the tcl result list
 */
int TclCommand::append_element(double d) 
{
    // PWD: use Tcl_PrintDouble to get tcl_precision encoded doubles.
    char buf[TCL_DOUBLE_SPACE + 1];
    Tcl_PrintDouble(interp_, d, buf );
    Tcl_AppendElement(interp_, buf);
    return TCL_OK;
}


/* 
 * append a string value to the tcl result list
 */
int TclCommand::append_element(const char* s) 
{
    Tcl_AppendElement(interp_, (char*)s);
    return TCL_OK;
}


/* 
 * report an error messageto the tcl result list
 */
int TclCommand::error(const char* msg1, const char* msg2) 
{
    // msg1 or msg2 might be the same as interp_->result...
    ostringstream os;
    os << msg1 << msg2;

    Tcl_ResetResult(interp_);
    Tcl_SetResult(interp_, (char*)os.str().c_str(), TCL_VOLATILE);
    return TCL_ERROR;
}

/* 
 * report an error message, keeping existing messages.
 */
int TclCommand::more_error(const char* msg1, const char* msg2) 
{
    // msg1 or msg2 might be the same as interp_->result...
    ostringstream os;
    os << msg1 << msg2;

    Tcl_AppendResult(interp_, os.str().c_str(), (char *)NULL);
    return TCL_ERROR;
}


/* 
 * static error message reporter, called from error.C to report error
 */
void TclCommand::tcl_error(const char* msg) 
{
    Tcl_ResetResult(maininterp_);
    Tcl_SetResult(maininterp_, (char*)msg, TCL_VOLATILE);
}
