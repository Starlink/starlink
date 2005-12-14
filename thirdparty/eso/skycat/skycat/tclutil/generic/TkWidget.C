/*
 * TkWidget.C - base class definitions for Tk widgets implemented in C++
 *
 * "@(#) $Id: TkWidget.C,v 1.6 2005/02/02 01:43:02 brighton Exp $"
 *
 * See the man page for a complete description.
 *
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * pbiereic        17/02/03  Added 'using namespace std'. Removed ::std specs.
 */

static const char* const rcsId="@(#) $Id: TkWidget.C,v 1.6 2005/02/02 01:43:02 brighton Exp $";

#include <cstdlib>
#include <cctype>
#include <iostream>
#include <cstring>
#include "config.h"
#include "TkWidget.h"

using namespace std;

/*
 * Constructor - create the widget and install the tcl command of the
 * same name.
 * 
 * pclass - if non null, is the expected class of the parent widget
 *          (Canvas, Frame, ...)
 * 
 * specs - is a pointer to the widget's config struct, which defines
 *         the config options for the widget
 * 
 * options - is a derived struct that holds the values defined in specs.
 *
 * argc, argv - are passed from the widget command (argv[0] is the widget command)
 */
TkWidget::TkWidget(Tcl_Interp* interp, const char* pclass, 
		   Tk_ConfigSpec* specs, TkWidgetOptions& options,
		   int /* argc */, char* argv[])
: TclCommand(interp, argv[0], argv[1]),
  tkwin_(NULL),
  pname_(strdup(instname_)),
  wclass_(strdup(cmdname_)),
  configSpecsPtr_(specs),
  optionsPtr_(&options),
  redraw_pending_(0)
{
    // if pclass was specified, check that the parent window has that class
    if (pclass != NULL) {
	// get the name of the parent window from the widgets's path
	char* p = strrchr(pname_, '.');
	if (p != NULL) 
	    *p = '\0';

	if (p == NULL || 
	    strcmp(Tk_Class(Tk_NameToWindow(interp, pname_, Tk_MainWindow(interp))), 
		   pclass) != 0) {
	    status_ = TCL_ERROR;
	    Tcl_ResetResult(interp_);
	    Tcl_AppendResult(interp, "bad path name for ", cmdname_, ": \"", 
			     instname_, "\" parent of ", cmdname_, 
			     " should be a ", pclass, " widget", 0);
	    return;
	}
    }
	    
    // create the widget window
    tkwin_ = Tk_CreateWindowFromPath(interp, Tk_MainWindow(interp), instname_, NULL);
    if (tkwin_ == NULL) {
	status_ = TCL_ERROR;
	return;
    }

    display_ = Tk_Display(tkwin_);

    // set the class to the widget type with the first letter capitalized
    *wclass_ = toupper(*wclass_);
    Tk_SetClass(tkwin_, wclass_);

    // add event handler to catch destroy event
    Tk_CreateEventHandler(tkwin_, StructureNotifyMask, structureNotify, (ClientData)this);

}


/* 
 * Dxestructor - called when tcl command is deleted
 */
TkWidget::~TkWidget() 
{
    free(pname_);
    free(wclass_);

    /* free config options */
    Tk_FreeOptions(configSpecsPtr_, (char *) optionsPtr_, display_, 0);

}


/* 
 * This static method is called for StructureNotify events in the widget
 */
void TkWidget::structureNotify(ClientData clientData, XEvent *eventPtr)
{
    TkWidget* thisPtr = (TkWidget*)clientData;

    if (eventPtr->type == DestroyNotify) 
	thisPtr->destroyNotify(eventPtr);
    else if (eventPtr->type == ConfigureNotify)
	thisPtr->configureNotify(eventPtr);
}


/* 
 * method called for DestroyNotify events in the widget 
 */
void TkWidget::destroyNotify(XEvent *eventPtr) 
{
    Tk_CancelIdleCall(redrawWidget, (ClientData)this);
    tkwin_ = NULL;
    Tk_EventuallyFree((ClientData) this, destroyProc);
}


/* 
 * method called for ConfigureNotify events in the widget 
 * (when the window is resized)
 */
void TkWidget::configureNotify(XEvent *eventPtr) 
{
}


/* 
 * This static method is invoked by Tk_EventuallyFree to clean up 
 * the internal structure of the widget at a safe time
 */
void TkWidget::destroyProc(char *clientData)
{
    TkWidget *thisPtr = (TkWidget *)clientData;

    // deleting the tcl command will result in the virtual 
    // destructors being called, which should do the cleanup
    Tcl_DeleteCommand(thisPtr->interp_, thisPtr->instname_);
}


/*
 * This procedure needs to be called from the derived class constructor
 * to complete the widget initialization. This can't be done in the 
 * constructor here since the options_ struct wouldn't be initialized yet.
 */
int TkWidget::initWidget(int argc, char* argv[])
{
    // handle the config options
    if ((status_ = configureWidget(argc-2, argv+2, 0)) != TCL_OK) {
        Tk_DestroyWindow(tkwin_);
        tkwin_ = NULL;
	return TCL_ERROR;
    }

    // return the name of the widget as a tcl result
    Tcl_SetResult(interp_, instname_, TCL_STATIC);
    return TCL_OK;
}


/*
 * This procedure is called to process an argv/argc list, plus
 * the Tk option database, in order to configure (or reconfigure) 
 * a widget.
 */
int TkWidget::configureWidget(int argc, char* argv[], int flags)
{
    if (Tk_ConfigureWidget(interp_, tkwin_, configSpecsPtr_,
			   argc, argv, (char*)optionsPtr_, flags) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}




/*
 * this is called for the configure widget command
 */
int TkWidget::configureCmd(int argc, char* argv[])
{
    if (argc == 0) {
	return Tk_ConfigureInfo(interp_, tkwin_, configSpecsPtr_, 
				(char*)optionsPtr_, NULL, 0);
    } else if (argc == 1) {
	return Tk_ConfigureInfo(interp_, tkwin_, configSpecsPtr_, 
				(char*)optionsPtr_, argv[0], 0);
    } else {
	eventually_redraw();
	return configureWidget(argc, argv, TK_CONFIG_ARGV_ONLY);
    }
}


/*
 * If not already scheduled, schedule the widget to be redrawn
 */
int TkWidget::eventually_redraw()
{
    if (!redraw_pending_) {
	Tk_DoWhenIdle(redrawWidget, (ClientData) this);
	redraw_pending_ = 1;
    }
    return TCL_OK;
}


/*
 * Redraw the widget by calling the virtual redraw_ method
 * which may be defined in a derived class.
 */
void TkWidget::redrawWidget(ClientData clientData)
{
    TkWidget* thisPtr = (TkWidget*)clientData;
    thisPtr->redraw();
    thisPtr->redraw_pending_ = 0;
}


/*
 * this method may be redefined in a derived class to redraw
 * the widget.
 */
void TkWidget::redraw()
{
}

/*
 * this is called for the tcget widget command
 */
int TkWidget::cgetCmd(int argc, char* argv[])
{
    if (argc != 1) 
	return error("wrong # args: should be: \"$widget cget option\"");
    
    return Tk_ConfigureValue(interp_, tkwin_, configSpecsPtr_,
			     (char*)optionsPtr_, argv[0], TK_CONFIG_ARGV_ONLY);
}


/*
 * Call the given method in this class with the given arguments
 * (In this case there is only one command defined: "configure"
 */
int TkWidget::call(const char* name, int len, int argc, char* argv[])
{
    if (strncmp(name, "configure", len) == 0) {
	return configureCmd(argc, argv);
    } 
    else if (strncmp(name, "cget", len) == 0) {
	return cgetCmd(argc, argv);
    }
    return TclCommand::call(name, len, argc, argv);
}
