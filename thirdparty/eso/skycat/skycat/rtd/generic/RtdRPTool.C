/*
 * E.S.O. - VLT project 
 *
 * RtdRPTool.C - member routines for class RtdRPTool, RtdRecorder,
 *		 and RtdPlayback.
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * D.Hopkinson     20/02/97  Created
 * D.Hopkinson	   17/04/97  Added playback and recorder tools,
 *			     changed names, added inheritance
 * P.Biereichel    07/07/97  Adapted for shared library. Code improved
 *                           and bugs fixed
 * pbiereic        28/05/01  Included Allan's changes for tcl8.3.3
 */

#include <unistd.h>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "RtdRPTool.h"

static rtdShm shmInfo_;		// Structure for managing shm/semaphores

// Define the image types for the recorder and playback tools.
static Tk_ImageType rtdRecorderType = {
    "rtdrecorder",		/* name */
    RtdRecorder::CreateImage,	/* createProc */
    RtdRecorder::GetImage,	/* getProc */
    RtdRecorder::DisplayImage,  /* displayProc */
    RtdRecorder::FreeImage,	/* freeProc */
    RtdRecorder::DeleteImage,	/* deleteProc */

#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    (Tk_ImagePostscriptProc *) NULL,    /* postscriptProc */
#endif

    (Tk_ImageType *)NULL
};

static Tk_ImageType rtdPlaybackType = {
    "rtdplayback",
    RtdPlayback::CreateImage,
    RtdPlayback::GetImage,
    RtdPlayback::DisplayImage,
    RtdPlayback::FreeImage,
    RtdPlayback::DeleteImage,

#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    (Tk_ImagePostscriptProc *) NULL,    /* postscriptProc */
#endif

    (Tk_ImageType *)NULL
};

/*
 * Registers the recorder and playback class types so that they may
 * be instantiated from the Tcl code.
 *
 * Arguments:
 *	Tcl_Interp * interp - pointer to the Tcl interpreter structure
 *
 * Return value:
 *	Return value from Tcl_Eval
 */
extern "C" int RtdrecordInit(Tcl_Interp* interp)  
{
    // add the rtdrecorder and rtdplayback image types
    Tk_CreateImageType(&rtdRecorderType);
    Tk_CreateImageType(&rtdPlaybackType);
    // Set the shared memory information structure to NULL
    memset(&shmInfo_, '\0', sizeof(rtdShm));
    return TCL_OK;
}

/*
 * These classes are defined as part of the C++/Tcl interface. They
 * contain a table of Tcl commands, with the addresses of corresponding 
 * C++ methods to call. See the TclCommand man page for more details.
 */
static class RtdRPToolSubCmds {
public:
    char *name;
    int (RtdRPTool::*fptr)(int argc, char *argv[]);
    int min_args;
    int max_args;
} RPsubcmds_[] = {
    {"close", &RtdRPTool::close, 0, 0},
    {"cycle", &RtdRPTool::cycle, 1, 1},
    {"filename", &RtdRPTool::filename, 1, 1},
    {"status", &RtdRPTool::status, 0, 0},	// Declared inline in header
};

static class RtdRecorderSubCmds {
public:
    char *name;
    int (RtdRecorder::*fptr)(int argc, char *argv[]);
    int min_args;
    int max_args;
} Recsubcmds_[] = {
    {"camera", &RtdRecorder::camera, 1, 1},
    {"file", &RtdRecorder::file, 2, 2},
    {"record", &RtdRecorder::record, 0, 0},
    {"stop", &RtdRecorder::stop, 0, 0},
    {"subimage", &RtdRecorder::subimage, 1, 5}
};

static class RtdPlaybackSubCmds {
public:
    char *name;
    int (RtdPlayback::*fptr)(int argc, char *argv[]);
    int min_args;
    int max_args;
} Playsubcmds_[] = {
    {"close", &RtdPlayback::close, 0, 0},
    {"filename", &RtdPlayback::filename, 1, 1},
    {"gotoimage", &RtdPlayback::gotoimage, 1, 1},
    {"play", &RtdPlayback::play, 0, 0},
    {"props", &RtdPlayback::props, 2, 2},
    {"hastime", &RtdPlayback::hastime, 0, 0},
    {"reset", &RtdPlayback::reset, 0, 0},
    {"spool", &RtdPlayback::spool, 1, 1},
    {"step", &RtdPlayback::step, 0, 0},
    {"stop", &RtdPlayback::stop, 0, 0}
};

/*
 * Recorder/playback tool object constructor. This does basic 
 * initialisation, and also registers the process with the server daemon.
 *
 * Arguments:
 *	Tcl_Interp *interp - pointer to the Tcl interpreter structure
 *	char *instname - the instance name of the itk widget
 *	int arg, char **argv - argument list, unused
 *	Tk_ImageMaster - Tk master
 */
RtdRPTool::RtdRPTool(Tcl_Interp* interp, char* instname, int argc, char** argv, Tk_ImageMaster master) :
    TclCommand(interp, instname, instname),
    eventHndl_(NULL),
    status_(TCL_OK),
    cycleMode_(1),
    fileHandler((RtdRPFile *)NULL),
    master_(master),
    tkwin_(Tk_MainWindow(interp)),
    display(Tk_Display(Tk_MainWindow(interp)))
{
    status_ = RtdRPTool::init();
}

/*
 * Recorder object destructor.
 */
RtdRPTool::~RtdRPTool()
{
    cleanup();
}

/*
 * init routine: if necessary register to rtdServer
 */

int RtdRPTool::init()
{
    if (eventHndl_ == NULL) {
	eventHndl_ = new rtdIMAGE_EVT_HNDL;

	/* register to rtdServer */
	if (rtdInitImageEvt("RTDRPTOOL", eventHndl_, NULL) != RTD_OK) {
	    delete eventHndl_;
	    eventHndl_ = NULL;
	    return TCL_ERROR;
	}
    }
    return TCL_OK; 
}


/*
 * Overridden TclCommand method for interfacing the C++ with the Tcl code.
 * This method cycles over the RtdRPToolSubCmds to search for the method
 * to call. If no method is found (which it should be) control is passed to
 * the baseclass method).
 *
 * Arguments:
 *	const char *name - name of the subcommand to call
 *	int len - length of the subcommand name
 *	int argc, char **argv - argument list for subcommand
 *
 * Return value:
 *	Return value from subcommand.
 */
int RtdRPTool::call(const char *name, int len, int argc, char *argv[])
{
    for (int i = 0; i < sizeof(RPsubcmds_)/sizeof(*RPsubcmds_); i++) {
	RtdRPToolSubCmds *t = &RPsubcmds_[i];
	if (strcmp(t->name, name) == 0) {
	    if (check_args(name, argc, t->min_args, t->max_args) != TCL_OK) {
		return TCL_ERROR;
	    }
	    return (this->*t->fptr)(argc, argv);
	}
    }
    return TclCommand::call(name, strlen(name), argc, argv);
}

/*
 * Toggle the cycle mode on or off. If cycle is on, then the playback/recorder
 * object will return to the start of the file when the file is full/completed.
 *
 * Usage:
 *	$rtdrecorder/$rtdplayback cycle <1/0>
 *
 * Arguments:
 *	int argc, char *argv[] - argument list,
 *	    argv[0] - 0,1 (value for cycleMode_)
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdRPTool::cycle(int argc, char *argv[])
{
    // Just copy argument into cycleMode_
    cycleMode_ = atoi(argv[0]);

    if (cycleMode_ < 0) {
	return error("Bad argument for cycle subcommand");
    }

    return TCL_OK;
}

/*
 * Close the dialogue. Call the cleanup function.
 *
 * Arguments:
 *	int argc, char *argv[] - argument list, unused
 *
 * Return value:
 *	TCL_OK
 */
int RtdRPTool::close(int argc, char *argv[])
{
    cleanup();

    return TCL_OK;
}

/*
 * Set the current file name.
 *
 * Usage:
 *	$rtdrecorder/$rtdplayback filename <name>
 *
 * Arguments:
 *	int argc, char *argv[] - argument list:
 *	    argv[0] - file name
 *
 * Return value:
 *	TCL_OK
 */
int RtdRPTool::filename(int argc, char *argv[])
{
    // Copy across the file name.
    strncpy(fileName, argv[0], MAXFILENAMELEN);

    return TCL_OK;
}

/*
 * This is invoked when the dialogue is quit, and does basic memory management,
 * and deinitialises the server.
 *
 * Arguments:
 *	None
 */
void RtdRPTool::cleanup()
{
    // Remove any open file handler objects.
    if (fileHandler) {
	delete(fileHandler);
	fileHandler = (RtdRPFile *)NULL;
	Mem_RPTcleanup();
    }

    // Remove the event handle.
    if (eventHndl_) {
	rtdClose(eventHndl_, NULL);
	delete(eventHndl_);
	eventHndl_ = NULL;
    }
}


/* - End of RtdRPTool method definitions -				     */
/*===========================================================================*/
/* - Start of RtdRecorder method definitions -				     */

/*
 * This is called when the recorder object is instantiated from the Tcl code.
 * This simply creates an RtdRecorder object instance.
 *
 * Arguments:
 *	Tcl_Interp *interp - pointer to interpreter structure
 *	char *name - instance name
 *	int argc, char *argv[] - argument list, unused
 *	Tk_ImageType *typePtr - Tk image type
 *	Tk_ImageMaster - Tk image master
 *	ClientData *clientDataPtr - client data
 *
 *	For more information, see Tk_CreateImageType(3).
 *
 * Return value:
 *	TCL_OK
 */
int RtdRecorder::CreateImage(
    Tcl_Interp *interp,         // Interpreter for application containing image.
    char *name,                 // Name to use for image.
    int argc,                   // Number of arguments.
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    Tcl_Obj *CONST objv[],      // Argument objects for options
#else
    char **argv,                // Argument strings for options
#endif
    Tk_ImageType *typePtr,      // Pointer to our type record (not used). 
    Tk_ImageMaster master,
    ClientData *clientDataPtr)
{
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    // just generate an argv from the objv argument
    char* argv[64];  // there shouldn't be more than a few options...
    for(int i = 0; i < argc; i++)
        argv[i] = Tcl_GetString(objv[i]);
    argv[argc] = NULL;
#endif
    RtdRecorder *im = new RtdRecorder(interp, name, argc, argv, master);

    return TCL_OK;
}

/*
 * RtdRecorder contructor. Initialises the recorder properties.
 *
 * Arguments:
 *	As for RtdRPTool - arguments are just passed on to baseclass.
 */
RtdRecorder::RtdRecorder(Tcl_Interp *interp, char *instname, int argc, char **argv, Tk_ImageMaster master) :
    RtdRPTool(interp, instname, argc, argv, master),
    fileFormat_(FITS_CUBE),
    fileSize_(MAXFILESIZE),
    attached_(0),
    subImage_(0),
    x0_(0),
    y0_(0),
    width_(0),
    height_(0)
{
    // Default camera name is RTDSIMULATOR.
    strcpy(camera_, "RTDSIMULATOR");
}

/*
 * Callback routine, invoked when the socket connection to the server becomes
 * readable (i.e. an image has been received).
 *
 * Arguments:
 *	ClientData clientData - client data from the callback
 *	int mask - Tcl/Tk mask
 *
 * Return value:
 *	None.
 */
void RtdRecorder::fileEventProc(ClientData clientData, int mask)
{
    // Call a processing routine.
    RtdRecorder* thisPtr = (RtdRecorder *)clientData;
    thisPtr->processFileEvent();
}

/*
 * Routine to process the incoming file events. Called from the callback
 * routine: fileEventProc. If the cycleMode is off then this routine also
 * stops the data acquisition immediately if the maximum file size is
 * exceeded.
 *
 * Arguments:
 *	None.
 *
 * Return value:
 *	None.
 */
int RtdRecorder::processFileEvent()
{
    rtdIMAGE_INFO imageInfo;
    if (RtdRPTool::init() == TCL_ERROR)
	return TCL_ERROR;

    // Get the image information that is waiting.
    if (rtdRecvImageInfo(eventHndl_, &imageInfo, 0, NULL) == RTD_ERROR)
	return TCL_ERROR;
    // Pass control to the image processor.
    fileHandler->addImage(&imageInfo, subImage_, x0_, y0_, width_, height_);

    // Service the semaphore contained in the image information.
    rtdShmServicePacket(&imageInfo);

    /*
     * If the file size has exceeded the maximum, then we want to stop
     * here. Call the stop routine.
     */
    if (fileHandler->fileSize() > fileSize_ && !cycleMode_) {
        fprintf(stderr, "Full up!\n");
	stop(0, NULL);
    }
    if (fileHandler->fileFull() && !cycleMode_) {
	stop(0, NULL);
    }
    return TCL_OK;
}

/*
 * Overridden TclCommand method for interfacing the C++ with the Tcl code.
 * This method cycles over the RtdRPToolSubCmds to search for the method
 * to call. If no method is found (which it should be) control is passed to
 * the baseclass method).
 *
 * Arguments:
 *	const char *name - name of the subcommand to call
 *	int len - length of the subcommand name
 *	int argc, char **argv - argument list for subcommand
 *
 * Return value:
 *	Return value from subcommand.
 */
int RtdRecorder::call(const char *name, int len, int argc, char *argv[])
{
    for (int i = 0; i < sizeof(Recsubcmds_)/sizeof(*Recsubcmds_); i++) {
	RtdRecorderSubCmds *t = &Recsubcmds_[i];
	if (strcmp(t->name, name) == 0) {
	    if (check_args(name, argc, t->min_args, t->max_args) != TCL_OK) {
		return TCL_ERROR;
	    }
	    return (this->*t->fptr)(argc, argv);
	}
    }
    return RtdRPTool::call(name, strlen(name), argc, argv);
}

/*
 * Method to set image recording going.
 *
 * Usage:
 *	$rtdrecorder record
 *
 * Arguments:
 *	int argc, char *argv[] - argument list
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdRecorder::record(int argc, char *argv[])
{
    char msg[64];		// User error message

    if (RtdRPTool::init() == TCL_ERROR)
	return TCL_ERROR;

    if (!fileFormat_) {
        fprintf(stderr, "FileFormat object is NULL\n");
    }

    // Set up an object to control the file handling. File will be opened
    // after the first image event.
    if (fileFormat_ == COMP_FITS) {
	fileHandler = (RtdFITSComp *)new RtdFITSComp(interp_, instname_, fileName, "\0", fileSize_);
    }
    else if (fileFormat_ == FITS_CUBE) {
	fileHandler = (RtdFITSCube *)new RtdFITSCube(interp_, instname_, fileName, "\0", fileSize_);
    }
    else {
	return error("Unknown file format specified");
    }

    if (fileHandler->status() == TCL_ERROR) {
	sprintf(msg, "Unable to open file %s", fileName);
	return error(msg);
    }

    // Attach to the current camera to start receiving images, if not already.
    if (!attached_) {
    	if (rtdAttachImageEvt(eventHndl_, camera_, NULL) != RTD_OK) {
	    return error("Error attaching camera to server");
	}
    }
    attached_ = 1;

    // Set up a file handler to detect the incoming image events.
    Tk_CreateFileHandler(eventHndl_->socket, TK_READABLE, fileEventProc, 
			 (ClientData)this);

    return TCL_OK;
}

/*
 * Stop method for the recorder tool. This detaches from the camera, closes
 * the file handler object, and removes the Tk file handler.
 *
 * Usage:
 *	$rtdrecorder/$rtdplayback stop
 *
 * Arguments:
 *	int argc, char *argv[] - argument list, unused
 *
 * Return value:
 *	TCL_OK
 */
int RtdRecorder::stop(int argc, char *argv[])
{
    rtdIMAGE_INFO imageInfo;		// Temp image information structure

    if (RtdRPTool::init() == TCL_ERROR)
	return TCL_ERROR;

    // Detach from the camera.
    if (attached_) {
	// Delete the Tk file handler.
	Tk_DeleteFileHandler(eventHndl_->socket);

	attached_ = 0;
	rtdDetachImageEvt(eventHndl_, camera_, NULL);


	// Get any outstanding images from the queue and process their semaphores.
	if (rtdRecvImageInfo(eventHndl_, &imageInfo, 0, NULL) != RTD_ERROR) {
	    rtdShmServicePacket(&imageInfo);
	}
    }

    // Delete the file handling object.
    if (fileHandler) {
	delete(fileHandler);
	fileHandler = (RtdRPFile *)NULL;
	Mem_RPTcleanup();
    }

    return TCL_OK;
}

/*
 * Method to set the current maximum file size or the file format type 
 * in the recorder.
 *
 * Usage:
 *	$rtdrecorder file size <size>
 *	$rtdrecorder file format <format>
 *	where <size> is the maximum file size in Mb and <format> is 0 for
 *	compressed FITS, 1 for FITS cube.
 *
 * Arguments:
 *	int argc, char *argv[] - argument list:
 *		argv[0] - size/format
 *		argv[1] - argument to argv[0]
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdRecorder::file(int argc, char *argv[])
{
    if (strcmp(argv[0], "size") == 0) {
	fileSize_ = atof(argv[1]);
    }
    else if (strcmp(argv[0], "format") == 0) {
	fileFormat_ = (enum fileFormat)atoi(argv[1]);
    }
    else {
	return error("Bad argument for $rtdrecorder file");
    }

    return TCL_OK;
}

/*
 * Method to set the current camera.
 *
 * Usage:
 *	$rtdrecorder camera <camera>
 *	where <camera> is the camera name to receive images from.
 *
 * Arguments:
 *	int argc, char *argv[] - argument list
 *		argv[0] - camera name
 *
 * Return value:
 *	TCL_OK
 */
int RtdRecorder::camera(int argc, char *argv[])
{
    strncpy(camera_, argv[0], 32);

    return TCL_OK;
}

/*
 * Method to set the subimage facility of the recorder tool.
 *
 * Usage:
 *	$rtdrecorder subimage <on> ?x? ?y? ?width? ?height?
 *	where <on> is set to 'on' if the subimage sampling is to be turned on,
 *	and 'off' if turned off. x, y, width, and height are specified if the
 *	facility is being turned on. Note that (x,y) are image coordinates of
 *	the top left corner - FITS data is written from the bottom left and
 *	so the y coordinate must be transformed in this routine.
 *
 * Arguments:
 *	int argc, char *argv, argument list, see above.
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdRecorder::subimage(int argc, char *argv[])
{
    // Check the first argument.
    if (strcmp(argv[0], "on") == 0) {
	subImage_ = 1;
    }
    else if (strcmp(argv[0], "off") == 0) {
	subImage_ = 0;
	return TCL_OK;
    }
    else {
	return error("Bad first argument to subimage subcommand");
    }

    // Get the subsequent arguments for when the sampling is enabled.
    x0_ = atoi(argv[1]);
    y0_ = atoi(argv[2]);
    width_ = atoi(argv[3]);
    height_ = atoi(argv[4]);

    y0_ -= height_;

    return TCL_OK;
}

/* - End of RtdRecorder method definitions -				     */
/*===========================================================================*/
/* - Start of RtdPlayback method definitions -				     */

/*
 * This is called when the playback object is instantiated from the Tcl code.
 * This simply creates an RtdPlayback object instance.
 *
 * Arguments:
 *	Tcl_Interp *interp - pointer to interpreter structure
 *	char *name - instance name
 *	int argc, char *argv[] - argument list, unused
 *	Tk_ImageType *typePtr - Tk image type
 *	Tk_ImageMaster - Tk image master
 *	ClientData *clientDataPtr - client data
 *
 *	For more information, see Tk_CreateImageType(3).
 *
 * Return value:
 *	TCL_OK
 */
int RtdPlayback::CreateImage(
    Tcl_Interp *interp,         // Interpreter for application containing image.
    char *name,                 // Name to use for image.
    int argc,                   // Number of arguments.
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    Tcl_Obj *CONST objv[],      // Argument objects for options (not including image name or type)
#else
    char **argv,                // Argument strings for options (not including image name or type)
#endif
    Tk_ImageType *typePtr,      // Pointer to our type record (not used). 
    Tk_ImageMaster master,
    ClientData *clientDataPtr)
{
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
    // just generate an argv from the objv argument
    char* argv[64];  // there shouldn't be more than a few options...
    for(int i = 0; i < argc; i++)
        argv[i] = Tcl_GetString(objv[i]);
    argv[argc] = NULL;
#endif

    RtdPlayback *im = new RtdPlayback(interp, name, argc, argv, master);

    return TCL_OK;
}

/*
 * RtdRecorder contructor. Initialises the recorder properties.
 *
 * Arguments:
 *	As for RtdRPTool - arguments are just passed on to baseclass.
 */
RtdPlayback::RtdPlayback(Tcl_Interp *interp, char *instname, int argc, char **argv, Tk_ImageMaster master) :
    RtdRPTool(interp, instname, argc, argv, master),
    direction_(1),
    playSpeed_(SPEED_SLOW),
    timer_((Tcl_TimerToken)-1),
    spool_(0)
{}

/*
 * Method to create a file handler from an existing file. This method calls
 * the makeFileObject static method of the RtdRPFile class to create the
 * file handler from the file name, and initialises the file pointer and
 * image counter appropriately.
 *
 * Arguments:
 *	char *err - error message to return to caller
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdPlayback::makeFileHandler(char *err)
{
    // First create the right type of file object for the file name.
    fileHandler = RtdRPFile::makeFileObject(interp_, instname_, fileName, err);

    // Test that the file object has been created successfully.
    if (fileHandler == NULL) {
	return TCL_ERROR;
    }
    if (fileHandler->status() == TCL_ERROR) {
	sprintf(err, "Unable to read file for playback");
	return TCL_ERROR;
    }
    // Create the shared memory areas from the file information
    if (fileHandler->getShm(RTD_SHMBUFFS, &shmInfo_) == TCL_ERROR) {
	sprintf(err, "Unable to allocate shared memory");
	return TCL_ERROR;
    }

    return TCL_OK;
}

/*
 * This method adds a timeout callback to reinvoke to send image routines
 * after a certain time. The timeout interval is governed by the setting
 * chosen by the user.
 *
 * Arguments:
 *	None
 *
 * Return value:
 *	None
 */
void RtdPlayback::makeTimeOut()
{
    double timePeriod;		// Time interval in msecs.

    // Determine time interval based on user requirements.
    switch(playSpeed_) {
    case SPEED_SLOW:
	timePeriod = SLOWSPEED;
	break;
    case SPEED_FAST:
	timePeriod = FASTSPEED;
	break;
    case SPEED_RT:
	// In this case, the period reflects the timestamp data recorded
	// when the images were recorded.
	timePeriod = fileHandler->getTimeIncrement(direction_);
	break;
    default:
	// Unknown speed type.
	fprintf(stderr, "Error: unknown replay speed type");
	timePeriod = SLOWSPEED;
	break;
    }

    // Invoke callback.
    timer_ = Tcl_CreateTimerHandler((int)timePeriod, sendEventProc, this);
}

/*
 * Static method called from the timer callback for sending images to the
 * server daemon. This just passes control to the main send routine,
 * sendImage().
 *
 * Arguments:
 *	ClientData - Tcl/Tk client data.
 *
 * Return value:
 *	None.
 */
void RtdPlayback::sendEventProc(ClientData clientData)
{
    RtdPlayback *thisPtr = (RtdPlayback *)clientData;
    thisPtr->sendImage(1);
}

/*
 * Protected method to retrieve image data from the file handler and send
 * it to the RTD server daemon.
 *
 * Arguments:
 *	int reinvoke - if true, this sets up a timeout to reinvoke the routine
 *		after the user defined time period.
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdPlayback::sendImage(int reinvoke)
{
    int index;		// Index of shm buffer filled with data.

    if (RtdRPTool::init() == TCL_ERROR)
	return TCL_ERROR;

    // Before doing anything, check if we have reached the end of the file
    // and we want to stop here.

    if (!cycleMode_) {
	if (direction_ && (fileHandler->imageCounter() == fileHandler->numFileImages()) ||
	    (!direction_ && fileHandler->imageCounter() == 1)) {
	    if (fileHandler->numFileImages() > 1) {
		fileHandler->update_count();
		return TCL_OK;
	    }
	    else
		reinvoke = 0;
	}
    }
    // Get the image data into a shared memory buffer.
    if (direction_) {
	index = fileHandler->getNextImage(&shmInfo_);
    }
    else {
	index = fileHandler->getPrevImage(&shmInfo_);
    }
    if (index == -1) {
	return TCL_OK;     // maybe shm is locked
    }
    rtdIMAGE_INFO imageInfo;	// Image information for send.
	
    // Create a image information structure.
    memset(&imageInfo, '\0', sizeof(rtdIMAGE_INFO));
    imageInfo.frameX  = 0;
    imageInfo.frameY  = 0;
    imageInfo.frameId = 0;
    imageInfo.xPixels = fileHandler->xPixels();
    imageInfo.yPixels = fileHandler->yPixels();
    imageInfo.dataType = fileHandler->dataType();
    
    // Fill in the remaining fields.
    rtdShmStruct(index, &imageInfo, &shmInfo_);
    // Send the shared memory off.
    if (rtdSendImageInfo(eventHndl_, &imageInfo, NULL) != RTD_OK)
	return TCL_ERROR;;

    // Add a timeout callback to reinvoke this routine, if required.
    if (reinvoke) {
	makeTimeOut();
    }

    return TCL_OK;
}

/*
 * Overridden TclCommand method for interfacing the C++ with the Tcl code.
 * This method cycles over the RtdRPToolSubCmds to search for the method
 * to call. If no method is found (which it should be) control is passed to
 * the baseclass method).
 *
 * Arguments:
 *	const char *name - name of the subcommand to call
 *	int len - length of the subcommand name
 *	int argc, char **argv - argument list for subcommand
 *
 * Return value:
 *	Return value from subcommand.
 */
int RtdPlayback::call(const char *name, int len, int argc, char *argv[])
{
    for (int i = 0; i < sizeof(Playsubcmds_) / sizeof(*Playsubcmds_); i++) {
	RtdPlaybackSubCmds *t = &Playsubcmds_[i];
	if (strcmp(t->name, name) == 0) {
	    if (check_args(name, argc, t->min_args, t->max_args) != TCL_OK) {
		return TCL_ERROR;
	    }
	    return (this->*t->fptr)(argc, argv);
	}
    }
    return RtdRPTool::call(name, strlen(name), argc, argv);
}

/*
 * Playback fast forward or reverse functions. We must ensure that we have a
 * fileHandler instance created before we start fast-forwarding, as we need
 * to know the size of the file, etc.
 *
 * Usage:
 *	$rtdplayback spool <direction>
 *	where <direction> is "ff" or "rewind"
 *
 * Arguments:
 *	int argc, char *argv[] - argument list, see above
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdPlayback::spool(int argc, char *argv[])
{
    char errMsg[64];	// Return error message.


    if (RtdRPTool::init() == TCL_ERROR)
	return TCL_ERROR;

    // Check that we have a file handler
    if (!fileHandler) {
	if (makeFileHandler(&errMsg[0]) != TCL_OK) {
	    return error(&errMsg[0]);
	}
    }

    if (strcmp(argv[0], "rewind") == 0) {
	stop(0, NULL);
	fileHandler->gotoImageCount(1);
	if (sendImage(0) == TCL_ERROR)
	    return error("Error sending initial image data segment");
	return TCL_OK;
    }
    else if (strcmp(argv[0], "ff") != 0) {
	return error("Bad argument for spool command");
    }

    if (spool_)
	return TCL_OK;

    // Flag that we are now doing a spool
    spool_ = 1;
    playSpeed_ = SPEED_FAST;

    if (sendImage(1) == TCL_ERROR) {
	return error("Error sending initial image data segment");
    }

    return TCL_OK;
}

/*
 * Playback play function. We must ensure that we have a fileHandler object
 * instance created before starting.
 *
 * Usage:
 *	$rtdplayback play
 *
 * Arguments:
 *	int argc, char *argv[] - argument list, unused
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdPlayback::play(int argc, char *argv[])
{
    char errMsg[64];		// Error message buffer


    if (RtdRPTool::init() == TCL_ERROR)
	return TCL_ERROR;

    if (!fileHandler) {
	if (makeFileHandler(&errMsg[0]) != TCL_OK) {
	    return error(&errMsg[0]);
	}
    }

    // If we have no timestamp information, we can't playback in real-time.
    if (!fileHandler->hasTimeInfo() && playSpeed_ == SPEED_RT) {
	playSpeed_ = SPEED_SLOW;
    }
    if (sendImage(1) == TCL_ERROR) {
	return error("Error sending initial image data segment");
    }

    return TCL_OK;
}

/*
 * Reset the playback tool - close the file handler.
 *
 * Usage:
 *	$rtdplayback reset
 *
 * Arguments:
 *	int argc, char *argv[] - argument list, unused
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdPlayback::reset(int argc, char *argv[])
{
    // Remove the file handler.
    if (fileHandler) {
	delete(fileHandler);
	fileHandler = (RtdRPFile *)NULL;
	Mem_RPTcleanup();
    }
    return TCL_OK;
}

/*
 * Set the playback object properties, i.e. playback direction, speed.
 *
 * Usage:
 *	$rtdplayback props speed <speed>
 *	$rtdplayback props direction <dir>
 *	where <speed> is the speed number as specified in the enum defined in
 *	the header file, and <dir> is 1 for forwards, 0 for reverse.
 *
 * Arguments:
 *	int argc, char *argv[] - argument list, see above
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdPlayback::props(int argc, char *argv[])
{
    // XXX allan: 10.4.98: increased size of errMsg from 64,
    // XXX Memory is not that expensive these days, and it avoids crashing...
    char errMsg[2*1024];	// Error message buffer 
    int direction,
	count,
	incr = 0;

    // Check that we have a file handler
    if (!fileHandler) {
	if (makeFileHandler(&errMsg[0]) != TCL_OK) {
	    return error(&errMsg[0]);
	}
    }

    if (strcmp(argv[0], "speed") == 0) {
	playSpeed_ = (enum playSpeed)atoi(argv[1]);
    }
    else if (strcmp(argv[0], "direction") == 0) {
	direction = atoi(argv[1]);
	if (direction) {
	    direction = 1;
	    incr = 1;
	}
	// after changing the direction goto the right file position
	if (direction != direction_) {
	    count = incr + fileHandler->imageCounter();
	    fileHandler->gotoImageCount(count);
	}
	direction_ = direction;
    }
    else {
	return error("Bad argument for setprop command");
    }

    return TCL_OK;
}

/*
 * Step the current playback image one frame in the current direction. If the
 * filehandler is not yet initialised, do this first.
 *
 * This routine is virtually identical to the play method, but is kept
 * distinct for simplicity.
 *
 * Usage:
 *	$rtdplayback step
 *
 * Arguments:
 *	int argc, char *argv[] - argument list, unused
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdPlayback::step(int argc, char *argv[])
{
    char errMsg[64];		// Error message buffer

    if (RtdRPTool::init() == TCL_ERROR)
	return TCL_ERROR;

    if (!fileHandler) {
	if (makeFileHandler(&errMsg[0]) != TCL_OK) {
	    return error(&errMsg[0]);
	}
    }

    // Send an image to the server without reinvoking the send routine after
    // a timeout.
    if (sendImage(0) == TCL_ERROR) {
	return error("Error sending initial image data segment");
    }

    return TCL_OK;
}

/*
 * Change the filename of the playback object.
 *
 * Usage:
 *	$rtdplayback filename <file>
 *
 * Arguments:
 *	int argc, char *argv[] - argument list,
 *	    argv[0] - file name
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdPlayback::filename(int argc, char *argv[])
{
    /*
     * Check to see if the file name has changed. If it has, then we want
     * to remove the current file handler and create a new one when this is
     * required. If the file name is the same, then nothing has changed so
     * we call the baseclass method alone.
     */
    if (strcmp(argv[0], fileName) != 0) {
	if (fileHandler) {
	    delete(fileHandler);
	    fileHandler = (RtdRPFile *)NULL;
	    // Remove the shared memory areas associated with this
	    Mem_RPTcleanup();
	}
    }

    // Baseclass method.
    return(RtdRPTool::filename(argc, argv));
}

/*
 * Stop the current playback.
 *
 * Usage:
 *	$rtdplayback stop
 *
 * Arguments:
 *	int argc, char *argv[] - argument list, not used
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdPlayback::stop(int argc, char *argv[])
{
    // If we are stopping a spool, then reset the spool_ flag
    spool_ = 0;

    // Remove the timer callback (if it exists) to prevent any more pictures
    // being displayed.
    Tcl_DeleteTimerHandler(timer_);
    timer_ = (Tcl_TimerToken)-1;

    return TCL_OK;
}

/*
 * Close the playback object.
 *
 * Usage:
 *	$rtdplayback close
 *
 * Arguments:
 *	int argc, char *argv[] - argument list
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdPlayback::close(int argc, char *argv[])
{
    // Call the baseclass destructor.
    RtdRPTool::cleanup();
    // Remove the shared memory/semaphore set.
    Mem_RPTcleanup();

    return TCL_OK;
}

/*
 * Return whether or not the file has timestamp information (for real-time
 * playback).
 *
 * Usage:
 *	$rtdplayback hastime
 *
 * Arguments:
 *	int argc, char *argv[] - argument list
 *
 * Return value:
 *	0 (file not timestamped), 1 (file timestamped).
 */
int RtdPlayback::hastime(int argc, char *argv[])
{
    char buf[2];	// Return buffer

    // Check file handler exists.
    if (!fileHandler) {
	return error("File handler is not instantiated");
    }

    // Return the result.
    sprintf(buf, "%d", fileHandler->hasTimeInfo());

    return set_result(buf);
}

/*
 * Goto to the image index specified by the argument.
 *
 * Usage:
 *      $rtdplayback gotoimage <imageindex>
 *
 * Arguments:
 *      int argc, char *argv[] - argument list
 *          - argv[0] - index of image to go to.
 *
 * Return value:
 *      TCL_OK / TCL_ERROR
 */
int RtdPlayback::gotoimage(int argc, char *argv[])
{
    int index;          // Image index to go to.

    // Check that there is a file handler.
    if (!fileHandler) {
        return TCL_OK;
    }

    // Check that the image index is in the right range.
    index = atoi(argv[0]);
    if (index < 0) {
        return error("Chosen index is out of range");
    }

    // Go to the appropriate index
    fileHandler->gotoImageCount(index);

    return TCL_OK;
}

void Mem_RPTcleanup() 
{
    // Remove the shared memory/semaphore set.
    rtdShmDelete(&shmInfo_);
}
