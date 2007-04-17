/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: RtdPerf.C,v 1.1.1.1 2006/01/12 16:39:21 abrighto Exp $" 
 *
 * RtdPerf.C - member functions for class RtdPerf
 *
 * RtdPerf is a class for managing the performance test
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * P. Biereichel   01/03/01  Created
 */

/************************************************************************
*   NAME
*   RtdPerf - class for managing the performance test
* 
*   SYNOPSIS
*   #include "RtdPerf.h"
*   RtdPerf::RtdPerf(Tcl_Interp* interp, int verbose, int debug)
*            Tcl_Interp* - pointer to a Tcl_Interp  structure
*            verbose     - verbose flag
*            debug       - debug flag
* 
*   DESCRIPTION
*
*   PUBLIC METHODS
*
*   void RtdPerf::timeInc(double *timevar)
*     Adds the delta timestamp to 'timevar'. Delta is the time between calls
*     to this method.
*
*   void RtdPerf::newCycle()
*     This method is called when a new image event cycle starts. It
*     initializes the time variables and the start timestamp.
*
*   void RtdPerf::endCycle()
*     This method is called when a new image event cycle ends. It
*     calculates the statistics on the time varibales and set the
*     global Tcl variables which are prefixed with 'PERF_'
*
*   void RtdPerf::reset()
*     Resets the time variables and Tcl variables
*
*   void RtdPerf::on()
*     Switches performance test on. It sets the protected member variable
*     maybeOn_ which indicates that the test will be started as soon as
*     there is a new image event.
*
*   FILES
*
*   ENVIRONMENT
*
*   CAUTIONS 
*
*   SEE ALSO
*    RtdImage(3)
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

#include "RtdPerf.h"
 
RtdPerf::RtdPerf(Tcl_Interp* interp)
    :   interp_(interp),
	on_(0),
	verbose_(0),
	debug_(0),
	maybeOn_(0),
	imageCount_(0.0),
	GENtime_(0.0),
	TCLtime_(0.0),
	Xtime_(0.0),
	FREQtime_(0.0),
	lastTimeStamp_(0.0),
	startTime_(0.0),
	accGENtime_(0.0),
	accTCLtime_(0.0),
	accXtime_(0.0),
	accFREQtime_(0.0)
{
    name("");
    dbl_ = new RtdDebugLog("RtdPerf", (int)0);
    reset();
}

RtdPerf::~RtdPerf()
{
    if (dbl_) {
	delete dbl_;
	dbl_ = NULL;
    }
}
/*
 * This routine is called as part of the interactive performance testing.
 * It evaluates the time between the present and the last timestamp, and
 * increments the variable pointed to be the argument.
 *
 * Arguments:
 *      double *timeval - address of performance indicator to be incremented.
 */
void RtdPerf::timeInc(double *timevar)
{

    if (! isOn())   // Return immediately if the testing is not activated
	return;

    struct timeval currentTime;
    double curTimeStamp;

    // Get the current time.
    gettimeofday(&currentTime, NULL);
    curTimeStamp = (double)currentTime.tv_sec + (double)currentTime.tv_usec / 1.0e+6;

    (*timevar) += curTimeStamp - lastTimeStamp_;  // Increment the appropriate variable
    lastTimeStamp_ = curTimeStamp;  // Update the last timestamp
}

/*
 * This routine is triggered by receipt of an image event,
 * and sets the timestamp information for the beginning of a cycle.
 */
void RtdPerf::newCycle()
{
    if (maybeOn())
	on(1);

    if ( ! isOn() ) 
	return;

    dbl_->setlog(verbose_ & debug_);
    dbl_->log("Starting image event cycle: %s\n", name());

    // Reset the performance variables.
    GENtime_   = 0.;
    TCLtime_   = 0.;
    Xtime_     = 0.;
    timeInc(&FREQtime_);  // set lastTimeStamp_ to current time
}

/*
 * This routine sets the variables of the performance test indicator form,
 * when it is realised.
 */
void RtdPerf::endCycle()
{
    if ( ! isOn() ) // performance testing is not activated
	return;
    char*  var = name();

    dbl_->log("Ended image event cycle: %s\n", name());

    imageCount_++;

    // Set the frequency Tcl variables. Needs at least two cycles
    if (imageCount_ > 1) {
	timeInc(&FREQtime_);  // set lastTimeStamp_ to current time
	FREQtime_ = lastTimeStamp_ - startTime_;
	accFREQtime_ += FREQtime_;
    
	sprintf(buffer_, "%.3f", 1.0 / FREQtime_);
	Tcl_SetVar2(interp_, var, "PERF_FREQ", buffer_, TCL_GLOBAL_ONLY);
	sprintf(buffer_, "%.3f", (imageCount_ - 1.0) / accFREQtime_);
	Tcl_SetVar2(interp_, var, "PERF_FREQ_AVE", buffer_, TCL_GLOBAL_ONLY);
    }
    startTime_ = lastTimeStamp_;

    // Set the total time for the image event.
    double aveXtime, aveGENtime, aveTCLtime; // Accumulated averages
    double TOTtime = GENtime_ + Xtime_ + TCLtime_;

    // Accumulate times (these are total times over all images).
    accGENtime_ += GENtime_;
    accTCLtime_ += TCLtime_;
    accXtime_   += Xtime_;

    // Average all the totals. Times in %
    double aveTOTtime = 
	(accGENtime_ + accTCLtime_ + accXtime_) / imageCount_;

    aveGENtime = accGENtime_ / imageCount_ * 100.0 / aveTOTtime;
    aveXtime   = accXtime_   / imageCount_ * 100.0 / aveTOTtime;
    aveTCLtime = accTCLtime_ / imageCount_ * 100.0 / aveTOTtime;

    GENtime_   = GENtime_  * 100.0 / TOTtime;
    Xtime_     = Xtime_    * 100.0 / TOTtime;
    TCLtime_   = TCLtime_  * 100.0 / TOTtime;

    // Set the Tcl variables
    sprintf(buffer_, "%.0f", imageCount_);
    Tcl_SetVar2(interp_, var, "PERF_COUNT", buffer_, TCL_GLOBAL_ONLY);

    sprintf(buffer_, "%6.3f", GENtime_);
    Tcl_SetVar2(interp_, var, "PERF_GEN", buffer_, TCL_GLOBAL_ONLY);
    sprintf(buffer_, "%6.3f", Xtime_);
    Tcl_SetVar2(interp_, var, "PERF_XFUNC", buffer_, TCL_GLOBAL_ONLY);
    sprintf(buffer_, "%6.3f", TCLtime_);
    Tcl_SetVar2(interp_, var, "PERF_TCL", buffer_, TCL_GLOBAL_ONLY);
    sprintf(buffer_, "%8.3f", TOTtime * 1.0e+3);  // in msec
    Tcl_SetVar2(interp_, var, "PERF_TOTAL", buffer_, TCL_GLOBAL_ONLY);

    // Do the same for the averaged amounts.
    sprintf(buffer_, "%6.3f", aveGENtime);
    Tcl_SetVar2(interp_, var, "PERF_GEN_AVE", buffer_, TCL_GLOBAL_ONLY);
    sprintf(buffer_, "%6.3f", aveXtime);
    Tcl_SetVar2(interp_, var, "PERF_XFUNC_AVE", buffer_, TCL_GLOBAL_ONLY);
    sprintf(buffer_, "%6.3f", aveTCLtime);
    Tcl_SetVar2(interp_, var, "PERF_TCL_AVE", buffer_, TCL_GLOBAL_ONLY);
    sprintf(buffer_, "%6.3f", aveTOTtime * 1.0e+3);  // in msec
    Tcl_SetVar2(interp_, var, "PERF_TOTAL_AVE", buffer_, TCL_GLOBAL_ONLY);
}

void RtdPerf::reset()
{
    char*  var = name();

    dbl_->log("Reset performance data: %s\n", name());

    on(0);
    imageCount_    = 0.0;
    lastTimeStamp_ = startTime_ = 0.0;
    GENtime_       = TCLtime_    = Xtime_    = FREQtime_    = 0.0;
    accGENtime_    = accTCLtime_ = accXtime_ = accFREQtime_ = 0.0;

    // Clear the Tcl variables.
    Tcl_SetVar2(interp_, var, "PERF_COUNT",     '\0', TCL_GLOBAL_ONLY);

    Tcl_SetVar2(interp_, var, "PERF_FREQ",      '\0', TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "PERF_GEN",       '\0', TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "PERF_XFUNC",     '\0', TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "PERF_TCL",       '\0', TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "PERF_TOTAL",     '\0', TCL_GLOBAL_ONLY);

    Tcl_SetVar2(interp_, var, "PERF_FREQ_AVE",  '\0', TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "PERF_GEN_AVE",   '\0', TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "PERF_XFUNC_AVE", '\0', TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "PERF_TCL_AVE",   '\0', TCL_GLOBAL_ONLY);
    Tcl_SetVar2(interp_, var, "PERF_TOTAL_AVE", '\0', TCL_GLOBAL_ONLY);
}

