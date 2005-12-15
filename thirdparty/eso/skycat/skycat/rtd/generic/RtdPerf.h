// -*-c++-*-
#ifndef _RtdPerf_h_
#define _RtdPerf_h_

/*
 * E.S.O. - VLT project 
 *
 * RtdPerf.h - class definitions for managing the performance data.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * pbiereic        01/03/01  Created
 */

#include <cstdio>
#include <sys/time.h>
#include <cstring>
#include <tk.h>

#include "RtdUtils.h"

class RtdPerf {

public:
    // Constructor
    RtdPerf(Tcl_Interp* interp);

    // Destructor
    ~RtdPerf();

    // Add delta time to 'timevar'
    void   timeInc(double *timevar);
 
    // Start a new cycle
    void   newCycle();

    // End a cycle and set the Tcl global variables
    void   endCycle();

    // Reset the time variables and Tcl global variables
    void   reset();

    // Switch performance test on
    void   on() {maybeOn_ = 1; on_ = 0;}
    void   on(int set) {on_ = set;}

    // Switch performance test on
    void   off() {maybeOn_ = 0; on_ = 0;}

    // Set verbose and debug flag
    void   verbose(int set) {verbose_ = set;}
    void   debug(int set) {debug_ = set;}

    // Set the instance name of the RtdImage (used for global Tcl variables)
    void   name(char *nam) {strcpy(name_, nam);}

    // -- short cuts --

    void  GENtime() {timeInc(&GENtime_);}
    void  TCLtime() {timeInc(&TCLtime_);}
    void  Xtime() {timeInc(&Xtime_);}

protected:
    RtdDebugLog *dbl_;    // debug log object
    Tcl_Interp* interp_;  // Tcl interp (for file events, error handling)
    int    on_;           // Flag: test is switched on/off
    int    verbose_;      // Verbose flag
    int    debug_;        // Debug flag
    int    maybeOn_;      // Switch test on when a new cycle start
    double imageCount_;   // Image counter
    double GENtime_;      // Time spent in general code processing within a image event cycle
    double TCLtime_;      // Time spent in Tcl code
    double Xtime_;        // Time spent for X updates
    double FREQtime_;     // Frequency of image event cycle
    double lastTimeStamp_;// Last time stamp
    double startTime_;    // Start time of cycle
    // Accumulated times
    double accGENtime_;
    double accTCLtime_;
    double accXtime_;
    double accFREQtime_;
    char   name_[100];
    char   buffer_[2048];

    // -- short cuts --

    char* name() {return(name_);}
    int   isOn() {return on_;}
    int   maybeOn() {return maybeOn_;}
};

#endif /* _RtdPerf_h_ */
