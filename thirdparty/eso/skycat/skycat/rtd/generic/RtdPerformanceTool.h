// -*-c++-*-
#ifndef _RtdPerformanceTool_h_
#define _RtdPerformanceTool_h_

/*
 * E.S.O. - VLT project 
 *
 * RtdPerformanceTool.h - class definitions for managing the performance data.
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * D. Hopkinson    21/11/96  Created
 */

#include <stdio.h>

#include "rtdImageEvent.h"

#define RTD_NUMTMSTMPS 20	// The number of timestamps produced 
				// for every received image event.

struct fLine {		// Represent each line of the performance file dump.
    char descr[32];	// Line description.
    double timeStamp;	// Line timestamp.
};

// For the summary report.
typedef struct {
    char	      procDesc[32];  /* Process description line. */
    float	      initTime;	     /* Time for startup processes. */
    float	      overallTime;   /* Overall time over whole test/ */
} reportRecord;

class RtdPerformanceTool {
protected:
    // Display diagnostic messages.
    int verbose_;

    // Flag that a test is underway.  This is required so that timestamp calls
    // can be added to routines which are called when the client is quiescent
    // without filling up the timestamp array before the test starts.
    int active_;

    // Array of timestamps for eventual output to the performance data file.
    struct timeval timeStamps[RTD_NUMSHM * RTD_NUMTMSTMPS];

    // Same for event descriptions.
    char evDescs[RTD_NUMSHM * RTD_NUMTMSTMPS][32];

    // Index to these arrays.
    int timeIndex;

    // Generate data summary.
    void generateSummary(const struct fLine *data, int numLines, 
	reportRecord* &summaryData, int& numReceived, int& correctOrdering);

    // Get the total processing time.
    float getProcTime(const reportRecord *data);

    // The sort algorithm for qsort.
    static int sortTime(const struct fLine *a, const struct fLine *b);

public:
    // Constructor
    RtdPerformanceTool();

    // Destructor
    ~RtdPerformanceTool();

    // Add timestamp for send event (take time from image information)
    void timeStamp(const rtdIMAGE_INFO *imageInfo);

    // Add timestamp in performance test.
    void timeStamp(char *evDesc);

    // Write the performance data to the browse file.
    int dumpPerformanceData(const rtdIMAGE_INFO *imageData);

    // Return active status of performance test.
    int active() {return active_ ? 1 : 0;}
};

#endif /* _RtdPerformanceTool_h_ */
