/*
 * E.S.O. - VLT project 
 * "@(#) $Id: RtdPerformanceTool.C,v 1.1.1.1 2006/01/12 16:38:53 abrighto Exp $"
 *
 * RtdPerformanceTool.C - member routines for class RtdPerformanceTool,
 *             manages manipulation of performance data
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * D.Hopkinson     21/11/96  Created
 * A.Brighton      08/12/97  added cast and fixed "new" expr for SunPro CC
 */

#include "RtdPerformanceTool.h"
#include "tcl.h"
#include <cstdlib>
#include <cstring>

/*
 * Const array of process type identifiers for performance testing.
 * e.g. PKT = packet transfer, TCL = tcl code interpretation.
 */
const char *rtdEventDesc[] = {"PKT_", "MEM_", "GEN_", "TCL_", "X_"};
const int numIdentifiers = 5;

/*
 * Constructor function - just initialise the timeIndex.
 *
 * Arguments:
 *	None
 */
RtdPerformanceTool::RtdPerformanceTool()
: timeIndex(0),
verbose_(1),
active_(0)
{
}

/*
 * Destructor function.
 */
RtdPerformanceTool::~RtdPerformanceTool()
{
}

/*
 * Add a timestamp to the performance test data.  This routine is called through
 * a buffer method in RtdCamera when timestamping RtdImage processes, which
 * prevents timestamping when the tool is not active.  When called directly
 * from an RtdCamera process, this routine ensures that the tool becomes
 * active.
 *
 * Arguments:
 *	char *evDesc - event description
 *
 * Return value:
 *	None
 */
void RtdPerformanceTool::timeStamp(char *evDesc)
{
    // Activate the tool.
    active_ = 1;

    // Timestamp.
    if (timeIndex < RTD_NUMSHM * RTD_NUMTMSTMPS) {
        gettimeofday(&timeStamps[timeIndex], NULL);
        sprintf(evDescs[timeIndex++], "%s", evDesc);
    }
    else {
	fprintf(stderr, "Warning: too many timestamps produced\n");
    }
}

/*
 * Add a timestamp to the performance test data, as above. This routine is
 * called when the image information is first received by the RTD, and
 * examines the image information structure to extract the timestamp.
 *
 * Arguments:
 *	rtdIMAGE_INFO *imageInfo - the image information structure
 *
 * Return value:
 *	None
 */
void RtdPerformanceTool::timeStamp(const rtdIMAGE_INFO *imageInfo)
{
    // activate the tool.
    active_ = 1;

    // Timestamp
    if (timeIndex < RTD_NUMSHM * RTD_NUMTMSTMPS) {
	sprintf(evDescs[timeIndex], "SEND");
	memcpy(&timeStamps[timeIndex++], (void *)&imageInfo->timeStamp, 
	    sizeof(struct timeval));
    }
    else {
	fprintf(stderr, "Warning: too many timestamps produced\n");
    } 
}

/*
 * Dump the performance data (held in the timeStamps array) to file.
 *
 * Arguments:
 *	rtdIMAGE_INFO *imageInfo - image information structure, used for
 *	    output of image data to the browse file.
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdPerformanceTool::dumpPerformanceData(const rtdIMAGE_INFO *imageData)
{
    int i, j;			// Index coutners.
    FILE *fStr;			// Input/output file stream.
    struct fLine *timeLines;	// Ditto from display process.
    reportRecord *sumData;	// Summary data array.
    int numReceived;		// Number of images received.
    int correctOrdering;	// Is the ordering of events correct?

    // Deactivate the tool.
    active_ = 0;

    /*
     * Copy all the performance data into the timeLines structure
     * prior to sorting.
     */
    timeLines = new fLine[timeIndex];
    for (j = 0; j < timeIndex; j++) {
	timeLines[j].timeStamp = timeStamps[j].tv_sec + 
	    (timeStamps[j].tv_usec / 1000000.);
	sprintf(timeLines[j].descr, "%s", evDescs[j]);
    }
    // End the array with the END event.
    sprintf(timeLines[timeIndex - 1].descr, "END");

    // Now sort the resulting line array in order of increasing time.
    qsort(&timeLines[0], timeIndex, sizeof(struct fLine), 
	  (int(*)(const void*, const void*))sortTime);  // added cast to avoid compiler error message

    // Generate the summary data from the array just created.
    generateSummary(timeLines, timeIndex, sumData, numReceived, 
	correctOrdering);

    // Open the output browse file for writing.
    if ((fStr = fopen(RTD_PTEST_FNAME, "w")) == NULL) {
	if (verbose_) {
	    fprintf(stderr, "Unable to open performance test browse file\n");
	    return TCL_ERROR;
	}
    }

    // Write the data.  Start with general image information.
    fprintf(fStr, "**** Performance Test Results ****\n");
    fprintf(fStr, "\nImage width/pixels\t%d", imageData->xPixels);
    fprintf(fStr, "\nImage height/pixels\t%d", imageData->yPixels);
    fprintf(fStr, "\nImage bytes per pixel\t%d", imageData->bytePerPixel);
    fprintf(fStr, "\nTotal image size\t%ld", imageData->xPixels * 
	imageData->yPixels * imageData->bytePerPixel);
    fprintf(fStr, "\nNumber of sent images\t%d", RTD_NUMSHM);
    fprintf(fStr, "\nNumber of received images\t%d", numReceived);

    // Write the timestamp list.
    fprintf(fStr, "\n\n**** Timestamp list ****\n");
    for (j = 0; j < timeIndex; j++) {
	fprintf(fStr, "%lf\t%s\n", timeLines[j].timeStamp, timeLines[j].descr);
    }

    // Add some general reporting information here.
    fprintf(fStr, "\n**** Summary results ****\n");
    for (j = 0; j < numIdentifiers; j++) {
	fprintf(fStr, "Process: %s\tInit_time: %6.4f\tOverall_time: %6.4f\n",
	    sumData[j].procDesc, sumData[j].initTime, sumData[j].overallTime);
    }
    fprintf(fStr, "Total processing time: %7.4f\n", getProcTime(sumData));

    delete(sumData);

    // Close the browse file and free memory.
    fclose(fStr);
    delete(timeLines);

    // Signal the end of the testing, and display the number of events received.
    printf("\n***** Performance Test Ended *****\n");
    if (!correctOrdering) {
	printf("\nImage client fell behind server");
    }
    else {
	printf("\nAll server events were processed immediately");
    }
    printf("\nNumber of image events skipped: %d\n", RTD_NUMSHM - numReceived);
    printf("Diagnostic output written to %s\n", RTD_PTEST_FNAME);

    timeIndex = 0;

    return TCL_OK;
}

/*
 * Generate the summary data from the timestamps.  This must be freed by the
 * caller.
 *
 * Arguments:
 *	struct fLine *data - pointer to event timestamp data from which the
 *	    summary is to be prepared.
 *	int numLines - the number of entries in the above array
 *	reportRecord* &summaryData - pointer to structure containing the output
 *	    summary data. Must be freed by caller.
 *	int numReceived - output, the number of image events received
 *	int correctOrdering - flag True if the ordering of the timestamps is
 *	    corrent (RTD did not fall behind in image processing).
 *
 * Return value:
 *	None
 */
void RtdPerformanceTool::generateSummary(const struct fLine *data, int numLines,
    reportRecord* &summaryData, int& numReceived, int& correctOrdering)
{
    float deltaTime = 0.;	// Time difference between sequential events.
    int i, k;			// Index counter.

    // Initialise the ordering and number received parameters
    correctOrdering = 1;
    numReceived = 0;

    summaryData = (reportRecord *)new reportRecord[numIdentifiers];

    // First check that the SEND events do not occur out of order.  Just check
    // that each send is followed by a PKT_ identifier.
    for (i = 0; i < numLines; i++) {

	// Count the number of received packets.
	if (strstr(data[i].descr, rtdEventDesc[0])) {
	    numReceived++;
	}

	if (strstr(data[i].descr, "SEND")) {
	    if (!strstr(data[i + 1].descr, "PKT")) {
		correctOrdering = 0;
	    }
	}
    }

    // Accumulate the data for each process identifier.
    for (i = 0; i < numIdentifiers; i++) {
	// Copy across the identifier for the process.
	strcpy(summaryData[i].procDesc, rtdEventDesc[i]);
	summaryData[i].initTime = 0.;
	summaryData[i].overallTime = 0.;

	for (int j = 1; j < numLines; j++) {
	    if (correctOrdering || strstr(rtdEventDesc[i], "PKT")) {
	        deltaTime = data[j].timeStamp - data[j - 1].timeStamp;
	    }
	    else {
		/*
		 * If the timestamps are out of order, then we require special
		 * processing. We ignore SEND events apart from the case of
		 * data transmission processes.
		 */
		for (k = j - 1; k > 0; k--) {
		    if (!strstr(data[k].descr, "SEND")) {
			deltaTime = data[j].timeStamp - data[k].timeStamp;
			break;
		    }
		}
	    }
	    if (strstr(data[j].descr, rtdEventDesc[i])) {
		// then add the time on to the total.
		summaryData[i].overallTime += deltaTime;
		if (strstr(data[j].descr, "INIT")) {
		    summaryData[i].initTime += deltaTime;
		}
	    }
	}
    }
}

/*
 * Get the overall total processing time for the image processing from 
 * the summary data.
 *
 * Arguments:
 *	reportRecord *data - pointer to summary data structure with the times
 *	    for each processing type.
 *
 * Return value:
 *	float, the total processing time for the image.
 */
float RtdPerformanceTool::getProcTime(const reportRecord *data)
{
    float totalProcTime = 0.;

    /*
     * Loop over each process type and add the total time for that processing
     * to the overall time for all processing.
     */
    for (int i = 0; i < numIdentifiers; i++) {
	totalProcTime += data[i].overallTime;
    }

    return totalProcTime;
}

/*
 * Routine required for qsort function call.  Orders a set of fLines's in
 * chronological order.
 *
 * Arguments:
 *	struct fLine *a, *b - pointers to two sample structures to be sorted
 *
 * Return value:
 *	-1, time (a) less than time (b)
 *	+1, time (b) less than time (a)
 */
int RtdPerformanceTool::sortTime(const struct fLine *a, const struct fLine *b) 
{
    if (a->timeStamp < b->timeStamp) {
	return -1;
    }
    else {
	return 1;
    }
}

