/*
 * E.S.O. - VLT project 
 *
 * RtdRPFile.C - member routines for class RtdRPFile, RtdFITSCube,
 *		 and RtdFITSComp.
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * D.Hopkinson     17/04/97  Created
 * P.Biereichel    23/07/97  Bug fixed. Revised
 */

#include "RtdRPFile.h"
#include <time.h>

/*
 * NULL constructor for RtdRPFile class.
 *
 * Arguments:
 *	None.
 */
RtdRPFile::RtdRPFile() :
  status_(TCL_OK),
  fPtr(NULL),
  imageCounter_(0),
  xPixels_(0),
  yPixels_(0),
  bytesPerPixel_(0),
  imageIndex_(0),
  fileSize_(0.),
  maxFileSize_(0.),
  numFileImages_(0),
  fileFull_(0),
  timeStamps_(NULL),
  hasTimeInfo_(0),
  shmSize_(0)
{
    // Put any RtdRPFile initialisation here.
}

/*
 * Constructor for RtdRPFile class. This opens a file for reading or writing.
 *
 * Arguments:
 *	char *fileName - name of the file to open
 *	char *accFlag - usual C access flag, e.g. r+.
 *	double maxFileSize - the maximum allowed file size (Mb)
 */
RtdRPFile::RtdRPFile(Tcl_Interp* interp, char* instname,
		     char *fileName, char *accFlag, double maxFileSize) :
    status_(TCL_OK),
    fPtr(NULL),
    imageCounter_(0),
    xPixels_(0),
    yPixels_(0),
    bytesPerPixel_(0),
    startIndex_(0),
    imageIndex_(0),
    fileSize_(0.),
    maxFileSize_(maxFileSize),
    numFileImages_(0),
    fileFull_(0),
    timeStamps_(NULL),
    hasTimeInfo_(0),
    instname_(instname),
    shmSize_(0),
    interp_(interp),
    fileName_(0)
{
    fileName_ = fileName;
    // Open the file in the required access mode.
    if (accFlag[0] != '\0') {
	if ((fPtr = fopen(fileName, accFlag)) == NULL) {
	    status_ = TCL_ERROR;
	}
    }
    else {
	// check that file is a regular file and not write protected
	struct stat   statbuf;
	if (stat(fileName, &statbuf) == 0) {
	    if (! S_ISREG(statbuf.st_mode) || ! (S_IWUSR & statbuf.st_mode)) {
		status_ = TCL_ERROR;
	    }
	}
    }
	    
}

/*
 * Destructor for RtdRPFile object. This just closes the file stream and does
 * some tidying up of memory.
 */
RtdRPFile::~RtdRPFile()
{
    cleanup();
}


/*
 * just closes the file stream and does some tidying up of memory.
 */
void RtdRPFile::cleanup()
{
    // Remove the timestamps array.
    delete(timeStamps_);
    timeStamps_ = NULL;

    // round off file size
    if (shmSize_ && (imageCounter_ != 0 || fileFull_)) {
	int n = fileFull_ ? numFileImages_ : imageCounter_;
	padFile(fPtr, shmSize_ * n);
    }

    // Close the file stream (this may take some time if the file is on a remote host)
    fclose(fPtr);
    fPtr = NULL;
    imageCounter_ = 0;
}


/*
 * This method takes the subimage coordinates and checks that they are
 * consistent with the current image information (it is possible, for example,
 * that the user may set up an edit window on an image which has different
 * properties to the images received from the camera).
 *
 * Arguments:
 *	rtdIMAGE_INFO - image information structure for image
 *	int& x0, y0 - coordinates of bottom left corner of subimage
 *	int& width, height - width and height of subimage
 *
 * Return value:
 *	None.
 */
void RtdRPFile::checkSubImage(rtdIMAGE_INFO *imageInfo, int& x, int& y,
  int& width, int& height)
{
    // First check the initial coordinates
    if (x < 0) x = 0;
    if (y < 0) y = 0;

    // Check the height and width
    if (width > imageInfo->xPixels) width = imageInfo->xPixels - 1;
    if (height > imageInfo->yPixels) height = imageInfo->yPixels - 1;

    // Finally, make sure the subimage fits into the main image
    if (x + width > imageInfo->xPixels) {
	x = imageInfo->xPixels - width - 1;
    }
    if (y + height > imageInfo->yPixels) {
	y = imageInfo->yPixels - height - 1;
    }
}

/*
 * Method to get the time increment between the image just read and the next
 * image (which the file pointer should be at the start of).
 *
 * Arguments:
 *	int direction - forwards through time (1), or backwards (0)
 *
 * Return value:
 *	double - the time interval (seconds)
 */
double RtdRPFile::getTimeIncrement(int direction)
{
    double increment;		// Required time increment for return.
    int tmpIndex;		// Temp index - index of last image read.

    if (!hasTimeInfo_) {
	// No timestamp information available. Return something.
	return 2000.;
    }

    /*
     * Strangely, the time increment required here is the same whether we
     * are going forwards through the file or backwards - the difference
     * between the previous index and the current index. This is because
     * when we are going backwards, the current index is already displayed.
     * This is not the case when going forwards.
     */
    // tmpIndex = (imageIndex_ - 1 < 0) ? numFileImages_ - 1 : imageIndex_ - 1;
    tmpIndex = (imageIndex_ - 1 < 0) ? imageIndex_ : imageIndex_ - 1;
    increment = (timeStamps_[imageIndex_] - timeStamps_[tmpIndex]) * 1000.;
    /*
     * The increment may be negative if we have reached the end of a file.
     * Alternatively, it may be zero if the above indices were the same (i.e.
     * only one imae in the cube).
     * In either case, just return the default for SLOW speed (or whatever).
     */
    if (increment < 0. || imageIndex_ == tmpIndex) {
	increment = 1000.;
    }

    return increment;
}

/*
 * Method to create and initialise a shared memory/semaphore set based on
 * the image information (that must have already been initialised).
 *
 * Arguments:
 *	int numShm - number of shared memory buffers to create
 *	rtdShm *shmInfo - structure to initialise
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdRPFile::getShm(int numShm, rtdShm *shmInfo)
{
    // Clear any outstanding allocation for the shmInfo
    memset(shmInfo, '\0', sizeof(rtdShm));

    // Check first that the image information is instantiated.
    if (xPixels_ == 0 || yPixels_ == 0 || bytesPerPixel_ == 0) {
	return TCL_ERROR;
    }

    return rtdShmCreate(numShm, shmInfo, xPixels_, yPixels_, dataType_);
}


/*
 * Update tcl variable for current image count and #of images
 */

void RtdRPFile::update_count()
{
    // Update recorder counter
    char buffer[64];
    int eof=0, bof=0;
    if (imageCounter_ != imageCounter__ || numFileImages_ != numFileImages__ || \
	imageCounter_ == numFileImages_ || imageCounter_ <=1) {
	if (imageCounter_ <=1)
	    bof = 1;
	if (imageCounter_ >= numFileImages_)
	    eof = 1;
	sprintf(buffer, "%d %d %d %d", imageCounter_, numFileImages_, bof, eof);
	imageCounter__  = imageCounter_;
	numFileImages__ = numFileImages_;
	Tcl_SetVar2(interp(), instname(), "COUNT", buffer, TCL_GLOBAL_ONLY);
#ifdef DEBUG
	printf("%d %d %d %d \n", imageCounter_, numFileImages_, bof, eof);
#endif
    }
}

/* Method to place the file pointer at the start of a certain image which is
 * indexed by the image count (i.e. the image count starts from zero at the
 * image which has the lowest timestamp, and this may be in the middle of
 * the file such that the index is non-zero). This method converts the image
 * count to an image index and calls a subclass method to actually place the
 * file pointer.
 *
 * Arguments:
 *	int count - the image count where the pointer is to be placed
 *
 * Return value:
 *	None
 */
void RtdRPFile::gotoImageCount(int count)
{
    // Set the image counter to count
    imageCounter_ = count;
    // Translate the count into an image index
    this->gotoImageIndex((count - 1 + startIndex_) % numFileImages_);
    update_count();
}

/*
 * Method to create an initialised FileObject given a file name. The determines
 * the type of file that is being loaded and creates the object that
 * corresponds to that file. It then places the file pointer at the correct
 * point in the file, ready for playing (or possibly recording in future).
 *
 * Arguments:
 *	char *fileName - name of the file to create object from
 *
 * Return value:
 *	RtdRPFile * - pointer to new object instance
 *	char *err - error message to return
 */
RtdRPFile *RtdRPFile::makeFileObject(Tcl_Interp* interp, char* instname, char *fileName, char *err)
{
    FILE *file;		// Temporary file pointer.
    char buffer[16];	// Temporary buffer.

    // Initialise the instance to return.
    RtdRPFile *newInstance = (RtdRPFile *)NULL;

    // Open the file for reading, to check the type of file that we have.
    if ((file = fopen(fileName, "r")) == NULL) {
	sprintf(err, "Unable to open file %s", fileName);
	return NULL;
    }

    /*
     * To check the file type, get the first ten characters. In a compressed
     * file type, these will be "compressed" (possibly...)
     */
    fgets(buffer, sizeof(buffer), file);
    fclose(file);

    if (strncmp(buffer, "compressed", 10) == 0) {
	newInstance = (RtdFITSComp *)new RtdFITSComp(interp, instname, fileName, "r", 5.);
    }
    else {
	newInstance = (RtdFITSCube *)new RtdFITSCube(interp, instname, fileName, "r", 5.);
    }

    // Set up the file pointer and timestamp array.
    if (newInstance) {
    	if (newInstance->open(err) == TCL_ERROR) {
	    delete(newInstance);
	    newInstance = (RtdRPFile *)NULL;
	}
    }

    return newInstance;
}

/*
 * round off the file size to the next FITS block.
 * (size is the current size)
 */
void RtdRPFile::padFile(FILE* f, int size) 
{
    int rest = (size + FITSBLOCK) % FITSBLOCK;
    if (rest) {
	fseek(f, 0, SEEK_END);
	while (rest < FITSBLOCK) {
	    fputc(' ', f);
	    rest++;
	}
    }
}


/* - End of RtdRPFile method definitions -				    */
/*==========================================================================*/
/* - Start of RtdFITSCube method definitions -				    */

/* Private methods first. */

/*
 * This method writes the header to the FITS cube file. It calculates the number
 * of FITS data files that will fit into the cube before the maximum file size
 * is reached, and returns this.
 *
 * Arguments:
 *	const rtdIMAGE_INFO *imageInfo - image information structure
 *	int subImage - flag true if subimage is to be taken
 *	int x0, y0, width, height - defines subimage bounding box.
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdFITSCube::writeFITSHeader(const rtdIMAGE_INFO *imageInfo, int subImage, 
int width, int height)
{
    char  buf[81],		// Temporary buffers
	buf2[20];

    // Get the size of the image segments.
    int dataSize = (subImage ? (width * height * abs(imageInfo->dataType / 8)) :
      (imageInfo->xPixels * imageInfo->yPixels * abs(imageInfo->dataType / 8)));

    // Calculate the number of images that there will be in the file.
    numFileImages_ = (int)(maxFileSize_ * 1024. * 1024./ (double)dataSize);

    // Get the number of keyword lines in the FITS header (including END).
    int size = FITSBLOCK/80;
    
    sprintf(buf, "%-8s= %s", "SIMPLE", "T");
    fprintf(fPtr, "%-80s", buf); size--;
    int bitpix = imageInfo->dataType;
    if (bitpix == -16)
	bitpix = 16;
    sprintf(buf, "%-8s= %d", "BITPIX", bitpix);
    fprintf(fPtr, "%-80s", buf); size--;
    sprintf(buf, "%-8s= %d", "NAXIS", 3);
    fprintf(fPtr, "%-80s", buf); size--;
    sprintf(buf, "%-8s= %d", "NAXIS1", (subImage ? width : imageInfo->xPixels));
    fprintf(fPtr, "%-80s", buf); size--;
    sprintf(buf, "%-8s= %d", "NAXIS2", (subImage ? height: imageInfo->yPixels));
    fprintf(fPtr, "%-80s", buf); size--;
    if (imageInfo->dataType == -16) {
	sprintf(buf, "%-8s= %f", "BZERO", (double)32768.0);
	fprintf(fPtr, "%-80s", buf); size--;
	sprintf(buf, "%-8s= %f", "BSCALE", (double)1.0);
	fprintf(fPtr, "%-80s", buf); size--;
    }

    // add the date to the cube header.
    time_t clock = time(0);
    strftime(buf2, sizeof(buf2), "%d/%m/%y", localtime(&clock));
    sprintf(buf, "%-8s= \'%s\'", "DATE", buf2);
    fprintf(fPtr, "%-80s", buf); size--;
    
    /*
     * The following blank cards will be used to provide image timestamp
     * information when the file is closed.
     */
    int i = 0;
    while (size > 1) {
	sprintf(buf, "BLANK%02d", i++);
    	fprintf(fPtr, "%-80s", buf); size--;
    }

    /*
     * The number of timestamps available can be smaller than the number
     * of images
     */
    i--;
    if (i*3 < numFileImages_)
	numFileImages_ = i*3;
    /*
     * Finally, put on the end card. There will be no need for any padding
     * before the start of the image data because we should have exactly
     * filled the FITS block.
     */
    fprintf(fPtr, "%-80s", "END");

    // Increment the file count.
    fileSize_ += FITSBLOCK / (1024. * 1024.);
    return TCL_OK;
}

/*
 * FITSCube object destructor. This goes through the FITS header, changing
 * BLANK fields into timestamp information - this is in the form:
 * COMMENT = "TS: <timestamp> <timestamp> ..." (3 timestamps per line).
 * It also adds a NAXIS3 field.
 *
 * The baseclass destructor closes the file down.
 */
RtdFITSCube::~RtdFITSCube()
{
    char buffer[81];	// Buffer for reading data from file.
    char buf2[64];	// General purpose buffer.
    int filePos = 0;	// Position of file pointer from beginning of stream.
    int i;		// Index counter
    int found = 0;	// Flag: found BLANK card

    if (imageCounter_ == 0 && !fileFull_) {
	// Image file is empty anyway
	return;
    }
    if (!shmSize_) {
	// Call baseclass destructor. Nothing was recorded.
	// RtdRPFile::~RtdRPFile;  (allan: 8.12.97: this will be called automatically!)
	return;
    }

    // Rewind the file pointer to the beginning of the file.
    rewind(fPtr);

    // Loop over the file header, changing fields as detailed in the method 
    // header.
    do {
	fgets(&buffer[0], 81, fPtr);
	if (feof(fPtr))
	    break;
	if (strncmp(buffer, "BLANK", 5) == 0) {
	    // Flag that we have found the card
	    found = 1;

	    // Replace this blank field with NAXIS3 count.
	    fseek(fPtr, filePos, SEEK_SET);
    	    sprintf(buf2, "%-8s= %d", "NAXIS3", 
		(fileFull_ ? numFileImages_ : imageCounter_));
	    sprintf(buffer, "%-80s", buf2);
	    fputs(buffer, fPtr);
	    break;
	}
	filePos += 80;
    }
    while(strncmp(buffer, "END", 3) != 0 && !feof(fPtr));
    // If we didn't find the blank card, then either we've got big problems
    // or we were playing back a FITS file from some other tool. Either way,
    // get out of here.
    if (!found) {
	// Call baseclass destructor.
	// RtdRPFile::~RtdRPFile; (allan: 8.12.97: this will be called automatically!)
	return;
    }

    /*
     * Now add the timestamp information below this line.
     */
    char TSBuf[64];
    sprintf(TSBuf, "\0");
    char buf3[32];
    for (i = 0; i < (fileFull_ ? numFileImages_ : imageCounter_); i++) {
	sprintf(buf3, "%.3lf ", timeStamps_[i]);
	// Create concatenated timestamp string.
	strcat(TSBuf, buf3);
	if (!((i + 1) % 3)) {
	    // Every third string, dump to the FITS header.
	    sprintf(buffer, "%-8s= \"TS: %s/\"", "COMMENT", TSBuf);
	    fprintf(fPtr, "%-80s", buffer);
	    sprintf(TSBuf, "\0");
	}
    }
    // If there are any timestamps left, dump these to the FITS header.
    if (TSBuf[0] != '\0') {
	sprintf(buffer, "%-8s= \"TS: %s/\"", "COMMENT", TSBuf);
	fprintf(fPtr, "%-80s", buffer);
    }

    // Tidy up
    // RtdRPFile::~RtdRPFile; (allan: 8.12.97: this will be called automatically!)
}

/*
 * Given a file pointer in an existing file, this method intialises the
 * timestamp information so that the images can be played back in real time.
 *
 * Arguments:
 *	char *errMsg - error message to return if not all OK
 *
 * Return value:
 *	TCL_OK / TCL_ERROR.
 */
int RtdFITSCube::open(char *errMsg)
{
    char buffer[81];		// Temporary buffer.
    char *ptr, *vptr;		// Temporary pointers.
    int bzero=0, bscale=0;
    int foundNAXIS3 = 0,
	foundNAXIS1 = 0,
	foundNAXIS2 = 0,
	foundBITPIX = 0;
    int i = 0;			// Index counter.

    /*
     * Loop over the header information to get to the timestamp information.
     * If the FITS cube was produced with this application, this will be
     * in COMMENT headers of the form COMMENT = "TS: <> <> <>" in groups
     * of three timestamps per header. If this information can not be found,
     * flag that it is missing and let the driver objects sort it out.
     *
     * Get the NAXIS3 info first for the timestamp array size. Also, at this
     * point, get the image information to determine the size of the images
     * int the file.
     */
    rewind(fPtr);

    do {
	fgets(buffer, sizeof(buffer), fPtr);
	if (feof(fPtr))
	    break;
	ptr = strtok(buffer, "=");
	if (strncmp(buffer, "NAXIS1", 6) == 0) {
	    foundNAXIS1 = 1;
	    vptr = strtok(NULL, "/");
	    xPixels_ = atoi(vptr);
	}
	if (strncmp(buffer, "NAXIS2", 6) == 0) {
	    foundNAXIS2 = 1;
	    vptr = strtok(NULL, "/");
	    yPixels_ = atoi(vptr);
	}
	if (strncmp(buffer, "BITPIX", 6) == 0) {
	    foundBITPIX = 1;
	    vptr = strtok(NULL, "/");
	    dataType_ = atoi(vptr);
	    bytesPerPixel_ = abs(dataType_) / 8;
	}
	if (strncmp(buffer, "NAXIS3", 6) == 0) {
	    // This gives the number of images, and so is also the number of
	    // timestamps in the array.
	    foundNAXIS3 = 1;
	    ptr = strtok(NULL, "/");
	    numFileImages_ = atoi(ptr);
	}
	if (strncmp(buffer, "BSCALE", 6) == 0) {
	    ptr = strtok(NULL, "/");
	    bscale = atoi(ptr);
	}
	if (strncmp(buffer, "BZERO", 5) == 0) {
	    ptr = strtok(NULL, "/");
	    bzero = atoi(ptr);
	}
    }
    while(strncmp(buffer, "END", 3) != 0 && !feof(fPtr));

    // Set BITPIX=-16
    if (bscale == 1 && bzero == 32768 && dataType_ == 16)
	dataType_ = -16;

    if (feof(fPtr) || !foundBITPIX || !foundNAXIS1 || !foundNAXIS2) {
	sprintf(errMsg, "Not a FITS file");
	return TCL_ERROR;
    }

    if (!foundNAXIS3) {
	numFileImages_ = 1;
    }
    timeStamps_ = (double *)new double[numFileImages_];

    // Now get the timestamp comments.
    rewind(fPtr);

    do {
	fgets(buffer, sizeof(buffer), fPtr);
	if (strncmp(buffer, "COMMENT = \"TS:", 14) == 0) {
	    hasTimeInfo_ = 1;
	    // Get the timestamp information by splitting up the line into
	    // constituent timestamps.
	    ptr = &buffer[15];
	    while (1) {
	    	vptr = strchr(ptr, ' ');
	    	*vptr = '\0';
		timeStamps_[i++] = atof(ptr);
		ptr = vptr + 1;
		if (*ptr == '/') {
		    break;
		}
	    }
	}
    }
    while(strncmp(buffer, "END", 3) != 0);

    // Get the file pointer at the end of the FITS block
    FITSHeaderSize_ = ftell(fPtr);

    // Index i should be the same as the number of file images, or something
    // is terribly wrong.
    if (hasTimeInfo_ && i != numFileImages_) {
	sprintf(errMsg, "Inconsistency between timestamp and image number");
	return TCL_ERROR;
    }

    /*
     * Now we set up the image counter and image indices by finding the lowest
     * of the timestamps (this may not be the first in the array if the image
     * was cycled when it was recorded).
     */
    imageCounter_ = 0;
    startIndex_ = 0;
    if (hasTimeInfo_) {
        double min = timeStamps_[0];
        for (i = 0; i < numFileImages_; i++) {
	    if (timeStamps_[i] < min) {
	        min = timeStamps_[i];
	        startIndex_ = i;
	    }
        }
    }
    gotoImageIndex(startIndex_);

    update_count();
    return TCL_OK;
}

/*
 * This routine sets the file pointer to the start of the image indexed by
 * the method argument.
 *
 * Argument:
 *	int index - index of the image to set the file pointer to
 *
 * Return value:
 *	None.
 */
void RtdFITSCube::gotoImageIndex(int index)
{
    long int offset = (long)((int)((FITSHeaderSize_ - 1) / FITSBLOCK) + 1) 
      * FITSBLOCK;

    // Go to the start of the images (after the header).
    fseek(fPtr, offset, SEEK_SET);

    // Set the image index to the required index.
    imageIndex_ = index;

    // Now go to the start of the required index.
    fseek(fPtr, (imageIndex_ * xPixels_ * yPixels_ * bytesPerPixel_), SEEK_CUR);
}

/*
 * Given an image information structure, this method adds the image data to the
 * FITS cube. If it is the first time round, FITS header information is also
 * created.
 *
 * Arguments:
 *	rtdIMAGE_INFO *imageInfo - image information structure
 *	int subimage - true if sub image is to be taken
 *	int x0, y0 - coordinates of lower left corner of subimage
 *	int width, height - dimensions if subimage
 *
 * Return value:
 *	TCL_OK / TCL_ERROR
 */
int RtdFITSCube::addImage(rtdIMAGE_INFO *imageInfo, int subImage, int x0,
  int y0, int width, int height)
{
    int shmSize;

    // Get the byte size of the shared memory.
    shmSize = imageInfo->xPixels * imageInfo->yPixels * 
	abs(imageInfo->dataType) / 8;
    shmSize_ = shmSize;
    if (shmSize <= 0)
	return TCL_ERROR;

    // Get a pointer to the start of the shared memory area.
    Mem shmData = Mem(shmSize, imageInfo->shmId, 0, 0, imageInfo->shmNum, 
	imageInfo->semId);
    // Check that the memory was attached successfully
    if (shmData.ptr() == NULL)
	return TCL_ERROR;

    /*
     * If this is the first image to add (file size is still 0), then add a FITS
     * header to start with. We also initialise an array to deal with all the
     * timestamps.
     */
    if (!imageCounter_ && !fileFull_) {
	if ((fPtr = fopen(fileName_, "w+")) == NULL) {
	    return TCL_ERROR;
	}
	writeFITSHeader(imageInfo, subImage, width, height);
	timeStamps_ = (double *)new double[numFileImages_]; 
    }

    // The timestamp can be given by the timestamp from the image information.
    timeStamps_[imageCounter_] = (double)imageInfo->timeStamp.tv_sec + (double)
	imageInfo->timeStamp.tv_usec / 1000000.;


    /*
     * If subimaging is not enabled, then just copy across the data into the
     * file from the shared memory. Otherwise, do a selective copy.
     */
    int bitpix = imageInfo->dataType;
    if (!subImage) {
	if (bitpix == -16) {
	    // unsigned short needs to be converted
	    unsigned short *pu = (unsigned short *)shmData.ptr();
	    int i = shmSize / 2;
	    short *ps_new = new short[i];
	    short *ps = ps_new;
	    if (ps_new == 0) {
		fprintf(stderr, "Not enough memory\n");
		return TCL_ERROR;
	    }
	    int nn;
	    while (i--) {
		nn = (int)(*pu++) - 32768;
		*ps++ = (unsigned int) nn;
	    }
	    fwrite((char*)ps_new, shmSize, 1, fPtr);
	    delete ps_new;
	}
	else {
	    fwrite(shmData.ptr(), shmSize, 1, fPtr);     // Copy entire buffer
	}
	if (!fileFull_) {
	    fileSize_ += shmSize / (1024. * 1024.);
	}
    }
    else {
    	// Get a pointer to the shared memory buffer.
    	char *srcPtr = (char *)shmData.ptr();
	int bpp = abs(imageInfo->dataType) / 8;
	checkSubImage(imageInfo, x0, y0, width, height);

    	// Offset of start of required buffer.
    	srcPtr += bpp * ((imageInfo->xPixels * y0) + x0);

    	// Copy across all lines to the file stream.
    	for (int j = 0; j < height; j++) {
	    if (bitpix == -16) {
		// unsigned short needs to be converted
		unsigned short *pu = (unsigned short *)srcPtr;
		int i = (width * bpp) / 2;
		short *ps_new = new short[i];
		short *ps = ps_new;
		if (ps_new == 0) {
		    fprintf(stderr, "Not enough memory\n");
		    return TCL_ERROR;
		}
		int nn;
		while (i--) {
		    nn = (int)(*pu++) - 32768;
		    *ps++ = (unsigned int) nn;
		}
		fwrite((char*)ps_new, shmSize, 1, fPtr);
		delete ps_new;
	    }
	    else {
		fwrite(srcPtr, width * bpp, 1, fPtr);
		srcPtr += bpp * (imageInfo->xPixels);
	    }
    	}
	if (!fileFull_) {
	    fileSize_ += (bpp * height * width) / (1024. * 1024.);
	}
    }

    /*
     * Increment the image counter. If we have reached the maximum size of the
     * file, reset the counter to one and reposition the file pointer to the
     * beginning of the image data segments.
     */
    if (++imageCounter_ == numFileImages_) {
	update_count();
	fseek(fPtr, FITSBLOCK, SEEK_SET);
	fileFull_ = 1;
	imageCounter_ = 0;
    }
    update_count();
}

/*
 * Method to get the next image from the file and put it into shared memory.
 * If this is the last image in the file, reset the file pointer to the
 * start of the images.
 *
 * Arguments:
 *	rtdShm *shmInfo - shared memory/semaphore structure to fill
 *
 * Return value:
 *	int index - the index of the shared memory buffer in the multi-buffered
 *		    scheme that was filled.
 */
int RtdFITSCube::getNextImage(rtdShm *shmInfo)
{
    char *tmpBuf;		// Temporary data buffer.
    int retIndex = -1;		// Return index.
    int imageSize;		// Size of image data.
    static int cnt = 0;         // Shared memory counter

    // Calculate the size of the image data.
    imageSize = xPixels_ * yPixels_ * bytesPerPixel_;
    // Allocate a temporary buffer for the image data.
    tmpBuf = new char[imageSize];

    // Copy into the shared memory.
    fread(tmpBuf, imageSize, 1, fPtr);
    // convert BITPIX=-16 data
    if (dataType_ == -16) {
	int nn = imageSize / 2;
	unsigned short *us = (unsigned short *) tmpBuf;
	for (int i=0; i < nn; i++)
	    *us++ += 32768;
    }

    // Fill up a buffer in the shared memory array by calling a CCD
    // convenience routine.
    retIndex = rtdShmFillNext(cnt, tmpBuf, shmInfo);
    if (retIndex < 0) {
	delete(tmpBuf);
	return -1;
    }
    cnt = retIndex;

    // Tidy up local memory.
    delete(tmpBuf);

    // Alter imageCounter_ and imageIndex_ as required.
    if (++imageIndex_ >= numFileImages_) {
	imageIndex_ = 0;
	gotoImageIndex(imageIndex_);
    }
    if (imageIndex_ > startIndex_) {
	imageCounter_ = imageIndex_ - startIndex_;
    }
    else {
	imageCounter_ = numFileImages_ - startIndex_ + imageIndex_;
    }	
    update_count();
    return retIndex;
}

/*
 * Method to get the previous image from the file and put it into shared memory.
 * If this is the first image in the file, reset the file pointer to the
 * end of the images.
 *
 * Arguments:
 *	rtdShm *shmInfo - shared memory/semaphore structure to fill
 *
 * Return value:
 *	int index - the index of the shared memory buffer in the multi-buffered
 *		    scheme that was filled.
 */
int RtdFITSCube::getPrevImage(rtdShm *shmInfo)
{
    char *tmpBuf;		// Temporary data buffer.
    int retIndex = -1;		// Return index.
    int imageSize;		// Size of image data.
    static int cnt = 0;         // Shared memory counter

    // Calculate the size of the image data.
    imageSize = xPixels_ * yPixels_ * bytesPerPixel_;

    // Allocate a temporary buffer for the image data.
    tmpBuf = new char[imageSize];

    // Rewind the file pointer to just before the required image.
    if (--imageIndex_ < 0) {
	imageIndex_ = numFileImages_ - 1;
    }
    gotoImageIndex(imageIndex_);	

    // Copy into the shared memory.
    fread(tmpBuf, imageSize, 1, fPtr);

    // convert BITPIX=-16 data
    if (dataType_ == -16) {
	int nn = imageSize / 2;
	unsigned short *us = (unsigned short *) tmpBuf;
	for (int i=0; i < nn; i++)
	    *us++ += 32768;
    }

    // Fill up a buffer in the shared memory array by calling a CCD
    // convenience routine.
    retIndex = rtdShmFillNext(cnt, tmpBuf, shmInfo);
    if (retIndex < 0) {
	delete(tmpBuf);
	return -1;
    }
    cnt = retIndex;

    // Tidy up local memory.
    delete(tmpBuf);

    // After this, the file pointer should be repositioned to the start of the
    // block of image data that it has just sent.
    gotoImageIndex(imageIndex_);

    /*
     * The image counter should represent the number of the image that is 
     * presently under display. As we have gone backwards to get this, the
     * image number is one greater than the imageIndex (with an extra
     * allowance for the fact that the start of the images is at startIndex_).
     */
    if (imageIndex_ >= startIndex_) {
	imageCounter_ = imageIndex_ - startIndex_ + 1;
    }
    else if (imageIndex_ < startIndex_) {
	imageCounter_ = numFileImages_ - startIndex_ + imageIndex_ + 1;
    }
    update_count();

    return retIndex;
}
