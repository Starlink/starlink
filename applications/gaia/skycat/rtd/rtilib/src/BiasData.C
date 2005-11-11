/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: BiasData.C,v 1.2 2005/02/02 01:43:03 brighton Exp $" 
 *
 * BiasData.C - member functions for class BiasData
 *
 * BiasData is a class for mananging the bias data which can be
 * subtracted from the image data
 * 
 * who         when      what
 * --------  --------  ----------------------------------------
 * pbiereic  22/03/99  Created
 * pbiereic  16/10/02  Byte order for shm data
 */

#include <fcntl.h>
#include "BiasData.h"
#include "error.h"
#include "define.h"
 
BiasData::BiasData()
    :   biasImage_(NULL),
	idxBias_(0)
{
    for (int i=0; i< MAXBIAS; i++) {
	biasImages_[i] = NULL;
	files_[i][0] = '\0';
    }
    clear(0);
}

/*
 * return status: selected bias image !loaded, on or off
 */
int BiasData::status() 
{
    if (! biasImage_)
	return -1; 
    if (biasinfo_.on)
	return 1;
    return 0;
}

/*
 * switch bias subtraction on
 */
int BiasData::on()
{
    if (! biasImage_) {
	error("selected bias image not loaded");
	return 1;
    }
    biasinfo_.on = 1;
    return 0;
}

/*
 * switch bias subtraction off
 */
int BiasData::off()
{
    if (! biasinfo_.on)
	return 1;
    biasinfo_.on = 0;
    return 0;
}

/*
 * return filename or objectname of a bias image
 */
char* BiasData::file(int nr)
{
    if (nr < 0 || nr >=MAXBIAS)
	return '\0';
    return &files_[nr][0];
}

/*
 * load bias frame from file
 */
int BiasData::file(char *file, int nr)
{
    FitsIO* fits = NULL;
    double bzero, bitpix;
    
    // if not '-' (stdin) check that it is a file
    if (strcmp(file, "-") != 0) { 
	struct stat buf;
	if (stat(file, &buf) != 0 || S_ISREG(buf.st_mode) == 0) {
	    error("expected a file, but got: ", file);
	    return 1;
	}
    }
    int on = biasinfo_.on;
    clear(nr);

    // read the FITS image
    fits = FitsIO::read(file, O_RDONLY | S_IRUSR);
    if (!fits || fits->status() != 0)
	return 1;
    biasinfo_.usingNetBO = 1;

    /*
     *  ushort images are a special case since FitsIO writes them
     * as short image with BZERO=32768. For bias subtraction we need
     * the true type back.
     */
    fits->get("BITPIX", bitpix);
    fits->get("BZERO", bzero);

    if (bitpix == 16 && bzero == 32768)
    {
	int width, height;

	// copy the fits object

	fits->get("NAXIS1", width);
	fits->get("NAXIS2", height);

	int length = width * height * 2;

	Mem data(length, 0), header;
	if (data.status() != 0)
	    return 1;

	FitsIO* fits2 = new FitsIO(width, height, -16, 0.0, 1.0, header, data);
	if (!fits2 || fits2->status() != 0)
	    return 1;

	fits2->usingNetBO(BIGENDIAN);

	memcpy((char *)data.ptr(), (char *)fits->data().ptr(), length);
	delete fits;

	// convert short's to ushort's (native byte order)

	int i = width * height;
	unsigned short *pus = (unsigned short *)data.ptr(), us;
	short *pss = (short *)data.ptr(), ss;

	if (BIGENDIAN) {  // native byte order?
	    while (i--) {
		us = *pus++;
		*pss++ = (short)(us - 32768);
	    }
	}
	else {
	    while (i--) {
		us = *pus++;
		*pss++ = SWAP16(us) - 32768;
	    }
	}

	biasImages_[nr] = ImageData::makeImage(BIASNAME, fits2 , &biasinfo_, 0);

	// Remember the byte order (=native) for the image.
	biasinfo_.usingNetBO = BIGENDIAN;
    }
    else
    {
	biasImages_[nr] = ImageData::makeImage(BIASNAME, fits , &biasinfo_, 0);
    }

    if (! biasImages_[nr])
	return 1;

    if (nr == idxBias_) {
	biasinfo_.on = on;
	select(nr);
    }
    return 0;
}

/*
 * copy image to a bias frame
 */
int BiasData::copy(ImageData* image, char* filename, int nr)
{
    if (! image || nr < 0 || nr >=MAXBIAS)
	return 1;
    int on = biasinfo_.on;
    clear(nr);

    int length = image->data().length();
    Mem data(length, 0), header;
    if (data.status() != 0)
	return 1;
    FitsIO* fits = new FitsIO(image->width(), image->height(), image->dataType(), 
			      0.0, 1.0, header, data);
    if (!fits || fits->status() != 0)
	return 1;

    // Remember the byte order for the image.
    biasinfo_.usingNetBO = image->image().usingNetBO();
    fits->usingNetBO(biasinfo_.usingNetBO);

    biasImages_[nr] = ImageData::makeImage(BIASNAME, fits, &biasinfo_, 0);
    if (! biasImages_[nr])
	return 1;

    memcpy((char *)data.ptr(), (char *)image->data().ptr(), length);

    biasImages_[nr]->object(image->object());
    strcpy(&files_[nr][0], filename);
    if (nr == idxBias_) {
	biasinfo_.on = on;
	select(nr);
    }
    return 0;
}

/*
 * select a bias frame
 */
int BiasData::select(int nr)
{
    if (nr < 0 || nr >= MAXBIAS)
	return 1;

    idxBias_ = nr;
    if (biasImages_[nr] != NULL) {
	biasImage_  = biasImages_[nr];
	biasinfo_.ptr    = (char *)biasImage_->image().dataPtr();
	biasinfo_.width  = biasImage_->image().width();
	biasinfo_.height = biasImage_->image().height();
	biasinfo_.type   = biasImage_->dataType();
 	biasinfo_.usingNetBO = biasImage_->image().usingNetBO();
    }
    else
	clear(nr);

    return 0;
}

/*
 * return number for selected bias frame
 */
int BiasData::select()
{
    if (! biasImages_[idxBias_])
	return -1;
    return idxBias_;
}

/*
 * clear bias frame
 */
void BiasData::clear(int nr)
{
    if (nr < 0 || nr >=MAXBIAS)
	return;
    if (nr == idxBias_) {
	biasImage_   = NULL;
	biasinfo_.on      = 0;
	biasinfo_.ptr     = NULL;
	biasinfo_.width   = 0;
	biasinfo_.height  = 0;
	biasinfo_.type    = -1;
	biasinfo_.usingNetBO  = 0;
    }
    files_[nr][0] = '\0';
    if (biasImages_[nr]) {
	delete biasImages_[nr];
	biasImages_[nr] = NULL;
    }
}
