/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: RtdCoords.C,v 1.1.1.1 2006/01/12 16:39:20 abrighto Exp $"
*
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * pbiereic        01/03/01  created (copied from RtdImage.C)
*/

/************************************************************************
*   NAME
*   
*   RtdCoords.C - methods for Rtdimage coordinate conversion
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
*
*   This file contains all RtdImage member functions needed for
*   Rtdimage coordinate conversion
*
*   FILES
*
*   ENVIRONMENT
*
*   CAUTIONS 
*
*   SEE ALSO
*    RtdImage(3), RTD documentation
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

static char *rcsId="@(#) $Id: RtdCoords.C,v 1.1.1.1 2006/01/12 16:39:20 abrighto Exp $";
 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "RtdImage.h"

/* 
 * Return the equinox (or coordinate system) portion of the given 
 * coordinate type string. The input arg is of the form 
 * "wcs 2000" or "deg B1950", for example. The return value is the part
 * after "wcs " or "deg ", and defaults to "2000".
 */
static const char* getEquinoxStr(const char* coordType) {
    if (coordType 
        && strlen(coordType) > 4 
        && (strncmp(coordType, "deg ", 4) == 0 || strncmp(coordType, "wcs ", 4) == 0)) {
        return coordType+4;
    }
    return "2000";
}


/*
 * return the enum CoordinateType value given the string name
 */
RtdImage::CoordinateType RtdImage::getCoordinateType(const char* s)
{
    switch (*s) {
    case 'i':
	return CT_IMAGE;
    case 's':
	return CT_SCREEN;
    case 'w':
	return CT_WCS;
    case 'd':
	return CT_DEG;
    case 'c':
	int n = strlen(s);
	if (strncmp(s, "canvas", n) == 0)
	    return CT_CANVAS;
	if (strncmp(s, "chip", n) == 0)
	    return CT_CHIP;
    }
    error("unknown coord type: ", s);
    return CT_NONE;
}

/*
 * This method converts between different coordinate representations
 * where the coordinates are passed in string form.
 *
 * If dist_flag is non-zero, the coords are treated as a distance,
 * otherwise as a point.
 *
 * inx_buf and iny_buf hold the input coords (or distance) in the given
 * input coordinate system. If outx_buf and outy_buf are not NULL, they
 * hold the string form of the resulting coordinates in the target (out)
 * coordinate system.  The decimal result values are written to "x" and
 * "y".
 *
 * The return value is the Tcl status.
 *
 * The available coordinate systems are:
 *
 *     canvas     - canvas coordinates (canvas scroll area)
 *     screen     - canvas window coords (visible area)
 *     image      - basic image pixel coords (at mag 1, no transformations)
 *     chip       - detector chip coordinates
 *     wcs        - world coordinates in H:M:S
 *     deg        - world coordinates in degrees
 *
 * The world coordinate types: "wcs" and "deg" may also include the equinox,
 * for example: "wcs 1950" or "deg 2000". The default equinox is 2000.
 *
 * Note: the coordinate types may be abbrieviated.
 */
int RtdImage::convertCoordsStr(int dist_flag, const char* inx_buf, const char* iny_buf,
			       char* outx_buf, char* outy_buf, 
			       double& x, double& y,
			       const char* in_type, const char* out_type)
{
    char in = *in_type, out = *out_type;
    
    if (outx_buf)
	outx_buf[0] = '\0';
    if (outy_buf)
	outy_buf[0] = '\0';

    // get x and y as doubles
    if (in == 'w') { 
	// convert H:M:S to degrees
	WorldCoords wcs(inx_buf, iny_buf);
	if (wcs.status() != TCL_OK)
	    return TCL_ERROR;
	x = wcs.ra_deg();
	y = wcs.dec_deg();
    }
    else {
	if (Tcl_GetDouble(interp_, (char*)inx_buf, &x) != TCL_OK 
	    || Tcl_GetDouble(interp_, (char*)iny_buf, &y) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
 
    if (convertCoords(dist_flag, x, y, in_type, out_type) != TCL_OK)
	return TCL_ERROR;
    
    // format world coords in h:m:s if needed
    if (out == 'w' && outx_buf && outy_buf) { 
        // Note: don't need equinox here since we are only formatting the coords
	WorldCoords wcs(x, y);
	wcs.print(outx_buf, outy_buf);
    }
    else {
	if (outx_buf)
	    sprintf(outx_buf, "%.17g", x);
	if (outy_buf)
	    sprintf(outy_buf, "%.17g", y);
    }
    return TCL_OK;
}



/*
 * NOTE: This method should not be called. It is only for backward
 * compatibility.
 *
 * Convert inx,iny in the given coord system to outx,outy in the given
 * output coord system.  The coord types here are single char
 * abbrieviations:
 *
 * 	c = canvas coords
 * 	i = image coords
 * 	s = screen coords
 * 	w = world coords (in the equinox of the image)
 * 	d = world coords in degrees (in the equinox of the image)
 *
 * For backward compatibility, this method just calls the more general
 * version, which checks if the equinox needs to be changed.
 */
int RtdImage::convertCoords(int dist_flag, double& x, double& y, char in_type, char out_type)
{
    char in[2], out[2];
    in[0] = in_type;
    in[1] = '\0';
    out[0] = out_type;
    out[1] = '\0';
    return convertCoords(dist_flag, x, y, in, out);
}


/*
 * Convert inx,iny in the given coord system to outx,outy in the given
 * output coord system.  The coord types are the same as for
 * convertCoordsStr():
 *
 *     canvas     - canvas coordinates (canvas scroll area)
 *     screen     - canvas window coords (visible area)
 *     image      - basic image pixel coords (at mag 1, no transformations)
 *     chip       - detector chip or CCD coordinates
 *     wcs        - world coordinates in H:M:S
 *     deg        - world coordinates in degrees
 *
 * The world coordinate types: "wcs" and "deg" may also include the equinox,
 * for example: "wcs 1950" or "deg 2000". The default equinox is 2000.
 *
 * To convert coordinates we have to take some or all of the following
 * into account:
 *
 * 1. Transformations: rotate, scale, flipX,Y 
 * 	These handled by the ImageData methods doTrans() and
 * 	undoTrans().
 *
 * 2. Scrolling offsets in canvas
 * 	member vars: canvasX_ and canvasY
 *
 * 3. Origin of image in image coords
 * 	member vars: frameX_, frameY_
 *
 * 4. Image coordinates at origin:
 * 	member vars: xOffset_, yOffset_
 *
 * For "chip" coordinates, special fields in the real-time image events,
 * or for files, the FITS keywords: HIERARCH ESO DET WIN1 STRX and
 * STRY indicate the chip origin. If this information is not available,
 * then chip coordinates are the same as image coordinates.
 */
int RtdImage::convertCoords(int dist_flag, double& x, double& y, 
			    const char* in_type, const char* out_type)
{
    CoordinateType in = getCoordinateType(in_type),
	out = getCoordinateType(out_type);

    if (in == CT_NONE || out == CT_NONE)
	return TCL_ERROR;

    if (in == out) 
	return TCL_OK;

    char* msg = "unknown coordinate type";

    switch(in) {

    case CT_CANVAS:			// input is canvas coords
	switch(out) {
	case CT_SCREEN:		// convert canvas to screen coords
	    return canvasToScreenCoords(x, y, dist_flag);
	case CT_IMAGE:		// convert canvas to image coords
	    return canvasToImageCoords(x, y, dist_flag);
	case CT_CHIP:		// convert canvas to chip coords
	    return canvasToChipCoords(x, y, dist_flag);
	case CT_WCS:		// convert canvas to world coords
	case CT_DEG:
            if (canvasToWorldCoords(x, y, dist_flag) == TCL_OK) {
                changeEquinox(dist_flag, x, y, image_->wcs().equinoxStr(), getEquinoxStr(out_type));
                return TCL_OK;
            }
	    return TCL_ERROR;
	default:
	    return error(msg);
	}
	break;

    case CT_SCREEN:			// input is screen coords
	switch(out) {
	case CT_CANVAS:		// convert screen to canvas coords
	    return screenToCanvasCoords(x, y, dist_flag);
	case CT_IMAGE:		// convert screen to image coords
	    return screenToImageCoords(x, y, dist_flag);
	case CT_CHIP:		// convert screen to chip coords
	    return screenToChipCoords(x, y, dist_flag);
	case CT_WCS:		// convert screen to world coords
	case CT_DEG:
            if (screenToWorldCoords(x, y, dist_flag) == TCL_OK) {
                changeEquinox(dist_flag, x, y, image_->wcs().equinoxStr(), getEquinoxStr(out_type));
                return TCL_OK;
            }
	    return TCL_ERROR;
	default:
	    return error(msg);
	}
	break;

    case CT_IMAGE:			// input is image coords
	switch(out) {
	case CT_CANVAS:		// convert image to canvas coords
	    return imageToCanvasCoords(x, y, dist_flag);
	case CT_SCREEN:		// convert image to screen coords
	    return imageToScreenCoords(x, y, dist_flag);
	case CT_CHIP:		// convert image to chip coords
	    return imageToChipCoords(x, y, dist_flag);
	case CT_WCS:		// convert image to world coords
	case CT_DEG:
            if (imageToWorldCoords(x, y, dist_flag) == TCL_OK) {
                changeEquinox(dist_flag, x, y, image_->wcs().equinoxStr(), getEquinoxStr(out_type));
                return TCL_OK;
            }
	    return TCL_ERROR;
	default:
	    return error(msg);
	}
	break;

    case CT_CHIP:			// input is chip coords
	switch(out) {
	case CT_CANVAS:		// convert chip to canvas coords
	    return chipToCanvasCoords(x, y, dist_flag);
	case CT_SCREEN:		// convert chip to screen coords
	    return chipToScreenCoords(x, y, dist_flag);
	case CT_IMAGE:		// convert chip to image coords
	    return chipToImageCoords(x, y, dist_flag);
	case CT_WCS:		// convert chip to world coords
	case CT_DEG:
            if (chipToWorldCoords(x, y, dist_flag) == TCL_OK) {
                changeEquinox(dist_flag, x, y, image_->wcs().equinoxStr(), getEquinoxStr(out_type));
                return TCL_OK;
            }
	    return TCL_ERROR;
	default:
	    return error(msg);
	}
	break;

    case CT_DEG:			// input is world coords
    case CT_WCS:		
	// convert to image equinox
        changeEquinox(dist_flag, x, y, getEquinoxStr(in_type), image_->wcs().equinoxStr());
	
	switch(out) {
	case CT_CANVAS:		// convert world to canvas coords
	    return worldToCanvasCoords(x, y, dist_flag);
	case CT_SCREEN:		// convert world to screen coords
	    return worldToScreenCoords(x, y, dist_flag);
	case CT_IMAGE:		// convert world to image coords
	    return worldToImageCoords(x, y, dist_flag);
	case CT_CHIP:		// convert world to chip coords
	    return worldToChipCoords(x, y, dist_flag);
	case CT_WCS:		// no conversion
	case CT_DEG:
            changeEquinox(dist_flag, x, y, image_->wcs().equinoxStr(), getEquinoxStr(out_type));
	    return TCL_OK;
	default:
	    return error(msg);
	}
	break;
    }

    return TCL_OK;
}



/*
 * Utility method to change the equinox of ra and dec
 * from in_quinox to out_equinox, if dist_flag is 0.
 * Note that the equinox values may be a number, such as
 * 2000 or 1950, or a system name, such as "galactic", "ecliptic",
 * "J2000", "B1950".
 */
void RtdImage::changeEquinox(int dist_flag, double& ra, double& dec, 
                             const char* in_equinox, const char* out_equinox)
{
    if (!dist_flag) {
        if (in_equinox && out_equinox && !strcmp(in_equinox, out_equinox) == 0) {
            WorldCoords wcs(ra, dec, in_equinox);
            wcs.get(ra, dec, out_equinox);
        }
    }
}


/*
 * convert canvas to screen coords
 */
int RtdImage::canvasToScreenCoords(double& x, double& y, int dist_flag)
{
    if (!dist_flag) {
	x += canvasX_;
	y += canvasY_;
    }
    return TCL_OK;
}


/*
 * convert canvas to image coords
 *
 * Note: there are 2 cases to handle here: 
 * 1. a frame displaying an image at an offset with the origin at 0,0
 * 2. same as above, but with the origin at xOffset,yOffset
 * 
 * In the normal case, the offset and origin are both zero...
 */
int RtdImage::canvasToImageCoords(double& x, double& y, int dist_flag)
{
    if (!dist_flag) {
	double dx = xOffset_, dy = yOffset_;
	doTrans(dx, dy, 1);
	if (frameX_ == 0)
	    x += dx;
	if (frameY_ == 0)
	    y += dy;
    }
    undoTrans(x, y, dist_flag);
    return TCL_OK;
}


/*
 * convert canvas to world coords
 */
int RtdImage::canvasToWorldCoords(double& x, double& y, int dist_flag)
{
    return canvasToImageCoords(x, y, dist_flag) ||
	imageToWorldCoords(x, y, dist_flag);
}


int RtdImage::canvasToChipCoords(double& x, double& y, int dist_flag)
{
    return canvasToImageCoords(x, y, dist_flag)
	|| imageToChipCoords(x, y, dist_flag);
}

int RtdImage::chipToImageCoords(double& x, double& y, int dist_flag)
{
    if (! dist_flag) 
	image_->chipToImageCoords(x, y);
    return TCL_OK;
}


int RtdImage::chipToCanvasCoords(double& x, double& y, int dist_flag)
{
    return chipToImageCoords(x, y, dist_flag)
	|| imageToCanvasCoords(x, y, dist_flag);
}


int RtdImage::chipToScreenCoords(double& x, double& y, int dist_flag)
{
    return chipToImageCoords(x, y, dist_flag)
	|| imageToScreenCoords(x, y, dist_flag);
}


int RtdImage::chipToWorldCoords(double& x, double& y, int dist_flag)
{
    return chipToImageCoords(x, y, dist_flag)
	|| imageToWorldCoords(x, y, dist_flag);
}



/*
 * convert image to canvas coords
 */
int RtdImage::imageToCanvasCoords(double& x, double& y, int dist_flag)
{
    doTrans(x, y, dist_flag);
    if (!dist_flag) {
	double dx = xOffset_, dy = yOffset_;
	doTrans(dx, dy, 1);
	if (frameX_ == 0)
	    x -= dx;
	if (frameY_ == 0)
	    y -= dy;
    }
    return TCL_OK;
}


/*
 * convert image to screen coords
 */
int RtdImage::imageToScreenCoords(double& x, double& y, int dist_flag)
{
    return imageToCanvasCoords(x, y, dist_flag) ||
	canvasToScreenCoords(x, y, dist_flag);
}

/*
 * convert main image coordinates to raw image array coordinates.
 *
 * The resulting coordinates can be used to index into the raw image
 * array, which might begin at a logical offset other than 0,0. This is
 * only neeeded for rapid frames, since these display coordinates for the
 * main image, but have smaller data arrays that start at some x,y offset.
 */
int RtdImage::imageToRawImageCoords(double& x, double& y)
{
    if (rapidFrame_) {    // only rapid frames need this...
	// get offset for rapid frame in main image:
	// (only one of rapidX_,frameX_ etc. will be non-zero...)
	double dx = (rapidX_ + frameX_), dy = (rapidY_ + frameY_);

	if (image_->flipY())
	    y -= dy;
	else
	    y -= viewMaster_->image_->height() - image_->height() - dy;
	
	if (image_->flipX())
	    x -= viewMaster_->image_->width() - image_->width() - dx;
	else
	    x -= dx;
    }

    return 0;
}



/*
 * convert image to world coords in the equinox of the image
 */
int RtdImage::imageToWorldCoords(double& x, double& y, int dist_flag)
{
    double ra, dec;
    if (dist_flag) {
	if (image_->wcs().pix2wcsDist(x, y, ra, dec) != 0) 
	    return TCL_ERROR;
    }
    else {
	if (image_->wcs().pix2wcs(x, y, ra, dec) != 0) 
	    return TCL_ERROR;
    }

    x = ra;
    y = dec;

    return TCL_OK;
}


int RtdImage::imageToChipCoords(double& x, double& y, int dist_flag)
{
    if (! dist_flag)
	image_->imageToChipCoords(x, y);
    return TCL_OK;
}


/*
 * convert screen to Ximage coords (index in X Image)
 */
int RtdImage::screenToXImageCoords(double& x, double& y)
{
    if (displaymode() == 0) {
	x -= canvasX_;
	y -= canvasY_;
    } 
    else {
	// if the xImage smaller than the window, add the neg. scroll offset
	double fx = frameX_, fy = frameY_;
	doTrans(fx, fy, 1);

	if (canvasX_ > 0)
	    x += -canvasX_ - fx;
	else if (fx)
	    x -= (fx + canvasX_) ;

	if (canvasY_ > 0)
	    y += -canvasY_ - fy;
	else if (fy)
	    y -= (fy + canvasY_);
	
    }
    return TCL_OK;
}


int RtdImage::screenToChipCoords(double& x, double& y, int dist_flag)
{
    return screenToImageCoords(x, y, dist_flag)
	|| imageToChipCoords(x, y, dist_flag);
}

/*
 * convert screen to canvas coords
 */
int RtdImage::screenToCanvasCoords(double& x, double& y, int dist_flag)
{
    if (!dist_flag) {
	x -= canvasX_;
	y -= canvasY_;
    }
    return TCL_OK;
}


/*
 * convert screen to image coords
 */
int RtdImage::screenToImageCoords(double& x, double& y, int dist_flag)
{
    return screenToCanvasCoords(x, y, dist_flag) ||
	canvasToImageCoords(x, y, dist_flag);
}


/*
 * convert screen to world coords
 */
int RtdImage::screenToWorldCoords(double& x, double& y, int dist_flag)
{
    return screenToImageCoords(x, y, dist_flag) ||
	imageToWorldCoords(x, y, dist_flag);
}

/*
 * convert world to canvas coords
 */
int RtdImage::worldToCanvasCoords(double& x, double& y, int dist_flag)
{
    return worldToImageCoords(x, y, dist_flag) || 
	imageToCanvasCoords(x, y, dist_flag);
}


/*
 * convert world to screen coords
 */
int RtdImage::worldToScreenCoords(double& x, double& y, int dist_flag)
{
    return worldToCanvasCoords(x, y, dist_flag) ||
	canvasToScreenCoords(x, y, dist_flag);
}


/*
 * convert world to image coords
 */
int RtdImage::worldToImageCoords(double& x, double& y, int dist_flag)
{
    double ra = x, dec = y;
    if (dist_flag) {
	if (image_->wcs().wcs2pixDist(ra, dec, x, y) != 0) 
	    return TCL_ERROR;
    }
    else if (image_->wcs().wcs2pix(ra, dec, x, y) != 0) 
	return TCL_ERROR;

    return TCL_OK;
}


int RtdImage::worldToChipCoords(double& x, double& y, int dist_flag)
{
    return worldToImageCoords(x, y, dist_flag)
	|| imageToChipCoords(x, y, dist_flag);
}




/*
 * convert Ximage to image coords
 */
int RtdImage::xImageToImageCoords(double& x, double& y, int dist_flag)
{
    double dx = xOffset_, dy = yOffset_;
    doTrans(dx, dy, 1);
    x += dx;
    y += dy;
    undoTrans(x, y, dist_flag);
    return TCL_OK;
}



/* * convert x,y image coords to a distance
 */
void RtdImage::coordsToDist(double& x, double& y)
{
    // for image frame in same window, use master coords
    if (viewMaster_) {
	if (viewMaster_->tkwin_ == tkwin_) 
	    viewMaster_->coordsToDist(x, y);
	else 
	    image_->coordsToDist(x, y, viewMaster_->image_->width(), viewMaster_->image_->height());
    }
    else {
	image_->coordsToDist(x, y);
    }
}


/*
 * convert x,y distance to coordinates
 */
void RtdImage::distToCoords(double& x, double& y)
{
    // for image frame in same window, use master coords
    if (viewMaster_) {
	if (viewMaster_->tkwin_ == tkwin_) 
	    viewMaster_->distToCoords(x, y);
	else 
	    image_->distToCoords(x, y, viewMaster_->image_->width(), viewMaster_->image_->height());
    }
    else {
	image_->distToCoords(x, y);
    }
}
/*
 * apply the current transformations to the given coordinates
 * If distFlag is 1, x and y are treated as a distance, otherwise
 * they are treated as a point and flipped as needed.
 */
void RtdImage::doTrans(double& x, double& y, int distFlag)
{
    if (distFlag) {
	image_->doTrans(x, y, distFlag);
    }
    else if (viewMaster_) {
	if (viewMaster_->tkwin_ == tkwin_) { 
	    viewMaster_->doTrans(x, y, distFlag);
	}
	else {
	    image_->doTrans(x, y, distFlag, rapidX_, rapidY_, 
			    viewMaster_->image_->width(), viewMaster_->image_->height());
	}
    }
    else {
	image_->doTrans(x, y, distFlag, rapidX_, rapidY_);
    }
}


/* 
 * undo the current transformations on the given coordinates.
 * If distFlag is 1, x and y are treated as a distance, otherwise
 * they are treated as a point and flipped as needed.
 */
void RtdImage::undoTrans(double& x, double& y, int distFlag)
{
    if (distFlag) {
	image_->undoTrans(x, y, distFlag);
    }
    else if (viewMaster_) {
	if (viewMaster_->tkwin_ == tkwin_) { 
	    viewMaster_->undoTrans(x, y, distFlag);
	}
	else {
	    image_->undoTrans(x, y, distFlag, rapidX_, rapidY_, 
			      viewMaster_->image_->width(), viewMaster_->image_->height());
	}
    }
    else {
	image_->undoTrans(x, y, distFlag, rapidX_, rapidY_);
    }
}
