/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: RtdHDU.C,v 1.2 2006/03/26 13:22:33 abrighto Exp $"
*
* who          when      what
* --------     --------  ----------------------------------------------
* A. Brighton  05/10/95  Created
* pbiereic     01/03/01  copied from RtdImage.C
* pbiereic     14/08/01  added "hdu fits" subcommand
* P. W. Draper 20/05/07  pick out special columns in getHDU entry
*                        and examine the properties so that we make
*                        sure there is an id column (when the positional
*                        columns take up column 0) and that any sky
*                        coordinates are convered from radians to degrees.
*                        also delete the fits copy in hduCmdSet when
*                        a table is accessed, stops a leak.
*/

/************************************************************************
*   NAME
*
*   RtdHDU.C - methods for Rtdimage subcommand hduCmd()
*
*   SYNOPSIS
*
*
*   DESCRIPTION
*
*   This file contains all RtdImage member functions needed for
*   the Rtdimage subcommand hduCmd()
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

static char *rcsId="@(#) $Id: RtdHDU.C,v 1.2 2006/03/26 13:22:33 abrighto Exp $";

static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <sstream>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "RtdImage.h"

//  Trig conversion factors.
static const double pi_ = 3.14159265358979323846;
static const double r2d_ = 57.295779513082323;   // (180.0/pi_)

/*
 * Implement the hdu headings subcommand:
 *
 *      <path> hdu headings ?$number?
 *
 * See comments for hduCmd() for details.
 */
int RtdImage::hduCmdHeadings(int argc, char** argv, FitsIO* fits)
{
    int hdu = fits->getHDUNum();
    int saved_hdu = hdu;
    int numHDUs = fits->getNumHDUs();

    // check for the optional hdu arg, otherwise use current
    if (argc >= 2 && sscanf(argv[1], "%d", &hdu) == 1) {
	if (hdu != saved_hdu) {
	    if (hdu < 1 || hdu > numHDUs)
		return fmt_error("HDU number %d out of range (max %d)", hdu, numHDUs);
	    // switch to the given HDU, but restore the original before returning
	    if (fits->setHDU(hdu) != 0)
		return TCL_ERROR;
	}
    }

    // get the info and catch any errors
    int status = getHDUHeadings(fits);

    // restore the original HDU before returning
    if (hdu != saved_hdu && fits->setHDU(saved_hdu) != 0)
	status = TCL_ERROR;

    return status;
}


/*
 * Implement the hdu fits subcommand:
 *
 *      <path> hdu fits ?$number?
 *
 * See comments for hduCmd() for details.
 */
int RtdImage::hduCmdFits(int argc, char** argv, FitsIO* fits)
{
    int hdu = fits->getHDUNum();
    int saved_hdu = hdu;
    int numHDUs = fits->getNumHDUs();
    int status = TCL_OK;

    // check for the optional hdu arg, otherwise use current
    if (argc >= 2 && sscanf(argv[1], "%d", &hdu) == 1) {
	if (hdu != saved_hdu) {
	    if (hdu < 1 || hdu > numHDUs)
		return fmt_error("HDU number %d out of range (max %d)", hdu, numHDUs);
	    // switch to the given HDU, but restore the original before returning
	    if (fits->setHDU(hdu) != 0)
		return TCL_ERROR;
	}
    }

    // get the FITS header
    ostringstream os;
    fits->getFitsHeader(os);
    set_result(os.str().c_str());

    // restore the original HDU before returning
    if (hdu != saved_hdu && fits->setHDU(saved_hdu) != 0)
	status = TCL_ERROR;

    return status;
}

/*
 * This method is used to implement the "hdu headings" subcommand.
 * It returns the table headings of the current FITS table as a
 * Tcl list. An error is returned if the current HDU is not a FITS
 * table.
 */
int RtdImage::getHDUHeadings(FitsIO* fits)
{
    // return a list of table headings for the current FITS table
    char* type = (char*)fits->getHDUType();
    if (!type || *type == 'i')
	return error("HDU is not a FITS table");

    long nrows = 0;
    int ncols = 0;
    if (fits->getTableDims(nrows, ncols) != 0)
	return TCL_ERROR;
    reset_result();
    for(int col = 1; col <= ncols; col++) {
	char* s = fits->getTableHead(col);
	if (!s)
	    return TCL_ERROR;
	append_element(s);
    }
    return TCL_OK;
}

/*
 * Implement the "hdu type" subcommand:
 *
 *    <path> hdu type ?$number?
 *
 * See comments for hduCmd() for details.
 */
int RtdImage::hduCmdType(int argc, char** argv, FitsIO* fits)
{
    int hdu = fits->getHDUNum();
    int saved_hdu = hdu;
    int numHDUs = fits->getNumHDUs();

    // check for the optional hdu arg, otherwise use current
    if (argc >= 2 && sscanf(argv[1], "%d", &hdu) == 1) {
	if (hdu != saved_hdu) {
	    if (hdu < 1)
		return fmt_error("HDU number %d out of range (min 1)", hdu);
	    if (hdu > numHDUs)
		return fmt_error("HDU number %d out of range (max %d)", hdu, numHDUs);
	    // switch to the given HDU, but restore the original before returning
	    if (fits->setHDU(hdu) != 0)
		return TCL_ERROR;
	}
    }

    char* type = (char*)fits->getHDUType();
    int status = TCL_OK;
    if (type)
	set_result(fits->getHDUType());
    else
	status = TCL_ERROR;

    // restore the original HDU before returning
    if (hdu != saved_hdu && fits->setHDU(saved_hdu) != 0)
	status = TCL_ERROR;

    return status;
}

/*
 * Implement the hdu get subcommand:
 *
 *    <path> hdu get ?$number? ?$filename? ?$entry?
 *
 * See comments for hduCmd() for details.
 */
int RtdImage::hduCmdGet(int argc, char** argv, FitsIO* fits)
{
    int hdu = fits->getHDUNum();
    int saved_hdu = hdu;
    int numHDUs = fits->getNumHDUs();

    // check for the optional hdu arg, otherwise use current
    if (argc >= 2 && sscanf(argv[1], "%d", &hdu) == 1) {
	argc--;
	argv++;
	if (hdu != saved_hdu) {
	    if (hdu < 1 || hdu > numHDUs)
		return fmt_error("HDU number %d out of range (max %d)", hdu, numHDUs);
	    // switch to the given HDU, but restore the original before returning
	    if (fits->setHDU(hdu) != 0)
		return TCL_ERROR;
	}
    }

    // check for the filename arg
    char* filename = NULL;
    if (argc >= 2)
	filename = argv[1];

    // check for the entry arg
    char* entry = NULL;
    if (argc >= 3)
	entry = argv[2];

    // get the info and catch any errors
    int status = getHDU(fits, filename, entry);

    // restore the original HDU before returning
    if (hdu != saved_hdu && fits->setHDU(saved_hdu) != 0)
	status = TCL_ERROR;

    return status;
}


/*
 * This method implements the main body of the hdu get subcommand.
 * If filename arg is not NULL, the contents of the current HDU are
 * written to the file as a local catalog. Otherwise, the contents
 * of the current HDU are returned as a Tcl list or rows. If the
 * entry arg is not null and a filename was specified, it specifies
 * the catalog config entry for the file's header, in Tcl list format
 * {{key value} {key value} ...}.
 */
int RtdImage::getHDU(FitsIO* fits, const char* filename, const char* entry)
{
    const char* type = fits->getHDUType();
    if (!type || *type == 'i')
	return error("HDU is not a FITS table");

    long nrows = 0;
    int ncols = 0;
    if (fits->getTableDims(nrows, ncols) != 0)
	return TCL_ERROR;

    if (filename == NULL) {
	// return the contents of the table as a tcl list of rows
	reset_result();
	for(int row = 1; row <= nrows; row++) {
	    append_result(" {");
	    for(int col = 1; col <= ncols; col++) {
		char* s = fits->getTableValue(row, col);
		if (!s)
		    return TCL_ERROR;
		append_element(s);
	    }
	    append_result("}");
	}
	return TCL_OK;
    }

    // Otherwise write the contents of the table to a local catalog file
    ofstream os(filename);
    if (! os)
	return sys_error("can't open file: ", filename);

    // output the catalog header
    os << "QueryResult\n\n";

    //  PWD: locate special columns and their conversion factors (to degrees
    //  from radians).
    int idcol = -1;
    int racol = -1;
    double rascale = 1.0;
    int deccol = -1;
    double decscale = 1.0;

    // output the catalog config entry, if there is one
    if (entry != NULL) {
	os << "# Config entry\n";
	int nkeys = 0;
	char** keys = NULL;
        const char *p;
        const char *t;
        char buf[20];
	if (Tcl_SplitList(interp_, (char*)entry, &nkeys, &keys) != TCL_OK)
	    return TCL_ERROR;
	for(int i = 0; i < nkeys; i++) {
	    int n = 0;
	    char** v = NULL;
	    if (Tcl_SplitList(interp_, keys[i], &n, &v) != TCL_OK) {
		Tcl_Free((char *)keys);
		return TCL_ERROR;
	    }
	    if (n != 2) {
		Tcl_Free((char *)keys);
		Tcl_Free((char *)v);
		return fmt_error("Invalid catalog config entry: '%s': Expected {key value}", keys[i]);
	    }

            //  PWD: record ra_col, dec_col and id_col columns.
            p = v[0];
            while ( p && *p && *p == ' ' ) p++;  // Trim leading blanks.
            if ( strncasecmp( p, "ra_col", 6 ) == 0 ) {
                sscanf( v[1], "%d", &racol );

                //  If these are radians we need to convert to degrees.
                sprintf( buf, "TUNIT%d", racol + 1 );
                t = fits->get( buf );
                while ( t && *t && *t == ' ' ) t++;  // Trim leading blanks.
                if ( t && strncasecmp( t, "radian", 6 ) == 0 ) {
                    rascale = r2d_;
                }
            }
            else if ( strncasecmp( p, "dec_col", 7 ) == 0 ) {
                sscanf( v[1], "%d", &deccol );

                //  If these are radians we need to convert to degrees.
                sprintf( buf, "TUNIT%d", deccol + 1 );
                t = fits->get( buf );
                while ( t && *t && *t == ' ' ) t++;  // Trim leading blanks.
                if ( t && strncasecmp( t, "radian", 6 ) == 0 ) {
                    decscale = r2d_;
                }
            }
            else if ( strncasecmp( p, "id_col", 6 ) == 0 ) {
                sscanf( v[1], "%d", &idcol );
            }
	    os << v[0] << ": " << v[1] << endl;
	    Tcl_Free((char *)v);
	}
        
        //  No id_col, so we fake an index at the end.
        if ( idcol == -1 ) {
            os << "id_col: " << ncols << endl;
        }

	Tcl_Free((char *)keys);
	os << "# End config entry\n\n";
    }

    // output the column headings, if id_col is undefined then create a fake
    // column at the end (to keep ra_col & dec_col at current settings, which
    // are written out, could also be x_col and y_col to keep happy).
    int col;
    for(col = 1; col <= ncols; col++) {
	char* s = fits->getTableHead(col);
	if (!s)
	    return TCL_ERROR;
	os << s;
	if (col < ncols)
	    os << '\t';
    }
    if ( idcol == -1 && entry != NULL ) {
        os << '\t' << "ID";
    }

    os << "\n---\n";    // heading separator (dashed line)

    // output the data
    for(long row = 1; row <= nrows; row++) {
	for(col = 1; col <= ncols; col++) {
            char* s;
            if ( col == ( racol + 1 ) ) {
                s = fits->getTableValue(row, col, rascale);
            }
            else if ( col == ( deccol + 1 ) ) {
                s = fits->getTableValue(row, col, decscale);
            }
            else {
                s = fits->getTableValue(row, col, 1.0);
            }
	    if (!s)
		return TCL_ERROR;
	    os << s;
	    if (col < ncols)
		os << '\t';
	}
        if ( idcol == -1 && entry != NULL ) {
            os << '\t' << row;
        }
	os << endl;
    }
    return TCL_OK;
}

/*
 * Implement the HDU create subcommand:
 *
 *   <path> hdu create $type $extname $headings $tform $data
 *
 * see comments for hduCmd() for details.
 */
int RtdImage::hduCmdCreate(int argc, char** argv, FitsIO* fits)
{
    if (argc != 6) {
	return error("hdu create: wrong number of args");
    }
    char* type = argv[1];
    char* extname = argv[2];
    char* headings = argv[3];
    char* tform = argv[4];
    char* data = argv[5];

    // save the current HDU number and restore it later, since creating a
    // new table sets the HDU to the new table.
    int hdu = fits->getHDUNum();

    int asciiFlag = (strncmp(type, "ascii", 5) == 0);

    // These arrays hold the Tcl list info for the list arguments.
    // The memory is allocated and must be deleted before returning.
    char** colHeadings = NULL;
    char** formats = NULL;
    char** dataRows = NULL;
    char** dataCols = NULL;;

    int status = TCL_OK;
    // dummy loop used only for error handling
    while (1) {
	// get the headings array and number of columns
	int numCols = 0;
	if (Tcl_SplitList(interp_, headings, &numCols, &colHeadings) != TCL_OK) {
	    status = TCL_ERROR;
	    break;
	}

	// get the column formats array
	int numFormats = 0;
	if (Tcl_SplitList(interp_, tform, &numFormats, &formats) != TCL_OK) {
	    status = TCL_ERROR;
	    break;
	}

	if (numFormats != numCols) {
	    status = error("Wrong number of column formats");
	    break;
	}

	// get the table data array and number of rows
	int numRows = 0;
	if (Tcl_SplitList(interp_, data, &numRows, &dataRows) != TCL_OK) {
	    status = TCL_ERROR;
	    break;
	}

	// Create the FITS table
	if (fits->createTable(extname, numRows, numCols, (char**)colHeadings, (char**)formats, asciiFlag) != 0) {
	    status = TCL_ERROR;
	    break;
	}

	// insert the data (FITS rows and cols start at 1!)
	for(int row = 1; row <= numRows; row++) {
	    int n;
	    if (Tcl_SplitList(interp_, dataRows[row-1], &n, &dataCols) != TCL_OK) {
		status = TCL_ERROR;
		break;
	    }
	    if (n != numCols) {
		status = fmt_error("Wrong number of columns in row %d", row);
		break;
	    }
	    for(int col = 1; col <= numCols; col++) {
		if (fits->setTableValue(row, col, dataCols[col-1]) != 0) {
		    status = TCL_ERROR;
		    break;
		}
	    }
	    if (status != TCL_OK)
		break;
	    if (dataCols) {
		Tcl_Free((char *)dataCols);
		dataCols = NULL;
	    }
	}
	break;                  // once only
    }

    // Clean up and return the status
    if (colHeadings)
	Tcl_Free((char *)colHeadings);
    if (formats)
	Tcl_Free((char *)formats);
    if (dataRows)
	Tcl_Free((char *)dataRows);
    if (dataCols)
	Tcl_Free((char *)dataCols);

    // restore the original HDU
    fits->setHDU(hdu);

    return status;
}

/*
 * Implement the HDU delete subcommand:
 *
 *   <path> hdu delete $number
 *
 * see comments for hduCmd() for details.
 */
int RtdImage::hduCmdDelete(int argc, char** argv, FitsIO* fits)
{
    int hdu;

    if (Tcl_GetInt(interp_, argv[1], &hdu) != TCL_OK)
	return TCL_ERROR;

    int n = fits->getNumHDUs();
    if (hdu <= 1 || hdu > n)
	return fmt_error("HDU index %d out of range: must be > 1 and <= %d", hdu, n);

    if (fits->deleteHDU(hdu) != 0)
	return TCL_ERROR;

    return TCL_OK;
}

/*
 * Implement the hdu list subcommand:
 *
 *   <path> hdu list
 *
 * see comments for hduCmd() for details.
 */
int RtdImage::hduCmdList(int argc, char** argv, FitsIO* fits)
{
    // return a list of HDUs
    int numHDUs = fits->getNumHDUs();
    if (numHDUs <= 0)
	return TCL_OK;  // empty return list

        // save current HDU, then loop through all HDUs to get info
    int curHDU = fits->getHDUNum();
    ostringstream os;
    int status = 0;
    int count = 0;
    for (int i = 1; i <= numHDUs; i++) {
	if (fits->setHDU(i) != 0) {
	    status++;
	    break;
	}
	const char* type = fits->getHDUType();
	if (!type) {
	    status++;
	    break;
	}

	// get these keyword values and default to ""
	char extName[80], naxis[32], naxis1[32], naxis2[32], naxis3[32];
	char crpix1[32], crpix2[32];
	fits->get("EXTNAME", extName, sizeof(extName));
	fits->get("NAXIS", naxis, sizeof(naxis));
	fits->get("NAXIS1", naxis1, sizeof(naxis1));
	fits->get("NAXIS2", naxis2, sizeof(naxis2));
	fits->get("NAXIS3", naxis3, sizeof(naxis3));
	fits->get("CRPIX1", crpix1, sizeof(crpix1));
	fits->get("CRPIX2", crpix2, sizeof(crpix2));

	// Try avoiding long fractional strings
	if (strlen(crpix1) != 0 &&  strlen(crpix2) != 0) {
	    double dcrpix1, dcrpix2;
	    fits->get("CRPIX1", dcrpix1);
	    fits->get("CRPIX2", dcrpix2);
	    os << "{"
	       << i
	       << " " << type
	       << " {" << extName << "}"
	       << " {" << naxis << "}"
	       << " {" << naxis1 << "}"
	       << " {" << naxis2 << "}"
	       << " {" << naxis3 << "}"
	       << " {" << dcrpix1 << "}"
	       << " {" << dcrpix2 << "}"
	       << "} ";
	}
	else {
	    os << "{"
	       << i
	       << " " << type
	       << " {" << extName << "}"
	       << " {" << naxis << "}"
	       << " {" << naxis1 << "}"
	       << " {" << naxis2 << "}"
	       << " {" << naxis3 << "}"
	       << " {" << crpix1 << "}"
	       << " {" << crpix2 << "}"
	       << "} ";
	}
	count++;
    }
    if (count) {
	if (status == TCL_OK) {
	    set_result(os.str().c_str());
	}
	fits->setHDU(curHDU);
    }
    return status;
}

/*
 * Implement the hdu set subcommand:
 *
 *      <path> hdu set $number
 * or:  <path> hdu $number
 *
 * see comments for hduCmd() for details.
 */
int RtdImage::hduCmdSet(int argc, char** argv, FitsIO* fits)
{
    if (strcmp(argv[0], "set") == 0) {
	argc--;
	argv++;
    }
    if (argc != 1)
	return error("wrong number of args: expected HDU number");

    int num = 0;
    if (Tcl_GetInt(interp_, argv[0], &num) != TCL_OK)
	return TCL_ERROR;

    // get a copy so we can change the HDU without changing the original
    fits = fits->copy();
    if (fits->setHDU(num) != 0) {
	delete fits;
	return TCL_ERROR;
    }

    const char* hduType = fits->getHDUType();
    if (!hduType)
	return TCL_ERROR;

    if (*hduType != 'i') {
        delete fits;            // Otherwise just dropped. Note original
                                // FitsIO now positioned ay new HDU anyway.
	return TCL_OK;          // FITS table, not image: don't display
    }

    // save image transformation parameters to restore later
    ImageDataParams p;
    image_->saveParams(p);

    // delete old image
    delete image_;
    image_ = NULL;
    updateViews();

    // Re-initialize the image from the given HDU
    ImageData* im = makeImage(fits);
    if (! im)
	return TCL_ERROR;
    image_ = im;

    // The WCS info will be different in this HDU
    fits->wcsinit();

    // restore transformations
    image_->restoreParams(p, !autoSetCutLevels_);

    // update the display
    return initNewImage();
}


/*
 * Implement the hdu display subcommand:
 *
 *      <path> hdu display ?$hduList?
 *
 * see comments for hduCmd() for details.
 */
int RtdImage::hduCmdDisplay(int argc, char** argv, FitsIO* fits)
{
    int hduList[256];
    int numHDUs = 0;

    if (!image_)
	return error("No image to display");

    if (argc == 2) {
	// parse list of HDU indexes
	char** hdus = NULL;
	if (Tcl_SplitList(interp_, argv[0], &numHDUs, &hdus) != TCL_OK)
	    return TCL_ERROR;

	if (numHDUs > sizeof(hduList)/sizeof(int))
	    return fmt_error("RtdImage::hduCmdDisplay: too many HDUs: %d (max 256)", numHDUs);

	if (numHDUs == 0)
	    return error("No image HDUs were specified");

	for(int i = 0; i < numHDUs; i++) {
	    if (Tcl_GetInt(interp_, hdus[1], &hduList[i]) != TCL_OK) {
		Tcl_Free((char *)hdus);
		return TCL_ERROR;
	    }
	}
	Tcl_Free((char *)hdus);
    }
    else {
	// use all image extensions, except the primary HDU (1)
	// Note that the primary HDU is 1.
	int n = fits->getNumHDUs();
	int saved_hdu = fits->getHDUNum();

	for(int i = 2; i <= n; i++) {
	    if (fits->setHDU(i) != 0) {
		fits->setHDU(saved_hdu);
		return TCL_ERROR;		// error
	    }
	    const char* type = fits->getHDUType();
	    if (type != NULL && *type == 'i')
		hduList[numHDUs++] = i;
	}
	fits->setHDU(saved_hdu);
	if (numHDUs == 0)
	    return error("No image HDUs found");
    }

    // get a (reference counted) copy of the image
    ImageIO imio = image_->image();

    // used to save and restore image transformation parameters
    ImageDataParams p;
    image_->saveParams(p);

    // delete old image
    delete image_;
    image_ = NULL;
    updateViews();

    // Create an image composed of all of the requested image extensions
    image_ = ImageData::makeCompoundImage(name(), imio, hduList, numHDUs, biasimage_->biasInfo(), verbose());
    if (! image_)
	return TCL_ERROR;

    // restore transformations
    image_->restoreParams(p, !autoSetCutLevels_);

    return initNewImage();
}

