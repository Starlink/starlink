/*
 * E.S.O. - VLT project/Archive
 * $Id: TclQueryUtil.C,v 1.5 2003/01/20 15:52:21 brighton Exp $
 *
 * TclQueryUtil.C - utility routines used by TclAstroCat and TclTcsCat
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  14 Jun 96  Created
 */
static const char* const rcsId="@(#) $Id: TclQueryUtil.C,v 1.5 2003/01/20 15:52:21 brighton Exp $";


#include <cstring>
#include <cctype>
#include <cstdio>
#include <iostream>
#include <cstdlib>
#include <unistd.h>
#include "config.h"
#include "tcl.h"
#include "error.h"
#include "AstroCatalog.h"


/*
 * This utility routine generates an AstroQuery object, given the command
 * line arguments for a Tcl query command. In addition, the world coordinate
 * position and equinox arguments of the query are returned.
 *
 * This assumes a Tcl query command with the following syntax:
 *
 * $cat query -option value ...
 *
 * Most options correspond to the AstroQuery class members and methods:
 *
 *  -id $id
 *     catalog id of object, (as returned from a previous query). If this
 *     is specified, -pos, -name, -mag and -radius should not be
 *     specified and will be ignored.
 *
 *  -pos {ra dec}
 *  -pos {x y}
 *  -pos {ra1 dec1 ra2 dec2}
 *  -pos {x1 y1 x2 y2}
 *     World {ra, dec} or image {x, y} coordinates of center position, or
 *     list {ra1 dec1 ra2 dec2} or {x1 y1 x2 y2} of 2 points for an area.
 *     World Coordinates are given as {H:M:S[+-]D:M:S} in the given
 *     equinox.  If the catalog config entry contains the keywords
 *     "x_col" and "y_col", the coords are interpreted as image coords,
 *     otherwise world coords.
 *
 *  -equinox $equinox
 *     equinox for position (default 2000). May also be a string of the form
 *     "J2000", "B1950", "GALACTIC", "ECLIPTIC", to indicate the type of the
 *     search coordinates.
 *  
 *  -width $w
 *  -height $h
 *     Dimensions of rectangle with pos at center (alternative to
 *     specifying 2 positions) in arcmin for world coords or pixel for
 *     image coords.
 *
 *  -mag $mag
 *      max or list {min max} magnitude of object
 *
 *  -radius $r
 *     max or list (min max} radius from position (in arcmin for world
 *     coords or pixel for image coords).
 *
 *  -nameserver $ns
 *     name of nameserver catalog to use (simbad@eso, ned@eso,...)
 *
 *  -name $name
 *     can be used instead of -pos. The name will be resolved using the
 *     value of -nameserver (default: SIMBAD)
 *
 *  -columns {col1 col2 ...}
 *     list of columns to return
 *
 *  -searchcols {col1 col2 ...}
 *      list of columns to search by. The -minvalues and -maxvalues options
 *      supply the corresponding value ranges and must have the same lengths.
 *
 *  -minvalues {v1 v2 ...}
 *  -maxvalues {v1 v2 ...}
 *      list of values corresponding to the columns specified with the -searchcols
 *      option. The values may be numeric or string format, but the lists must have
 *      the same lengths as the one specified by the -searchcols option.
 *
 *  -sort {col1 col2 ...}
 *     list of column names to sort by
 *
 *  -sortorder increasing
 *  -sortorder decreasing
 *     Specify the sort order.
 *
 *  -nrows $n
 *     max number of rows to return.
 *
 * Each option has one value, however, for a range or area query, some
 * values can be a list, such as -radius "$rad1 $rad2" to give a radius
 * range or -pos "$pos1 $pos2" to give an area.
 *
 * If -columns is not specified, all columns are assumed.  Otherwise, if
 * -columns is specified, the column names should be valid for the
 * catalog and the result will be a list of rows with those columns.
 *
 * Note that not all catalogs will support sorting by all fields.
 *
 * On return, the AstroQuery object is set so that it cat be passed to a
 * query routine. In addition the world coordinate position and
 * equinox args are set, if applicable. 
 *
 * The feedback argument, if not null, should be a pointer to an open
 * file to which feedback information should be printed during the query.
 */
int genAstroQuery(Tcl_Interp* interp, int argc, char* argv[], 
		  AstroQuery& q, WorldOrImageCoords& pos1, WorldOrImageCoords& pos2, 
		  char* equinoxStr, FILE* feedback, CatalogInfoEntry* entry)
{
    // set defaults
    int status = 0;
    pos1.setNull(); pos2.setNull();
    int isWcs = entry->isWcs();
    int isPix = entry->isPix();
    strcpy(equinoxStr, "2000");
    double radius1 = 0.0, radius2 = 0.0;
    double mag1 = 0.0, mag2 = 0.0;
    double width = 0.0, height = 0.0;
    char* id = "";
    char* nameServer = "simbad@eso";
  
    // for sorting
    int numSortCols = 0; 
    char** sortCols = NULL; 
    char* sortOrder = "increasing";
    int nrows = 0;  // no default limit...

    // column selection
    int numCols = 0;
    char** colNames = NULL;

    // for searching by colName, minValue, maxValue
    int numSearchCols = 0;
    char** searchCols = NULL;
    char** minValues = NULL;
    char** maxValues = NULL;

    // misc
    int got_pos = 0;		// flag: true if we read the position arg
    char** values = NULL;
    int numValues = 0;

    // parse options
    for (int i = 0; i < argc; i += 2) {
	char* option = argv[i];
	char* value = argv[i+1];

	// first handle options with only one value
	if (strcmp(option, "-id") == 0) {
	    id = value;
	}
	else if (strcmp(option, "-nameserver") == 0) {
	    nameServer = value;
	}
	else if (strcmp(option, "-sortorder") == 0) {
	    sortOrder = value;
	    if (strlen(value) == 0)
		sortOrder = "increasing";
	    else if (strcmp(sortOrder, "increasing") != 0 && strcmp(sortOrder, "decreasing") != 0)
		return error("expected -sortorder increasing (or decreasing), not: ", sortOrder);
	}
	else if (strcmp(option, "-name") == 0) {
	    if (AstroCatalog::nameToWorldCoords(value, pos1, nameServer, feedback) != 0)
		return TCL_ERROR;
	}
	else if (strcmp(option, "-equinox") == 0) {
	    if (got_pos) 
		return error("-equinox should precede the -pos argument");
	    strcpy(equinoxStr, value);
	}
	else if (strcmp(option, "-nrows") == 0) {
	    if (Tcl_GetInt(interp, value, &nrows) != TCL_OK)
		return error("bad value for max number of rows: ", interp->result);;
	}
	else if (strcmp(option, "-width") == 0) {
	    if (Tcl_GetDouble(interp, value, &width) != TCL_OK) 
		return error("bad -width value: ", interp->result);
	}
	else if (strcmp(option, "-height") == 0) {
	    if (Tcl_GetDouble(interp, value, &height) != TCL_OK) 
		return error("bad -height value: ", interp->result);
	}
	else {
	    // handle options whic<h may have a tcl list of values
	    if (Tcl_SplitList(interp, value, &numValues, &values) != TCL_OK) 
		return TCL_ERROR;
	    
	    if (numValues < 1) {
		status = fmt_error("expected a list of values for %s option", option);
		break;
	    }

	    if (strcmp(option, "-pos") == 0) {
		if (!isWcs && !isPix) {
		    status = error("This catalog does not have coordinates");
		    break;
		}
		got_pos++;
		if (numValues != 2 && numValues != 4) {
		    if (isWcs)
			status = error("expected -pos {ra dec} or {ra1 dec1 ra2 dec2} (WCS positions)");
		    else if (isPix)
			status = error("expected -pos {x y} or {x1 y1 x2 y2} (in image coordinates)");
		    break;
		}
		if (numValues == 4) {
		    if (isWcs) 
			pos2 = WorldCoords(values[2], values[3], equinoxStr, 1);
		    else if (isPix)
			pos2 = ImageCoords(values[2], values[3]);
		    if (pos2.status()) {
			status = TCL_ERROR;
			break;
		    }
		}
		if (isWcs)
		    pos1 = WorldCoords(values[0], values[1], equinoxStr, 1);
		else if (isPix)
		    pos1 = ImageCoords(values[0], values[1]);
		if (pos1.status()) {
		    status = TCL_ERROR;
		}
	    }
	    else if (strcmp(option, "-radius") == 0) {
		if (numValues > 2) {
		    status = error("expected 1 or 2 values for -radius option");
		    break;
		}
		if (numValues == 2 && Tcl_GetDouble(interp, values[1], &radius2) != TCL_OK) {
		    status = error("bad max radius value: ", interp->result);
		    break;
		}
		if (Tcl_GetDouble(interp, values[0], &radius1) != TCL_OK) {
		    status = error("bad min radius value: ", interp->result);
		    break;
		}
	    }
	    else if (strcmp(option, "-mag") == 0) {
		if (numValues > 2) {
		    status = error("expected 1 or 2 values for -mag");
		    break;
		}
		if (numValues == 2 && Tcl_GetDouble(interp, values[1], &mag2) != TCL_OK) {
		    status = error("bad max magnitude value: ", interp->result);
		    break;
		}
		if (Tcl_GetDouble(interp, values[0], &mag1) != TCL_OK) {
		    status = error("bad min magnitude value: ", interp->result);
		    break;
		}
	    }
	    else if (strcmp(option, "-columns") == 0) {
		numCols = numValues;
		colNames = values;
		values = NULL;	// don't free
	    }
	    else if (strcmp(option, "-sort") == 0) {
		numSortCols = numValues;
		sortCols = values;
		values = NULL;	// don't free
	    }
	    else if (strcmp(option, "-searchcols") == 0) {
		numSearchCols = numValues;
		searchCols = values;
		values = NULL;	// don't free
	    }
	    else if (strcmp(option, "-minvalues") == 0) {
		if (numValues != numSearchCols) {
		    status = error("number of items for -minvalues not the same as for -searchcols");
		    break;
		}
		minValues = values;
		values = NULL;	// don't free
	    }
	    else if (strcmp(option, "-maxvalues") == 0) {
		if (numValues != numSearchCols) {
		    status = error("number of items for -maxvalues not the same as for -searchcols");
		    break;
		}
		maxValues = values;
		values = NULL;	// don't free
	    }
	}
    }

    if (values)
	free(values);
    if (status != TCL_OK)
	return TCL_ERROR;

    // setup the query object and return an error if the arguments are invalid
    // (args are checked by AstroQuery class)
    if (strlen(id) && q.id(id))
	return TCL_ERROR;

    if (pos2.isNull()) {
	if (! pos1.isNull())
	    if (q.pos(pos1))
		return TCL_ERROR;
    }
    else {
	if (q.pos(pos1, pos2))
	    return ERROR;
    }

    if (radius2) {
	if (q.radius(radius1, radius2))
	    return TCL_ERROR;
    }
    else if (radius1) {
	if (q.radius(radius1))
	    return TCL_ERROR;
    }

    if (mag2) {
	if (q.mag(mag1, mag2))
	    return TCL_ERROR;
    }
    else if (mag1) {
	if (q.mag(mag1))
	    return TCL_ERROR;
    }

    if (width && height) 
	if (q.dim(width, height))
	    return TCL_ERROR;
    
    if (numCols && colNames) 
	if (q.colNames(numCols, (char**)colNames, 1))
	    return TCL_ERROR;

    if (nrows && q.maxRows(nrows)) 
	return TCL_ERROR;

    if (numSortCols && sortCols) {
	if (q.sort(numSortCols, (char**)sortCols, 1))
	    return TCL_ERROR;
	q.sortOrder(*sortOrder == 'i' ? 1 : -1);
    }

    if (numSearchCols && searchCols) {
	if (q.condition(numSearchCols, (char**)searchCols, (char**)minValues, (char**)maxValues, 1))
	    return TCL_ERROR;
    }

    return TCL_OK;
}


