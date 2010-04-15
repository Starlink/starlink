#ifndef _GAIAUTILS_INCLUDED_
#define _GAIAUTILS_INCLUDED_

/*
 *  External prototypes and definitions for gaiaUtils.c.
 */

#ifdef __cplusplus
extern "C" {
#endif
#include <ast.h>
#include <star/atl.h>

    /* Maximum AST axes. */
    enum { MAX_DIMS = 32 };

    /* Construct an error message string from the current ADAM status. */
    char *gaiaUtilsErrMessage();

    /* Extract a FrameSet for an axis */
    int gaiaUtilsGtAxisWcs( AstFrameSet *fullwcs, int axis, int offset,
                            AstFrameSet **iwcs, char **error_mess );

    /* Extract a FrameSet for an image */
    int gaiaUtilsGt2DWcs( AstFrameSet *fullwcs, int axis1, int axis2,
                          int length1, int length2, int index,
                          AstFrameSet **iwcs, char **error_mess );


    /* Query the coordinate of a base pixel along the equivalent world
     * coordinate axis */
    int gaiaUtilsQueryCoord( AstFrameSet *frameset, int axis, double *coords,
                             int trailed, int readable, int formatted,
                             int ncoords, char **coord, char **error_mess );

    /* Get Plots for each ROI in a Plot */
    int gaiaUtilsAtlPlROI( AstPlot *plot, AstKeyMap **rplots,
                           char **error_mess );

    /* Trim current frame axes and identify any ROIs */
    int gaiaUtilsAtlAxTrm( AstFrameSet *frameset, int axes[], int lbnd[],
                           int ubnd[], double work[], char **error_mess );

    /* Read FITS headers (char array) into a FITS channel */
    int gaiaUtilsGtFitsChan( char header[], int ncards,
                             AstFitsChan **fitschan );

    /* Read FITS headers and return a FrameSet */
    int gaiaUtilsGtFitsWcs( char header[], int ncards, char *encoding,
                            AstFrameSet **iwcs );

    /* Convert ARD description into a mask */
    int gaiaUtilsCreateArdMask( char *desc, int maskPtr[], int dims[],
                                int lbnd[], int ubnd[], char **error_mess );

#ifdef __cplusplus
}
#endif

#endif
