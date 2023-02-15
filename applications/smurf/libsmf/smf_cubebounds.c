/*
*+
*  Name:
*     smf_cubebounds

*  Purpose:
*     Calculate the pixel index bounds for a cube

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_cubebounds( Grp *igrp,  int size, AstSkyFrame *oskyframe,
*                     int autogrid, int usedetpos, AstFrameSet *spacerefwcs,
*                     AstFrameSet *specrefwcs, double par[ 7 ],
*                     Grp *detgrp, int moving, int specunion, dim_t lbnd[ 3 ],
*                     dim_t ubnd[ 3 ], AstFrameSet **wcsout, int *npos,
*                     int *hasoffexp, smfBox **boxes, int *polobs,
*                     double *aref, double *bref, int *status );

*  Arguments:
*     igrp = Grp * (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     oskyframe = AstSkyFrame * (Given)
*        A SkyFrame that specifies the coordinate system used to describe
*        the spatial axes of the output cube. If "moving" is non-zero, this
*        should represent offsets from the tracking centre rather than
*        absolute celestial coordinates. If "spacerefwcs" is non-NULL,
*        the supplied value is ignored and the current Frame in "spacererfwcs"
*        is used instead.
*     autogrid = int (Given)
*        If non-zero then the fractional pixel shift implied by elements
*        0 and 1 of "par" are retained. Otherwise, they are ignored and
*        the used shift puts the central axis value at the centre of a
*        pixel.
*     usedetpos = int (Given)
*        If a non-zero value is supplied, then the detector positions for
*        a given time slice are read directly from the input NDF. Otherwise
*        the detector positions are calculated on the basis of the focal
*        plane detector positions and the telescope pointing information.
*     spacerefwcs = AstFrameSet * (Given)
*        A pointer to a FrameSet describing the spatial axes of the reference
*        NDF. This will be NULL if no reference NDF was supplied or if
*        the reference NDF had no spatial axes. If a non-null pointer is
*        supplied, the output WCS is inherited from this FrameSet rather than
*        from the values supplied in "par".
*     specrefwcs = AstFrameSet * (Given)
*        A pointer to a FrameSet describing the spectral axis of the reference
*        NDF. This will be NULL if no reference NDF was supplied or if
*        the reference NDF had no spectral axis. If a non-null pointer is
*        supplied, the output WCS is inherited from this FrameSet rather than
*        from the spectral axis of the first input NDF.
*     par = double[ 7 ] (Given)
*        This parameter is ignored if a non-NULL value is supplied for
*        "spacerefwcs". Otherwise, it should be an array holding the
*        parameters describing the spatial projection between celestial
*        (longitude,latitude) in the system specified by "oskyframe", and an
*        interim GRID coordinate system for the output cube (interim because
*        the bounds of the output cube are not yet known - the interim grid
*        system is thus more like a PIXEL system for the output cube but with
*        an arbitrary pixel origin). These are stored in the order CRPIX1,
*        CRPIX2, CRVAL1, CRVAL2, CDELT1, CDELT2, CROTA2. The supplied values
*        are used to produce the output WCS FrameSet. All the angular
*        parameters are in units of radians, and CRPIX1/2 are in units of
*        pixels.
*     detgrp = Grp * (Given)
*        A Group containing the names of the detectors to be used. All
*        detectors will be used to calculate the bounds of the output
*        cube if this pointer is NULL or if the group is empty.
*     moving = int (Given)
*        A flag indicating if the telescope is tracking a moving object. If
*        so, each time slice is shifted so that the position specified by
*        TCS_AZ_BC1/2 is mapped on to the same pixel position in the
*        output cube.
*     specunion = int (Given)
*        If non-zero, then the output spectral range is the union of the
*        input spectral ranges. Otherwise it is the intersection of the input
*        spectral ramges.
*     lbnd = dim_t [ 3 ] (Returned)
*        The lower pixel index bounds of the output cube.
*     ubnd = dim_t [ 3 ] (Returned)
*        The upper pixel index bounds of the output cube.
*     wcsout = AstFrameSet ** (Returned)
*        A pointer to a location at which to return a pointer to an AST
*        Frameset describing the WCS to be associated with the output cube.
*     npos = int * (Returned)
*        Address of an int in which to return the number of good spatial data
*        positions that will be used in the output cube.
*     hasoffexp = int * (Returned)
*        Address of an int in which to return a flag indicating if any of
*        the supplied input files has a OFF_EXPOSURE component in the JCMTSTATE
*        NDF extension.
*     boxes = smfBox ** (Returned)
*        Location at which to returned a pointer to an array of smfBox
*        structures. The length of this array is equal to the number of input
*        files in group "igrp". Each element of the array holds the bounds
*        of the spatial coverage of the corresponding input file, given as
*        pixel indices within the output cube. The array should be freed
*        using astFree when no longer needed.
*     polobs = int * (Returned)
*        Non-zero if all the input files contain polarisation data.
*     aref = double * (Returned)
*        If "moving" is non-zero but the output skyframe represents
*        absolute sky coordinates, then "*aref" is returned holding the
*        absolute longitude (rads) of the target on the first time slice.
*        The supplied value is left unchanged otherwise.
*     bref = double * (Returned)
*        If "moving" is non-zero but the output skyframe represents
*        absolute sky coordinates, then "*aref" is returned holding the
*        absolute latitude (rads) of the target on the first time slice.
*        The supplied value is left unchanged otherwise.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function finds the pixel index bounds of the 3D output cube that
*     will just encompass all the data in the supplied group of input NDFs.
*     Each input NDF should be an ACSIS archive file. A WCS FrameSet is also
*     returned for the output cube. The base Frame in this FrameSet is 3D
*     GRID coords in the cube, and the current Frame is a CmpFrame holding
*     (lon,lat,freq) axes, where "lon,lat" are (if "moving" is zero) celestial
*     longitude and latitude in the system specified by "system". The spatial
*     projection in the cube is a tangent plane projection defined by
*     "par". The spectral axis system and projection are inherited from the
*     first supplied input data file, or from "specrefwcs".
*
*     If "moving" is non-zero, the spatial axes represent (lon,lat) offsets
*     in the requested output frame from the base telescope position associated
*     with the first time slice.
*
*     Note, the bounds of the spatial axes represent the union of the spatial
*     coverage of each input NDF, but the bounds of the spectral axis
*     represent either the intersection or union, as specified by "specunion".

*  Authors:
*     David S Berry (JAC, UCLan)
*     Ed Chapin (UBC)
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     19-SEP-2006 (DSB):
*        Initial version.
*     13-OCT-2006 (DSB):
*        Changed to get the input spectral WCS from the WCS FrameSet rather
*        than the FITS header.
*     1-NOV-2006 (DSB):
*        Use new smf_makefitschan interface.
*     14-NOV-2006 (DSB):
*        - Exclude bad data values from the bounding box.
*        - Move catalogue creation to smf_cubegrid.
*        - New interface.
*     21-NOV-2006 (DSB):
*        Correct inversion of ospecmap so that it combines correctly with
*        specmap.
*     23-NOV-2006 (DSB):
*        Correct indexing of "specin" array.
*     29-NOV-2006 (DSB):
*        Allow user to restrict the spectral range of the output cube
*        using parameter SPECBOUNDS.
*     1-DEC-2006 (DSB):
*        Correct memory leak.
*     9-JAN-2007 (DSB):
*        Determine the pixel index bounds of the spectral axis in the
*        same way for both autogrid and non-autogrid mode.
*     12-JAN-2007 (DSB):
*        Move reporting of axis labels into smurf_makecube.
*     22-JAN-2007 (DSB):
*        - Restructured again for better handing of moving targets.
*        - Added "detgrp" parameter.
*        - Restrict the output spectral ramge to the intersection of the
*        input spectral ranges, rather than the union.
*        - Ensure SPECBOUNDS values are used in the right order.
*     24-JAN-2007 (DSB):
*        Change determination of output bounds and pixel origin so that
*        the pixel origin is at the tangent point.
*     12-FEB-2007 (DSB):
*        Added hasoffexp argument.
*     15-FEB-2007 (DSB):
*        Report error if SPECBOUNDS values do not have any overlap with
*        the spectral range covered by the data.
*     24-AUG-2007 (DSB):
*        Added argument "boxes". The bounding box of each input file will
*        be needed by the algorithm that chooses which input files contribute
*        to each tile of the output.
*     31-AUG-2007 (DSB):
*        Remove debugging printf statements.
*     19-OCT-2007 (DSB):
*        Added parameter "specunion".
*     29-OCT-2007 (EC):
*        Modified interface to smf_open_file.
*     14-DEC-2007 (EC):
*        Call smf_open_file with SMF__NOCREATE_DATA
*     18-DEC-2007 (DSB):
*        - Added arguments spacerefwcs and specrefwcs.
*        - Remove the SMF__NOCREATE_DATA flag in the call to smf_open_file.
*     18-DEC-2007 (AGG):
*        Update to use new smf_free behaviour
*     19-DEC-2007 (DSB):
*        Correct the way reference WCS is handled.
*     04-JAN-2008 (TIMJ):
*        Remove debugging printf statements.
*     7-JAN-2008 (DSB):
*        Allow user to override default output pixel bounds using
*        parameters LBND and UBND.
*     21-JAN-2008 (DSB):
*        Added argument polobs.
*     12-FEB-2008 (DSB):
*        Modify the pixel origin in the returned FrameSet if the user
*        changes the lower pixel bounds using parameter LBND.
*     28-MAY-2008 (TIMJ):
*        NINT now defined in smf.h
*     18-DEC-2008 (TIMJ):
*        Use smf_makefitschan
*     14-JUL-2010 (DSB):
*        Changed specbounds values to be the spectral values at the outer
*        edges of the first and last channel, rather than the values at
*        the channel centres. This makes it consistent with the values
*        written to the FLBND and FUBND parameters.
*     14-MAR-2011 (DSB):
*        Ensure user-supplied pixel bounds do not extend beyond the
*        available data if the TRIM parameter is set TRUE.
*     14-OCT-2014 (DSB):
*        Handle cases where the target is moving but the output cube has
*        absolute sky coords (e.g. when creating JSA tiles for moving targets).
*        Included adding arguments aref and bref.
*     16-OCT-2015 (DSB):
*        Use smf_set_moving to assign attributes for a moving target,
*        rather than just setting SkyRefIs (smf_set_moving also sets
*        AlignOffset).
*     22-JAN-2021 (DSB):
*        Report an error if the cube exceeds 5000 pixels on either of the
*        two spatial axes.
*     15-OCT-2022 (GSB):
*        Add check of jos_drcontrol position problem flag.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2011 Science and Technology Facilities Council.
*     Copyright (C) 2006, 2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2021 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/ndg.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_cubebounds"
#define MAX_DIM 5000

void smf_cubebounds( Grp *igrp,  int size, AstSkyFrame *oskyframe,
                     int autogrid, int usedetpos, AstFrameSet *spacerefwcs,
                     AstFrameSet *specrefwcs, double par[ 7 ],
                     Grp *detgrp, int moving, int specunion, dim_t lbnd[ 3 ],
                     dim_t ubnd[ 3 ], AstFrameSet **wcsout, int *npos,
                     int *hasoffexp, smfBox **boxes, int *polobs,
                     double *aref, double *bref, int *status ){

/* Local Variables */
   AstCmpFrame *cmpfrm = NULL;  /* Current Frame for output FrameSet */
   AstCmpMap *cmpmap = NULL;    /* Base -> Current Mapping for output FrameSet */
   AstCmpMap *ssmap = NULL;     /* I/p GRID-> o/p PIXEL Mapping for spectral axis */
   AstCmpMap *tmap = NULL;      /* Temporary Mapping */
   AstCmpMap *totmap = NULL;    /* WCS->GRID Mapping from input WCS FrameSet */
   AstFitsChan *fc = NULL;      /* FitsChan used to construct spectral WCS */
   AstFitsChan *fct = NULL;     /* FitsChan used to construct time slice WCS */
   AstFrame *abskyframe = NULL; /* Output SkyFrame (always absolute) */
   AstFrame *ospecframe = NULL; /* Spectral Frame in output FrameSet */
   AstFrame *sf1 = NULL;        /* Pointer to copy of input current Frame */
   AstFrame *skyin = NULL;      /* Pointer to current Frame in input WCS FrameSet */
   AstFrame *specframe = NULL;  /* Spectral Frame in input FrameSet */
   AstFrameSet *azel2usesys_fs = NULL;/* FrameSet from AZEL to the output sky frame */
   AstFrameSet *fs = NULL;      /* A general purpose FrameSet pointer */
   AstFrameSet *swcsin = NULL;  /* FrameSet describing spatial input WCS */
   AstMapping *azel2usesys = NULL;/* Mapping from AZEL to the output sky frame */
   AstMapping *fsmap = NULL;    /* Base->Current Mapping extracted from a FrameSet */
   AstMapping *oskymap = NULL;  /* Sky <> PIXEL mapping in output FrameSet */
   AstMapping *oskymap2 = NULL; /* Sky offsets <> PIXEL mapping in output FrameSet */
   AstMapping *ospecmap = NULL; /* Spec <> PIXEL mapping in output FrameSet */
   AstMapping *specmap = NULL;  /* PIXEL -> Spec mapping in input FrameSet */
   const char *name;     /* Pointer to current detector name */
   dim_t irec;           /* Index of current input detector */
   dim_t ishift;         /* Shift to put pixel origin at centre */
   dim_t ispec;          /* Index of current spectral sample */
   dim_t itime;          /* Index of current time slice */
   dim_t itmp;           /* Temporary storage */
   dim_t lbnd0[ 2 ];     /* Defaults for LBND parameter */
   dim_t ubnd0[ 2 ];     /* Defaults for UBND parameter */
   double *xin = NULL;   /* Workspace for detector input grid positions */
   double *xout = NULL;  /* Workspace for detector output pixel positions */
   double *yin = NULL;   /* Workspace for detector input grid positions */
   double *yout = NULL;  /* Workspace for detector output pixel positions */
   double a;             /* Longitude value */
   double b;             /* Latitude value */
   double dlbnd[ 3 ];    /* Floating point lower bounds for output cube */
   double dubnd[ 3 ];    /* Floating point upper bounds for output cube */
   double gshift[ 3 ];   /* Shifts from interim to final GRID coords */
   double ispecbounds[ 2 ];/* Bounds of spectral axis in grid pixels */
   double olda;          /* Previous longitude value */
   double oldb;          /* Previous latitude value */
   double specbounds[ 2 ]; /* Bounds of spectral axis in spectral WCS units */
   double specin[ 2];    /* Spectral values to be transformed */
   double specout[ 2];   /* Transformed spectral values */
   double temp;          /* Temporary storage used when swapping values */
   drcntrl_bits drcntrl_mask = DRCNTRL__TCS_POSN_BIT; /* Mask to use for DRCONTROL */
   float *pdata;         /* Pointer to next data sample */
   int actval;           /* Number of parameter values supplied */
   int good;             /* Are there any good detector samples? */
   int ibasein;          /* Index of base Frame in input FrameSet */
   int ifile;            /* Index of current input file */
   int nval;             /* Number of values supplied */
   int pixax[ 3 ];       /* The output fed by each selected mapping input */
   int specax;           /* Index of spectral axis in input FrameSet */
   int trim;             /* Trim borders of bad pixels from o/p cube? */
   size_t found;         /* Was the detector name found in the supplied group? */
   smfBox *box;          /* Pointer to bounding box for next input file */
   smfData *data = NULL; /* Pointer to data struct for current input file */
   smfFile *file = NULL; /* Pointer to file struct for current input file */
   smfHead *hdr = NULL;  /* Pointer to data header for this time slice */

/* Initialise returned values */
   *npos = 0;
   *hasoffexp = 0;
   *polobs = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Tell the user what is happening. */
   msgOutif( MSG__VERB, " ", "SMURF_MAKECUBE: Determine cube bounds", status );

/* Begin an AST context. */
   astBegin;

/* Initialise the bounds of the output cube in floating point PIXEL coords. */
   dlbnd[ 0 ] = VAL__MAXD;
   dlbnd[ 1 ] = VAL__MAXD;
   dlbnd[ 2 ] = VAL__MAXD;
   dubnd[ 0 ] = VAL__MIND;
   dubnd[ 1 ] = VAL__MIND;
   dubnd[ 2 ] = VAL__MIND;

/* If spatial reference WCS was supplied, use its current frame rather
   than the supplied "oskyframe". */
   if( spacerefwcs ) oskyframe = astGetFrame( spacerefwcs, AST__CURRENT );

/* Create the array of returned smfBox structures, and store a pointer to
   the next one to be initialised. */
   *boxes = astMalloc( sizeof( smfBox )*size );
   box = *boxes;

/* If a reference NDF was supplied, the spectral Mapping (from output
   spectral to interim GRID) and Frame (a SpecFrame) are obtained from the
   supplied spectral reference NDF. Otherwise, these pointers will be set
   later from the first input NDF. */
   if( specrefwcs ) {
      ospecframe = astGetFrame( specrefwcs, AST__CURRENT );
      ospecmap = astGetMapping( specrefwcs, AST__CURRENT, AST__BASE );
   }

/* Assume for the moment that all data is polarisation data. */
   *polobs = 1;

/* Loop round all the input NDFs. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++, box++ ) {

/* Initialise the spatial bounds of section of the the output cube that is
   contributed to by the current ionput file. */
      box->lbnd[ 0 ] = VAL__MAXD;
      box->lbnd[ 1 ] = VAL__MAXD;
      box->ubnd[ 0 ] = VAL__MIND;
      box->ubnd[ 1 ] = VAL__MIND;

/* Obtain information about the current input NDF. */
      smf_open_file( NULL, igrp, ifile, "READ", 0, &data, status );

/* Issue a suitable message and abort if anything went wrong. */
      if( *status != SAI__OK ) {
         msgSeti( "I", ifile );
         errRep( FUNC_NAME, "Could not open input data file no. ^I.", status );
         break;

      } else {
         if( data->file == NULL ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "No smfFile associated with smfData.",
                    status );
            break;

         } else if( data->hdr == NULL ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "No smfHead associated with smfData.",
                    status );
            break;

         } else if( data->hdr->fitshdr == NULL ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "No FITS header associated with smfHead.",
                    status );
            break;

         }
      }

/* Get some convenient pointers. */
      file = data->file;
      hdr = data->hdr;

/* Report the name of the input file. */
      smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
      msgSeti( "I", ifile );
      msgSeti( "N", size );
      msgOutif( MSG__VERB, " ", "SMF_CUBEBOUNDS: Processing ^I/^N ^FILE",
                status );

/* Make sure the input file is a suitable ACSIS cube. */
      if( hdr->instrument != INST__ACSIS ) {
         smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "^FILE does not contain ACSIS instrument data.",
                 status );
         break;
      }

/* Check that there are 3 pixel axes. */
      if( data->ndims != 3 ) {
         smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
         msgSeti( "NDIMS", data->ndims );
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "^FILE has ^NDIMS pixel axes, should be 3.",
                 status );
         break;
      }

/* If the detector positions are to calculated on the basis of FPLANEX/Y
   rather than RECEPPOS, then free the detpos array in the smfHead
   structure. This will cause smf_tslice_ast to use the fplanex/y values. */
      if( !usedetpos && hdr->detpos ) {
         hdr->detpos = astFree( (double *) hdr->detpos );
      }

/* We want a description of the spectral WCS axis in the input file. If
   the input file has a WCS FrameSet containing a SpecFrame, use it,
   otherwise we will obtain it from the FITS header later. NOTE, if we knew
   that all the input NDFs would have the same spectral axis calibration,
   then the spectral WCS need only be obtained from the first NDF. However,
   in the general case, I presume that data files may be combined that use
   different spectral axis calibrations, and so these differences need to
   be taken into account. */
      if( hdr->tswcs ) {
         fs = astClone( hdr->tswcs );

/* The first axis should be a SpecFrame. See if this is so. If not annul
   the specframe pointer. */
         specax = 1;
         specframe = astPickAxes( fs, 1, &specax, NULL );
         if( !astIsASpecFrame( specframe ) ) specframe = astAnnul( specframe );
      }

/* If the above did not yield a SpecFrame, use the FITS-WCS headers in the
   FITS extension of the input NDF. Take a copy of the FITS header (so that
   the contents of the header are not changed), and then read a FrameSet
   out of it. */
      if( !specframe ) {
         fc = astCopy( hdr->fitshdr );
         astClear( fc, "Card" );
         fs = astRead( fc );

         if( !fs ) {
            if( *status == SAI__OK ) {
               smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "Cannot read WCS information from "
                       "the FITS header in ^FILE.", status );
            }
            break;
         }

/* Extract the SpecFrame that describes the spectral axis from the current
   Frame of this FrameSet. This is assumed to be the third WCS axis (NB
   the different axis number). */
         specax = 3;
         specframe = astPickAxes( fs, 1, &specax, NULL );
         if( !astIsASpecFrame( specframe ) ) {
            if( *status == SAI__OK ) {
               smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "FITS-WCS axis 1 in ^FILE is not a spectral "
                       "axis.", status );
            }
            break;
         }
      }

/* Split off the 1D Mapping for this single axis from the 3D Mapping for
   the whole WCS. This results in "specmap" holding the Mapping from
   SpecFrame value to interim GRID value. */
      fsmap = astGetMapping( fs, AST__CURRENT, AST__BASE );
      astMapSplit( fsmap, 1, &specax, pixax, &specmap );
      if( !specmap || astGetI( specmap, "Nout" ) != 1 ) {
         if( *status == SAI__OK ) {
            smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "The spectral axis in ^FILE is not "
                    "independent of the other axes.", status );
         }
         break;
      }

/* Invert the Mapping for the spectral axis so that it goes from interim GRID
   coord to spectral coord. */
      astInvert( specmap );

/* If this is the first input file, and the output is inheriting the
   spectral WCS from the first input file, initialise the bounds of the
   spectral axis in the output cube. */
      if( !ospecframe && !specrefwcs ) {
         dlbnd[ 2 ] = 1.0;
         dubnd[ 2 ] = (data->dims)[ 0 ];

/* If this is not the first input file, or if we are copying spectral WCS
   from a reference NDF to the output NDF, then there is potentially a
   difference between the spectral system of this input file and the spectral
   system of the output cube. So use astConvert to get a Mapping from one
   to the other. */
      } else {
         fs = astConvert( specframe, ospecframe, "" );

/* Report an error and abort if no conversion could be found between the two
   spectral axes. */
         if( !fs ) {
            if( *status == SAI__OK ) {
               smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "The spectral axis in ^FILE is not "
                       "compatible with the spectral axis in the first "
                       "input file.", status );
            }
            break;

/* Otherwise, combine these Mappings to get the Mapping from the input
   spectral interim GRID axis to the output spectral PIXEL axis. Note,
   "ospecmap" represents the Mapping from the output spectral WCS axis to
   the corresponding output interim GRID axis. */
         } else {
            ssmap = astCmpMap( astCmpMap( specmap,
                                          astGetMapping( fs, AST__BASE,
                                                         AST__CURRENT ),
                                          1, " " ),
                               ospecmap, 1, " " );

            ssmap = astSimplify( ssmap );
         }

/* Use this Mapping to transform the first and last spectral values
   in the input into the corresponding values on the output spectral
   interim GRID axis. */
         specin[ 0 ] = 1.0;
         specin[ 1 ] = (data->dims)[ 0 ];
         astTran1( ssmap, 2, specin, 1, specout );

/* Order the values in "specout". */
         if( specout[ 0 ] > specout[ 1 ] ) {
            temp = specout[ 0 ];
            specout[ 0 ] = specout[ 1 ];
            specout[ 1 ] = temp;
         }

/* Update the bounds of the output cube on the spectral PIXEL axis to
   hold the bounds of either the union or intersection of the spectral
   ranges. */
         if( specunion ) {
            if( specout[ 0 ] < dlbnd[ 2 ] ) dlbnd[ 2 ] = specout[ 0 ];
            if( specout[ 1 ] > dubnd[ 2 ] ) dubnd[ 2 ] = specout[ 1 ];
         } else {
            if( specout[ 0 ] > dlbnd[ 2 ] ) dlbnd[ 2 ] = specout[ 0 ];
            if( specout[ 1 ] < dubnd[ 2 ] ) dubnd[ 2 ] = specout[ 1 ];
         }
      }

/* Allocate work arrays big enough to hold the coords of all the
   detectors in the current input file.*/
      xin = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
      yin = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
      xout = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
      yout = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );

/* Store the input "GRID" coords of the detectors. These are just the
   detector index on axis 1 and a fixed value of 1 on axis 2. */
      for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {
         xin[ irec ] = irec + 1.0;
         yin[ irec ] = 1.0;
      }

/* Store a pointer to the next input data value */
      pdata = ( data->pntr )[ 0 ];

/* We now need to determine the spatial extent of the input file, and
   then modify the spatial bounds of the output cube to accomodate it.
   This involves finding the spatial extent of each time slice in the
   input. Loop round all the time slices in the input file. */
      for( itime = 0; itime < (data->dims)[ 2 ] && *status == SAI__OK; itime++ ) {

/* Skip this time slice if flagged due to a position problem. */
         if( (hdr->allState)[itime].jos_drcontrol & drcntrl_mask ) {
            continue;
         }

/* Get a FrameSet describing the spatial coordinate systems associated with
   the current time slice of the current input data file. The base frame in
   the FrameSet will be a 2D Frame in which axis 1 is detector number and
   axis 2 is unused. The current Frame will be a SkyFrame (the SkyFrame
   System may be any of the JCMT supported systems). The Epoch will be
   set to the epoch of the time slice. */
         smf_tslice_ast( data, itime, 1, NO_FTS, status );
         swcsin = hdr->wcs;

/* Update the flag indicating if any OFF_EXPOSURE values are available in
   the input data. */
         if( hdr->state->acs_offexposure != VAL__BADR ) *hasoffexp = 1;

/* Update the flag indicating if all input data is polarisation data. */
         if( hdr->state->pol_ang == VAL__BADD ) *polobs = 0;

/* Create an interim FrameSet describing the WCS to be associated with the
   output cube unless this has already be done. It is described as
   interim because the base Frame that describes the pixel grid coords is
   not yet finalised since we have not yet decided where the pixel origin
   will be. The pixel origin will be decided once the bounds of the data
   in the interim grid system have been found. */
         if( *wcsout == NULL && *status == SAI__OK ) {

/* If no reference NDF was supplied, the spectral Mapping (from interim GRID
   to SPECTRUM) and Frame (a SpecFrame) are obtained from the first input
   file. If a spectral Mapping was obtained from the reference NDF, then
   invert it so that he "ospecmap" mapping goes from grid to spectral coords
   which ever way it was obtained. */
            if( !specrefwcs ) {
               ospecframe = astClone( specframe );
               ospecmap = astClone( specmap );
            } else {
               astInvert( ospecmap );
            }

/* Ensure the specframe has the same epoch as the supplied SkyFrame. */
            if( astTest( oskyframe, "Epoch" ) ) {
               astSetC( ospecframe, "Epoch", astGetC( oskyframe, "Epoch" ) );
            }

/* Get a copy of the output SkyFrame and ensure it represents absolute
   coords rather than offset coords. */
            abskyframe = astCopy( oskyframe );
            astClear( abskyframe, "SkyRefIs" );
            astClear( abskyframe, "AlignOffset" );

/* If a spatial reference WCS has been supplied, use it. */
            if( spacerefwcs ) {
               fs = astClone( spacerefwcs );

/* Otherwise, get one from the "par" values using a FitsChan. */
            } else {

/* Now populate a FitsChan with FITS-WCS headers describing
   the required tan plane projection. The longitude and
   latitude axis types are set to either (RA,Dec) or (AZ,EL)
   to get the correct handedness. */
               fct = astFitsChan ( NULL, NULL, " " );
               smf_makefitschan( astGetC( oskyframe, "System"), &(par[0]),
                                 &(par[2]), &(par[4]), par[6], fct, status );

/* Read a FrameSet from this FitsChan. */
               astClear( fct, "Card" );
               fs = astRead( fct );
            }

/* Get the mapping from interim GRID to SKY coordsi n the output grid. */
            oskymap = astGetMapping( fs, AST__BASE, AST__CURRENT );

/* If the target is moving, but the output SkyFrame is absolute (for
   instance, if an absolute reference NDF has been supplied), we need a
   version of oskymap that goes from output grid coords to *offset* sky
   coords. */
            if( moving && astChrMatch( astGetC( oskyframe, "SkyRefIs" ),
                                       "Ignored" ) ){

/* We need to force the skyframe to use offset coords, with respect to
   the first telescope base position. So first we get the Mapping from AZEL
   (at the current input epoch) to the (absolute) output sky system. Use
   it to convert the telescope base pointing position from (az,el) to the
   requested system. */
               skyin = astGetFrame( swcsin, AST__CURRENT );
               sf1 = astCopy( skyin );
               astSetC( sf1, "System", "AZEL" );
               azel2usesys_fs = astConvert( sf1, oskyframe, "" );
               tmap = astGetMapping( azel2usesys_fs, AST__BASE, AST__CURRENT );
               azel2usesys = astSimplify( tmap );
               tmap = astAnnul( tmap );
               azel2usesys_fs = astAnnul( azel2usesys_fs );

               astTran2( azel2usesys, 1, &(hdr->state->tcs_az_bc1),
                         &(hdr->state->tcs_az_bc2), 1, &a, &b );
               if( aref ) *aref = a;
               if( bref ) *bref = b;

/* Modified the FrameSet to represent offsets from this origin. */
               olda = astTest( fs, "SkyRef(1)" ) ? astGetD( fs, "SkyRef(1)" ) : AST__BAD;
               oldb = astTest( fs, "SkyRef(2)" ) ? astGetD( fs, "SkyRef(2)" ) : AST__BAD;
               astSetD( fs, "SkyRef(1)", a );
               astSetD( fs, "SkyRef(2)", b );
               smf_set_moving( (AstFrame *) fs, NULL, status );

/* Get the Mapping and then reset the SkyRef attributes. */
               oskymap2 = astGetMapping( fs, AST__BASE, AST__CURRENT );

               astClear( fs, "SkyRefIs" );
               if( olda != AST__BAD ) {
                  astSetD( fs, "SkyRef(1)", olda );
               } else {
                  astClear( fs, "SkyRef(1)" );
               }
               if( oldb != AST__BAD ) {
                  astSetD( fs, "SkyRef(2)", oldb );
               } else {
                  astClear( fs, "SkyRef(2)" );
               }

/* If the target is not moving, we need a version of oskymap that goes from
   output grid coords to *absolute* sky coords. This will be required if
   a reference NDF that uses offset coords is supplied for a non-moving target. */
            } else if( !moving && astChrMatch( astGetC( oskyframe, "SkyRefIs" ),
                                              "Origin" ) ){
               astClear( fs, "SkyRefIs" );
               oskymap2 = astGetMapping( fs, AST__BASE, AST__CURRENT );
               astSetC( fs, "SkyRefIs", "Origin" );

/* Otherwiuse, just use the FrameSet as it is. */
            } else {
               oskymap2 = astClone( oskymap );
            }

/* Construct the CmpFrame that will be used as the current Frame in the
   output cube WCS FrameSet. */
            cmpfrm = astCmpFrame( oskyframe, ospecframe, " " );

/* Construct the corresponding Mapping (from interim GRID coords to the
   above CmpFrame). */
            cmpmap = astCmpMap( oskymap, ospecmap, 0, " " );

/* Create the returned output cube WCS FrameSet, initialising it to hold a 3D
   GRID Frame, which represents interim GRID coords. This interim GRID
   Frame will be re-mapped later (once the bounds of the output cube are
   known) to represent actual GRID coords in the output cube, and a PIXEL
   Frame will also be added which puts the PIXEL origin at the tangent
   point. */
            *wcsout = astFrameSet( astFrame( 3, "Domain=GRID" ), " " );

/* Add the CmpFrame created above into the new FrameSet, using the above
   Mapping to join it to the 3D GRID Frame already in the FrameSet. */
            astAddFrame( *wcsout, AST__BASE, cmpmap, cmpfrm );

/* For later convenience, invert "oskymap2" so that it goes from output
   (lon,lat) to interim grid coords. */
            astInvert( oskymap2 );

/* Also invert the output spectral Mapping so that it goes from output
   spectral WCS value to output interim spectral grid value. */
            astInvert( ospecmap );
         }

/* Find out how to convert from input GRID coords (i.e. detector index) to
   the output sky frame. Note, we want absolute sky coords here, even if the
   target is moving. Record the original base frame before calling astConvert
   so that it can be re-instated later (astConvert modifies the base Frame). */
         astInvert( swcsin );
         ibasein = astGetI( swcsin, "Base" );
         fs = astConvert( swcsin, abskyframe, "SKY" );
         astSetI( swcsin, "Base", ibasein );
         astInvert( swcsin );

         if( fs == NULL ) {
            if( *status == SAI__OK ) {
               smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "The spatial coordinate system in ^FILE "
                       "is not compatible with the spatial coordinate "
                       "system in the first input file.", status );
            }
            break;
         }

/* The "fs" FrameSet has input GRID coords as its base Frame, and output
   (absolute) sky coords as its current frame. If the target is moving,
   modify this so that the current Frame represents offsets from the
   current telescope base pointing position (the mapping in the "fs"
   FrameSet is also modified automatically). */
         if( moving ) {

/* Get the Mapping from AZEL (at the current input epoch) to the
   (absolute) output sky system. Use it to convert the telescope base
   pointing position from (az,el) to the requested system. */
            skyin = astGetFrame( swcsin, AST__CURRENT );
	    sf1 = astCopy( skyin );
	    astSetC( sf1, "System", "AZEL" );
            azel2usesys = astConvert( sf1, abskyframe, "" );
            astTran2( azel2usesys, 1, &(hdr->state->tcs_az_bc1),
                      &(hdr->state->tcs_az_bc2), 1, &a, &b );

/* Explicitly annul these objects for efficiency in this tight loop. */
            azel2usesys = astAnnul( azel2usesys );
            sf1 = astAnnul( sf1 );
            skyin = astAnnul( skyin );

/* Modified the FrameSet to represent offsets from this origin. We use the
   FrameSet pointer "fs" rather than a pointer to the current Frame within
   the FrameSet. This means that the Mapping in the FrameSet will be
   modified to remap the current Frame. */
            astSetD( fs, "SkyRef(1)", a );
            astSetD( fs, "SkyRef(2)", b );
            smf_set_moving( (AstFrame *) fs, NULL, status );

/* Get the Mapping and then clear the SkyRef attributes (this is because
   the current Frame in "fs" may be "*skyframe" and we do not want to make a
   permanent change to *skyframe). */
            fsmap = astGetMapping( fs, AST__BASE, AST__CURRENT );
            astClear( fs, "SkyRef(1)" );
            astClear( fs, "SkyRef(2)" );
            astClear( fs, "SkyRefIs" );

/* If the target is not moving, just get the Mapping. */
         } else {
            fsmap = astGetMapping( fs, AST__BASE, AST__CURRENT );
         }

/* The output from "fs" now corresponds to the input to "oskymap2",
   whether the target is moving or not. Combine the input GRID to output
   SKY Mapping with the output SKY to output interim grid Mapping found
   earlier. */
         tmap = astCmpMap( fsmap, oskymap2, 1, " " );
         totmap = astSimplify( tmap );
         tmap = astAnnul( tmap );

/* Initialise a string to point to the name of the first detector for which
   data is available */
         name = hdr->detname;

/* Transform the positions of the detectors from input GRID to output
   interim GRID coords. Then extend the bounds of the output cube on the
   spatial axes to accomodate the new positions. */
         astTran2( totmap, (data->dims)[ 1 ], xin, yin, 1, xout, yout );
         for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {

/* If a group of detectors to be used was supplied, search the group for
   the name of the current detector. If not found, set the "good" flag
   false in order to skip this detector. */
            good = 1;
            if( detgrp ) {
               found = grpIndex( name, detgrp, 1, status );
               if( !found ) good = 0;
            }

/* Move on to the next available detector name. */
            name += strlen( name ) + 1;

/* If the detector is included in the group and has a valid position, see if
   it produced any good data values. */
            if( good && xout[ irec ] != AST__BAD && yout[ irec ] != AST__BAD ) {
               good = 0;
               for( ispec = 0; ispec < (data->dims)[ 0 ]; ispec++ ){
                  if( *(pdata++) != VAL__BADR ) {
                     good = 1;
                     pdata += (data->dims)[ 0 ] - ispec - 1;
                     break;
                  }
               }

/* If it did, extend the interim grid bounding box for the file and for the
   output cube to include the detector. */
               if( good ) {
                  if( xout[ irec ] > box->ubnd[ 0 ] ) box->ubnd[ 0 ] = xout[ irec ];
                  if( xout[ irec ] < box->lbnd[ 0 ] ) box->lbnd[ 0 ] = xout[ irec ];
                  if( yout[ irec ] > box->ubnd[ 1 ] ) box->ubnd[ 1 ] = yout[ irec ];
                  if( yout[ irec ] < box->lbnd[ 1 ] ) box->lbnd[ 1 ] = yout[ irec ];

                  if( xout[ irec ] > dubnd[ 0 ] ) dubnd[ 0 ] = xout[ irec ];
                  if( xout[ irec ] < dlbnd[ 0 ] ) dlbnd[ 0 ] = xout[ irec ];
                  if( yout[ irec ] > dubnd[ 1 ] ) dubnd[ 1 ] = yout[ irec ];
                  if( yout[ irec ] < dlbnd[ 1 ] ) dlbnd[ 1 ] = yout[ irec ];
                  npos++;
               }

/* If this detector is not included or does not have a valid position,
   increment the data pointer to point at the first sample for the
   next detector. */
            } else {
               pdata += (data->dims)[ 0 ];
            }
         }

/* For efficiency, explicitly annul the AST Objects created in this tight
   loop. */
         fs = astAnnul( fs );
         totmap = astAnnul( totmap );
         fsmap = astAnnul( fsmap );
      }

/* Close the current input data file. */
      smf_close_file( NULL, &data, status);
      data = NULL;

/* Free work space. */
      xin = astFree( xin );
      yin = astFree( yin );
      xout = astFree( xout );
      yout = astFree( yout );
   }

/* Close any data file that was left open due to an early exit from the
   above loop. */
   if( data != NULL ) smf_close_file( NULL, &data, status );

/* Check we found some usable data. */
   if( dlbnd[ 0 ] == VAL__MAXD || dlbnd[ 1 ] == VAL__MAXD ) {

      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "No usable data positions found.", status );
      }

   } else if( dlbnd[ 2 ] == VAL__MAXD || dlbnd[ 2 ] > dubnd[ 2 ] ) {

      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "No usable spectral channels found.", status );
      }
   }

/* See if the user wants to restrict the spectral range of the output cube.
   First get the bounds of the full frequency axis. Note, the specbounds
   values suppleid by the user are the spectral values at the outer edges
   of the first and last channel, not the centre of the channels. This is
   done so that the values written to output parameters FLBND/FUBND can be
   supplied directly as values for SPECBOUNDS.  */
   ispecbounds[ 0 ] = dlbnd[ 2 ] - 0.5;
   ispecbounds[ 1 ] = dubnd[ 2 ] + 0.5;
   astTran1( ospecmap, 2, ispecbounds, 0, specbounds );

/* Now allow the user to provide alternative values. The above values are
   used as dynamic defaults for the SPECBOUNDS parameter. */
   kpg1Gtaxv( "SPECBOUNDS", 2, 1, ospecframe, 1, specbounds, &nval, status );

/* Convert the supplied spectral values back to interim grid coords. */
   astTran1( ospecmap, 2, specbounds, 1, ispecbounds );

/* Ensure element 0 is lower than element 1. */
   if( ispecbounds[ 0 ] > ispecbounds[ 1 ] ) {
      temp = ispecbounds[ 0 ] ;
      ispecbounds[ 0 ] = ispecbounds[ 1 ];
      ispecbounds[ 1 ] = temp;
   }

/* Correct back to the centre of the first and last channel. */
   ispecbounds[ 0 ] += 0.5;
   ispecbounds[ 1 ] -= 0.5;

/* Check the specified spectral bounds have some overlap with the available
   spectral range. */
   if( ispecbounds[ 0 ] >= dubnd[ 2 ] ||
       ispecbounds[ 1 ] <= dlbnd[ 2 ] ){
      if( *status == SAI__OK ) {

         specin[ 0 ] = dlbnd[ 2 ];
         specin[ 1 ] = dubnd[ 2 ];
         astTran1( ospecmap, 2, specin, 0, specout );

         *status = SAI__ERROR;
         msgSetd( "L", specbounds[ 0 ] );
         msgSetd( "U", specbounds[ 1 ] );
         msgSetd( "LL", specout[ 0 ] );
         msgSetd( "UU", specout[ 1 ] );
         errRep( "", "Requested spectral bounds (^L,^U) do not overlap "
                 "the spectral range of the data (^LL,^UU).", status );
      }
   }

/* Update the output interim grid bounds, keeping them in the right order. */
   dlbnd[ 2 ] = ispecbounds[ 0 ] ;
   dubnd[ 2 ] = ispecbounds[ 1 ] ;

/* Now we know the bounds of the output cube in interim GRID coords, we
   can remap the GRID Frame in the output WCS FrameSet so that it
   represents real GRID coords rather than interim grid coords, and we
   can also add a PIXEL Frame. If reference WCS was supplied then we
   retain the existing bounds. This will cause the reference PIXEL origin
   to be retained. Otherwise, for the spatial axes, we choose the PIXEL
   origin so that the centre of pixel (1,1) (i.e. pixel coords (0.5,0.5) )
   is at the tangent point specified by par[2] and par[3]. However, if an
   optimal grid is being used, then this is modified to retain the fractional
   pixel offsets included in par[0] and par[1]. First deal with causes
   where we are not using an optimal grid. */
   if( !autogrid ) {

/* If spatial reference wcs was supplied, store par values that result in
   no change to the pixel origin. */
      if( spacerefwcs ){
         par[ 0 ] = 0.5;
         par[ 1 ] = 0.5;
      }

/* We want the interim grid value of par[0] to correspond to an output pixel
   coord of 0.5. Therefore the upper bound in interim grid coords (i.e.
   dubnd[0]) will correspond to "dubnd[0]-par[0]+0.5" in pixel coords. This
   will fall in the pixel with index "ceil(dubnd[0]-par[0]+0.5)". Likewise
   the lower bounds in interim grid coords will fall in the pixel with
   index "ceil(dlbnd[0]-par[0]+0.5)". These are the bounds of the output
   cube in pixel indices. Calculate them now. */
      lbnd[ 0 ] = ceil( dlbnd[ 0 ] - par[ 0 ] + 0.5 );
      ubnd[ 0 ] = ceil( dubnd[ 0 ] - par[ 0 ] + 0.5 );
      lbnd[ 1 ] = ceil( dlbnd[ 1 ] - par[ 1 ] + 0.5 );
      ubnd[ 1 ] = ceil( dubnd[ 1 ] - par[ 1 ] + 0.5 );

/* Do the same for the individual input file bounding boxes. */
      box = *boxes;
      for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++, box++ ) {
         box->lbnd[ 0 ] = ceil( box->lbnd[ 0 ] - par[ 0 ] + 0.5 );
         box->ubnd[ 0 ] = ceil( box->ubnd[ 0 ] - par[ 0 ] + 0.5 );
         box->lbnd[ 1 ] = ceil( box->lbnd[ 1 ] - par[ 1 ] + 0.5 );
         box->ubnd[ 1 ] = ceil( box->ubnd[ 1 ] - par[ 1 ] + 0.5 );
      }

/* Calculate the shifts from interim grid to final grid coords. */
      gshift[ 0 ] = 2.0 - par[ 0 ] - lbnd[ 0 ];
      gshift[ 1 ] = 2.0 - par[ 1 ] - lbnd[ 1 ];

/* Now deal with causes where we are using an optimal grid. In this case
   the fractional part of the supplied par[0] value will indicate where
   about within the central pixel the tangent point is to be put. Since
   par[0] is a *grid* value, a fractional part of zero means that the
   tangent point should be put at the centre of the pixel, which has a
   pixel coord of 0.5. So to get the pixel coord at the tanegt point, we
   need to add on 0.5 to the fractional part of par[0]. */
   } else {

/* Calculate the pixel index bounds. */
      lbnd[ 0 ] = ceil( dlbnd[ 0 ] - NINT( par[ 0 ] ) + 0.5 );
      ubnd[ 0 ] = ceil( dubnd[ 0 ] - NINT( par[ 0 ] ) + 0.5 );
      lbnd[ 1 ] = ceil( dlbnd[ 1 ] - NINT( par[ 1 ] ) + 0.5 );
      ubnd[ 1 ] = ceil( dubnd[ 1 ] - NINT( par[ 1 ] ) + 0.5 );

/* Do the same for the individual input file bounding boxes. */
      box = *boxes;
      for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++, box++ ) {
         box->lbnd[ 0 ] = ceil( box->lbnd[ 0 ] - NINT( par[ 0 ] ) + 0.5 );
         box->ubnd[ 0 ] = ceil( box->ubnd[ 0 ] - NINT( par[ 0 ] ) + 0.5 );
         box->lbnd[ 1 ] = ceil( box->lbnd[ 1 ] - NINT( par[ 1 ] ) + 0.5 );
         box->ubnd[ 1 ] = ceil( box->ubnd[ 1 ] - NINT( par[ 1 ] ) + 0.5 );
      }

/* Calculate the shifts from interim grid to final grid coords. */
      gshift[ 0 ] = 2.0 - NINT( par[ 0 ] ) - lbnd[ 0 ];
      gshift[ 1 ] = 2.0 - NINT( par[ 1 ] ) - lbnd[ 1 ];
   }

/* Now do the spectral axis. If spectral reference WCS was supplied,
   arrange for no change to the pixel origin. */
   if( specrefwcs ) {
      lbnd[ 2 ] = ceil( dlbnd[ 2 ] );
      ubnd[ 2 ] = ceil( dubnd[ 2 ] );
      gshift[ 2 ] = 1.5 - lbnd[ 2 ];

/* Otherwise, find the indices of the pixels containing the
   DLBND and DUBND points. */
   } else {
      lbnd[ 2 ] = NINT( dlbnd[ 2 ] );
      ubnd[ 2 ] = NINT( dubnd[ 2 ] );

/* Find the integer pixel shifts needed to put the lower bounds at pixel
   (1,1,1). */
      gshift[ 2 ] = 1 - lbnd[ 2 ];

/* Modify the bounds to put the origin at the centre */
      ishift = 1 + ( ubnd[ 2 ] + lbnd[ 2 ] )/2;
      lbnd[ 2 ] -= ishift;
      ubnd[ 2 ] -= ishift;
   }

/* Now remap the interim GRID Frame in the returned FrameSet so that it
   represent actual GRID coords in the output cube. */
   astRemapFrame( *wcsout, AST__BASE, astShiftMap( 3, gshift, " " ) );

/* Allow the user to override the output pixel bounds calculated above. */
   lbnd0[ 0 ] = lbnd[ 0 ];
   lbnd0[ 1 ] = lbnd[ 1 ];
   parDef1k( "LBND", 2, lbnd0, status );

   ubnd0[ 0 ] = ubnd[ 0 ];
   ubnd0[ 1 ] = ubnd[ 1 ];
   parDef1k( "UBND", 2, ubnd0, status );

   parGet1k( "LBND", 2, lbnd, &actval, status );
   if( actval == 1 ) lbnd[ 1 ] = lbnd[ 0 ];

   parGet1k( "UBND", 2, ubnd, &actval, status );
   if( actval == 1 ) ubnd[ 1 ] = ubnd[ 0 ];

/* Ensure the bounds are the right way round. */
   if( lbnd[ 0 ] > ubnd[ 0 ] ) {
      itmp = lbnd[ 0 ];
      lbnd[ 0 ] = ubnd[ 0 ];
      ubnd[ 0 ] = itmp;
   }

   if( lbnd[ 1 ] > ubnd[ 1 ] ) {
      itmp = lbnd[ 1 ];
      lbnd[ 1 ] = ubnd[ 1 ];
      ubnd[ 1 ] = itmp;
   }

/* If borders of bad pixels are being trimmed from the output cube,
   then do not allow the user-specified bounds to extend outside the
   default bounding box (since we know that the default bounding box
   encloses all available data). */
   parGet0l( "TRIM", &trim, status );
   if( trim ) {
      if( lbnd[ 0 ] < lbnd0[ 0 ] ) lbnd[ 0 ] = lbnd0[ 0 ];
      if( lbnd[ 1 ] < lbnd0[ 1 ] ) lbnd[ 1 ] = lbnd0[ 1 ];
      if( ubnd[ 0 ] > ubnd0[ 0 ] ) ubnd[ 0 ] = ubnd0[ 0 ];
      if( ubnd[ 1 ] > ubnd0[ 1 ] ) ubnd[ 1 ] = ubnd0[ 1 ];
   }

/* Modify the returned FrameSet to take account of the new pixel origin. */
   gshift[ 0 ] = lbnd0[ 0 ] - lbnd[ 0 ];
   gshift[ 1 ] = lbnd0[ 1 ] - lbnd[ 1 ];
   gshift[ 2 ] = 0.0;
   if( gshift[ 0 ] != 0.0 || gshift[ 1 ] != 0.0 ) {
      astRemapFrame( *wcsout, AST__BASE, astShiftMap( 3, gshift, " " ) );
   }

/* Report the pixel bounds of the cube. */
   if( *status == SAI__OK ) {
      msgOutif( MSG__NORM, " ", " ", status );
      msgSetk( "XL", lbnd[ 0 ] );
      msgSetk( "YL", lbnd[ 1 ] );
      msgSetk( "ZL", lbnd[ 2 ] );
      msgSetk( "XU", ubnd[ 0 ] );
      msgSetk( "YU", ubnd[ 1 ] );
      msgSetk( "ZU", ubnd[ 2 ] );
      msgOutif( MSG__NORM, " ", "   Output cube pixel bounds: ( ^XL:^XU, ^YL:^YU, ^ZL:^ZU )",
                status );

/* Report an error if the bounds look too big (this can happen if the
   telescope looses its mind during an observation). */
      if( ( ubnd[ 0 ] - lbnd[ 0 ] + 1 ) > MAX_DIM ||
          ( ubnd[ 1 ] - lbnd[ 1 ] + 1 ) > MAX_DIM ){
         *status = SAI__ERROR;
         errRep( "", FUNC_NAME ": The spatial extent of the cube is too large. "
                 "Check your list of input data files does not include widely "
                 "separated observations.", status );
      }
   }

/* If no error has occurred, export the returned FrameSet pointer from the
   current AST context so that it will not be annulled when the AST
   context is ended. Otherwise, ensure a null pointer is returned. */
   if( *status == SAI__OK ) {
      astExport( *wcsout );
   } else {
      *wcsout = astAnnul( *wcsout );
   }

/* End the AST context. This will annul all AST objects created within the
   context (except for those that have been exported from the context). */
   astEnd;

/* Issue a context message if anything went wrong. */
   if( *status != SAI__OK ) errRep( FUNC_NAME, "Unable to determine cube "
                                    "bounds", status );
}
