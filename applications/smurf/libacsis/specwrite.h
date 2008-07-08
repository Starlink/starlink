
#ifndef INCLUDED_HDS_SPEC_H
#define INCLUDED_HDS_SPEC_H

/*
*+
*  Name:
*     hdsspec.h

*  Purpose:
*     Define the public C interface to the HDS spectrum writing ACSIS functions

*  Invocation:
*     #include "acsis/specwrite.h"

*  Language:
*     C Include file

*  Description:
*     This module defines the public interface to the HDS spectrum
*     writing functions used by ACSIS.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     27-FEB-2006 (TIMJ):
*        Original version.
*     21-APR-2006 (TIMJ):
*        nchans now used in specWrite not specOpen.
*     07-JUL-2006 (TIMJ):
*        acsSpecWriteTS now returns a result indicating whether the
*        spectrum was treated as a science or calibration spectrum.
*     26-JUL-2006 (TIMJ):
*        acsSpecOpenTS now gets focal plane information.
*     09-OCT-2006 (TIMJ):
*        Add TCS_TAI
*     11-OCT-2006 (TIMJ):
*        Add FE_LOFREQ and FE_DOPPLER
*     01-JAN-2007 (TIMJ):
*        Synchronize with jcmt/state.h
*     01-FEB-2007 (TIMJ):
*        Add acsSpecWriterVersion
*     09-MAY-2007 (TIMJ):
*        Add OCS config string to OpenTS

*  Copyright:
*     Copyright (C) 2006-2007 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*-
*/

#include "jcmt/state.h"

/* Public data structures */

/* Structure for ACSIS meta data (see jcmt/state.h for State structure information */
typedef struct ACSISSpecHdr {
  unsigned int rts_endnum;  /* Highest number expected in this sequence */
  double acs_feedx;  /* X coordinate of feed "acs_feed" */
  double acs_feedy;  /* Y coordinate of feed "acs_feed" */
  unsigned int acs_feed;  /* Feed number */
  float  acs_tsys;
  float  acs_trx;
} ACSISSpecHdr;

typedef enum backend_type {
   ACS__BACKEND_NONE,
   ACS__BACKEND_ACSIS,
   ACS__BACKEND_DAS,
   ACS__BACKEND_AOS,
} backend_type;


/* NDF versions of the Spectrum writing */

/* Initialise the file for writing */
void acsSpecOpenTS( const char * dir,
		    unsigned int yyyymmdd,
		    unsigned int obsnum,
		    unsigned int nrecep,
		    unsigned int nsubsys,
		    const char * const recepnames[],
		    const char* focal_station,
		    const float fplanex[],  /* Arcsec offsets in X, Y of */
		    const float fplaney[],  /* each receptor. */
		    const char* ocsconfig, /* XML configuration */
		    int * status );

/* Write a spectrum to the file */
int acsSpecWriteTS( unsigned int subsys,
		    unsigned int nchans,
		    const float spectrum[],
		    const JCMTState * record,
		    const ACSISSpecHdr * spechdr,
		    int * status );

/* Close the file */
void acsSpecCloseTS( AstFitsChan * const fits[],
		     int incArchiveBounds,
		     int * status );

/* Library version number */
int acsSpecWriterVersion( void );

/* Set the backend type flag */
void acsSpecSetBackend ( backend_type type, 
                         int *status );

/* Overrides default memory allocation. */
void acsSpecSetMem ( const int nBytes, 
                     int *status );

/* INCLUDE_HDS_SPEC_H */
#endif
