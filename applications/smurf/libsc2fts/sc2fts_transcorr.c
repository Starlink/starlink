/*
*+
*  Name:
*     sc2fts_transcorr.c

*  Purpose:
*     Mainly remove PWV effect

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2fts_transcorr ( Grp* igrp, Grp* ogrp, AstKeyMap * parKeymap,
*                        int *status )

*  Arguments:
*     igrp = Grp* (Given)
*        the group of input files
*     ogrp = Grp* (Given)
*        the group of output files
*     parKeymap = AstKeyMap* (Given)
*        the parameter Keymap for this operation. Currently,
*        there are three parameters in parKeymap:
*          AM:  Air Mass
*          PWV: Precipitable Water Vapor (in mm).
*          TAU: the TAU data file name. The values of wet and dry
*               components of TAU are for PWV=1mm and airmass of 1.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Structure of TAU data file which stores dry and wet components of
*     atmosphere. Output of hdstrace:

TAU  <NDF>

   DATA_ARRAY     <ARRAY>         {structure}
      DATA(1)        <_UBYTE>        *
      ORIGIN(1)      <_INTEGER>      1

   MORE           <EXT>           {structure}
      UNIT           <_CHAR*4>       '1/mm'
      FACTOR         <_DOUBLE>       0.0001
      DRY(25001)     <_REAL>         0,1.680301E-9,6.7279533E-9,
                                     ... 0.02712704,0.02691713,0.02676998
      WET(25001)     <_REAL>         0,6.5415251E-10,2.6174807E-9,
                                     ... 238.8973,242.7828,246.7523,250.8079

End of Trace.

*     Here, the wavenumber (=1/wavelength) for DRY(n) or WET(n) is n*FACTOR
*     (in UNIT) (n starts from 0).
*     Combinative TAU = (PWV x AM x TAU_wet) + (AM x TAU_dry)

*  Authors:
*     B.Zhang (UoL)

*  History :
*     2008-03-16 (BZ):
*        Create a test implementation for FTS-2

*  Copyright:
*     Copyright (C) 2008 University of Lethbridge. All Rights Reserved.

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <math.h>

/* STARLINK includes */
#include "ast.h"
#include "star/hds.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf_typ.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* FTS-2 includes */
#include "sc2fts_common.h"

#define FUNC_NAME "sc2fts_transcorr"

void sc2fts_transcorr
(
Grp *igrp,
Grp* ogrp,
AstKeyMap* parKeymap,
int *status          /* global status (given and returned) */
)
{
  int i, j, k, index;
  const char* tau_fn;  /* the file name of TAU */
  float airmass;       /* Air Mass */
  float pwv;           /* Precipitable Water Vapor */
  HDSLoc *loc_tau = NULL;   /* root TAU HDSLoc */
  HDSLoc *loc_more = NULL;  /* HDSLoc to TAU More */
  HDSLoc *loc_dry  = NULL;  /* HDSLoc to TAU More.DRY */
  HDSLoc *loc_wet  = NULL;  /* HDSLoc to TAU More.WET */
  HDSLoc *loc_wn_factor = NULL; /* HDSLoc to TAU More.FACTOR */
  HDSLoc *fts2drloc = NULL;     /* HDSLoc to More.FTS2DR */
  HDSLoc *ftswnloc = NULL;      /* HDSLoc to More.FTS2DR.FTS_WN */
  double tau_wnfactor;          /* TAU's scaling factor of wave number */
  double fts_wnfactor;          /* Spectrum's scaling factor of wave number */
  float *dry_ptr;               /* pointer to Dry component */
  float *wet_ptr;               /* pointer to Wet component */
  float *tau_ptr;               /* pointer to combinative TAU */
  size_t drywet_size;           /* size of dry/wet components of TAU */
  smfData *data;                /* Pointer to in/output SCUBA2 data struct */
  int nwn;                      /* number of spectral wavenumber in input data */
  float *tsm;                   /* Atomspheric Transmittance */
  double *ftswn_rel;            /* spectrum's wavenumber relative to TAU's wavenumber */
  float *tstream = NULL;        /* Pointer to input data stream */
  int bolrow;                   /* Row of bolometer array */
  int bolcol;                   /* Col of bolometer array */
  float intensity;              /* intensity for a specified wavenumber of a pixel */


  /* get the TAU file name */
  if(astMapHasKey( parKeymap, "TAU" ) ==0)
  {
    printf("No TAU file!!!\n");
    return;
  }
  else
  {
    astMapGet0C(parKeymap, "TAU", &tau_fn);
  }
  /* get the value of airmass */
  if(astMapHasKey( parKeymap, "AM" ) ==0)
  {
    printf("No AirMass!!!\n");
    return;
  }
  else
  {
    astMapGet0F(parKeymap, "AM", &airmass);
  }
  /* get the value of PWV */
  if(astMapHasKey( parKeymap, "PWV" ) ==0)
  {
    printf("No PWV!!!\n");
    return;
  }
  else
  {
    astMapGet0F(parKeymap, "PWV", &pwv);
  }

  /* NDF start */
  ndfBegin();

  /* open TAU file */
  hdsOpen(tau_fn, "READ", &loc_tau, status);
  datFind(loc_tau, "MORE", &loc_more, status);
  datFind(loc_more, "DRY", &loc_dry, status);
  datFind(loc_more, "WET", &loc_wet, status);
  datFind(loc_more, "FACTOR", &loc_wn_factor, status);

  /* verify that everything was found */
  if(*status != SAI__OK)
  {
	errRep(FUNC_NAME,	"Tau file format incorrect", status);
	/* close TAU file */
	 datAnnul(&loc_wn_factor, status);
	 datAnnul(&loc_wet,  status);
	 datAnnul(&loc_dry,  status);
	 datAnnul(&loc_more, status);
	 datAnnul(&loc_tau, status);
	return;
  }
  /* get wavenumber */
  datGet0D(loc_wn_factor, &tau_wnfactor, status);

  /* get dry/wet components */
  datSize(loc_dry, &drywet_size, status);

  dry_ptr = astCalloc( drywet_size, sizeof(float), 0 );
  wet_ptr = astCalloc( drywet_size, sizeof(float), 0 );


  datGetVR(loc_dry, drywet_size, dry_ptr,
           &drywet_size, status);
  datGetVR(loc_wet, drywet_size, wet_ptr,
            &drywet_size, status);

  /* generate combinative TAU and e^(-tau) */
  tau_ptr = (float*)astCalloc( drywet_size, sizeof(float), 0 );
  for(i=0; i<drywet_size; i++)
  {
    *(tau_ptr+i) = airmass*(pwv*(*(wet_ptr+i)) + *(dry_ptr+i));
    *(tau_ptr+i) = exp(-(*(tau_ptr+i)));
  }

  /* release memories */
  astFree( dry_ptr );
  astFree( wet_ptr );

  /* close TAU file */
  datAnnul(&loc_wn_factor, status);
  datAnnul(&loc_wet,  status);
  datAnnul(&loc_dry,  status);
  datAnnul(&loc_more, status);
  datAnnul(&loc_tau, status);

   /* open spectrumcube file */
   smf_open_file( ogrp, 1, "UPDATE", SMF__NOCREATE_QUALITY, &data, status );

  /* get the locator to More.FTS2DR */
  fts2drloc = smf_get_xloc( data, "FTS2DR", "EXT", "READ", 0, 0, status );

  /* get the locator to More.FTS2DR.FTS_WN_FACTOR */
  datFind( fts2drloc, "FTS_WN_FACTOR", &ftswnloc, status );

  /* get the scaling factor of spectrum of FTS. The scaling factor is
   * 1/L (L is the double-sided scan length with unit millimeter)
  */
  datGet0D( ftswnloc, &fts_wnfactor, status );

  /* Annual HDSLoc */
  datAnnul( &ftswnloc, status );
  datAnnul( &fts2drloc, status );
  /* verify that everything was found */
  if(*status != SAI__OK)
  {
	errRep(FUNC_NAME,	"Input file structure error", status);

	return;
  }

  if(data->ndims != 3)
  {
    printf("structure of data array is wrong!!!\n");
  }
  else
  {
    nwn = (data->dims)[2];
    tsm = (float*)astCalloc( nwn, sizeof(float), 0 );
    ftswn_rel = (double*)astCalloc( nwn, sizeof(double), 0 );
    for(i=0; i<nwn; i++)
    {
      *(ftswn_rel + i) = i*fts_wnfactor/tau_wnfactor;
    }

    csi_simplified(tau_ptr, drywet_size, ftswn_rel, nwn, tsm);


    /* get bolometer dimension */
    bolrow =  (data->dims)[0];
    bolcol =  (data->dims)[1];

    /* retrieve pointer to the input data */
    tstream = (float*)((data->pntr)[0]);

    for(i=0; i<bolrow; i++)
      for(j=0; j<bolcol; j++)
      {
        for(k=0; k<nwn; k++)
        {
          index = i + bolrow*j + bolrow*bolcol*k;
          intensity = *(tstream+index);
          intensity = intensity/(*(tsm+k));
          *(tstream+index) = intensity;
        }
      }
    /* release memory */
    astFree( tsm );
    astFree( ftswn_rel );
  }

  /* release memories */
  astFree( tau_ptr );
 printf("status = %d\n", *status);
  /* NDF end */
  ndfEnd(status);
}
