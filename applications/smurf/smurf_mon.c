/*
*+
*  Name:
*     smurf_mon

*  Purpose:
*     Top-level SMURF subroutine for A-task monolith on UNIX.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_mon( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the top-level A-task monolith subroutine for the SMURF
*     suite of A-tasks.  Each SMURF command is an alias to a softlink
*     that points to this monolith.  The chosen command is obtained
*     from the ADAM routine TASK_GET_NAME.  The command may be specified
*     from the shell or ICL.  Given the command, the requested A-task
*     is called after a successful matching of the input string with a
*     valid task name.  If there is no match, an error report is made.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     David Berry (JAC, UCLan)
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     COBA: Coskun Oba (UoL)
*     Matt Sherwood (UoL)
*     {enter_new_authors_here}

*  History:
*     2005-09-26 (TIMJ):
*        Initial test version
*     2005-10-05 (TIMJ):
*        Register inherited status pointer with AST
*     2006-01-24 (TIMJ):
*        Use NDF__SZAPP.
*        Check for GRP leaks
*     2006-01-25 (TIMJ):
*        Check for locator leaks.
*     2006-01-30 (TIMJ):
*        Use astBegin/astEnd
*     2006-02-17 (DSB):
*        Switch on AST object caching.
*     2006-02-17 (AGG):
*        Add REMSKY
*     2006-02-24 (DSB):
*        Switch on AST memory caching (instead of Object caching).
*     2006-03-02 (TIMJ):
*        Clear out sc2ast cache
*     2006-03-16 (AGG):
*        Add QLMAKEMAP
*     2006-04-12 (EC):
*        Modified call to createwcs for new interface
*     2006-06-06 (AGG):
*        Add simulator task, SIM (renamed SC2SIM)
*     2006-06-13 (AGG):
*        Add DREAMSOLVE
*     2006-07-28 (TIMJ):
*        Add MAKECUBE
*     2006-08-21 (EC):
*        Add IMPAZTEC
*     2006-09-07 (EC):
*        Modified sc2ast_createwcs calls to use new interface.
*     2006-09-11 (EC):
*        Added call to smf_create_lutwcs to clear cache
*     2006-09-13 (JB):
*        Added BADBOLOS
*     2006-09-15 (AGG):
*        Add DREAMWEIGHTS
*     2006-10-12 (DSB):
*        Added call to smf_detpos_wcs to clear cache.
*     2006-10-26 (AGG):
*        Add STARECALC
*     2006-11-01 (DSB):
*        Added value for new steptime parameter in calls to
*        smf_create_lutwcs and smf_detpos_wcs.
*     2006-11-01 (TIMJ):
*        Add SMURFHELP
*     2007-03-23 (TIMJ):
*        Remove FILE leak checking since it sometimes reports
*        HDS internal scratch files that are not real leaks. Rely
*        solely on the locator leak check.
*     2007-10-19 (TIMJ):
*        Add RAWUNPRESS
*     2007-11-8 (DSB):
*        Add TIMESORT.
*     2008-01-25 (DSB):
*        Add UNMAKECUBE
*     2008-03-16 (BZ)
*        Added action SC2FTS
*     2008-03-27 (EC)
*        Added SC2CLEAN
*     2008-04-09 (TIMJ):
*        API change to create_lutwcs and detpos_wcs
*     2008-04-21 (JB):
*        Add GSDSHOW task.
*     2008-05-23 (TIMJ):
*        Turn on automatic NDG propagation. Factor out task_get_name.
*     2008-05-28 (TIMJ):
*        No longer use automatic provenance propagation. It took too
*        long even if provenance had already been written.
*     2008-07-23 (EC):
*        Add SC2FFT
*     2008-08-14 (TIMJ):
*        Add SMURFCOPY
*     2008-08-20 (TIMJ):
*        Report if the EMS stack level has changed.
*     2008-08-22 (TIMJ):
*        Add CALCDARK
*     2009-03-27 (TIMJ):
*        Add CALCRESP
*     2009-05-19 (TIMJ):
*        Add RAWFIXMETA
*     2009-09-27 (TIMJ):
*        Add STACKFRAMES
*     2009-10-14 (TIMJ):
*        Alphabetize call to tasks.
*     2009-10-16 (DSB):
*        Use ndgBeggh and ndgEndgh to record expanded GRP groups in
*        output NDF history.
*     2010-05-19 (EC):
*        Add SC2EXPANDMODEL
*     2010-07-19 (COBA):
*        Add FTS2_FREQCORR, FTS2_REMOVEBSE, FTS2_SPATIALWCS
*     2010-09-16 (EC):
*        SCANFIT deprecated in favour of SC2CLEAN and KAPPA:MFITTREND
*     2010-09-17 (COBA):
*        Add FTS2_DEGLITCH
*     2010-09-30 (COBA):
*        Add FTS2_EQSLICED
*     2010-11-03 (COBA):
*        Add FTS2SPECRE
*     2011-03-18 (EC):
*        Add SC2PCA
*     2011-06-08 (DSB):
*        Add UNMAKEMAP
*     2011-06-21 (DSB):
*        Use astCheckMemory rather than astFlushMemory since we do not want
*        to free the memory used to hold the singleton workforce returned by
*        thrGetWorkforce.
*     2011-07-18 (COBA):
*         Remove FTS2EQSLICED
*         Remove FTS2SPECRE
*         Add FTS2SPECTRUM
*         Update FTS2PHASECORR
*     2011-08-04 (COBA):
*         Replaced FTS2PHASECORR with FTS2PHASECORRDS and FTS2PHASECORRSS
*     2011-09-22 (EC):
*         Add sc2mapfft
*     2011-08-04 (COBA):
*         Updated FTS2PHASECORR
*     2010-10-24 (EC):
*         Add sc2filtermap
*     2012-04-20 (TIMJ):
*        Remove QLMAKEMAP
*     2012-10-19 (DSB):
*        Use ndfCancl to cancel all newly active NDF parameters at the
*        end of the monolith. This avoid such parameters causing warnings
*        about dangling HDS locators.
*     2013-05-08 (MSHERWOOD):
*         Add FTS2SPLIT
*     2014-03-19 (TIMJ):
*        Call SUPERCAM2ACSIS
*     2014-04-01 (TIMJ):
*        Call NANTEN2ACSIS
*     19-FEB-2020 (DSB):
*        Include used CPU time in logged information.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2012 Science and Technology Facilities Council.
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2005-2008,2010-2011 University of British Columbia.
*     Copyright (C) 2014 Cornell University.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "sae_par.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/grp.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/one.h"
#include "ast.h"
#include "ems.h"
#include "sc2da/sc2ast.h"

#include "libsmurf/smurflib.h"
#include "jcmt/state.h"
#include "libsmf/smf.h"

/* internal protoypes */

void smurf_mon (int * );

/* An AST KeyMap used to pass global parameters around all SMURF
   routines. */

AstKeyMap *smurf_global_keymap = NULL;

/* Main monolith routine */

void smurf_mon( int * status ) {

  /* Local variables */
  char taskname[PAR__SZNAM+1];
  char appname[NDF__SZAPP+1];
  char filter[PAR__SZNAM+PAR__SZNAM+1];
  double junk;                 /* An unused value */
  int cputim[4];               /* Context info for kpg1Cputm */
  int ngrp0;                   /* Number of grp ids at start */
  int ngrp1;                   /* Number of grp ids at end */
  int nloc0;                   /* Number of active HDS Locators at start */
  int nloc1;                   /* Number of active HDS Locators at end */
  int memory_caching;          /* Is AST current caching unused memory? */
  int emslev1;                 /* EMS level on entry */
  int emslev2;                 /* EMS level on exit */

  if ( *status != SAI__OK ) return;

  /* Read the input error message stack level */
  emsLevel( &emslev1 );

  /* Initialise AST */
  astBegin;
  memory_caching = astTune( "MemoryCaching", 1 );

  /* Register our status variable with AST */
  astWatch( status );

  /* If we are watching a particular memory Id reported by astActiveMemory
     we set the watch point here. */
  /* astWatchMemory( 29 ); */

  /* For debugging, watch one of the leaked GRP identifiers listed by the
     call to grpWatch at the end of this routine (if any). */
  /* grpWatch( 3129345, status ); */

  /* Mark any currently active NDF parameters, so that they will
     not be cancelled by the call to ndfCancl at the end of this
     function. */
  ndfCancl( "*", status );

  /* Find out the task name and provenance name we were invoked with */
  smf_get_taskname( taskname, NULL, status );

  /* Get the GRP and HDS status for leak checking - need the task name
     to mask out parameter names. Also need to mask out the monlith name */
  one_strlcpy( filter, "!SMURF_MON,!", sizeof(filter), status);
  one_strlcat( filter, taskname, sizeof(filter), status );
  grpInfoi( NULL, 0, "NGRP", &ngrp0, status );
  hdsInfoI( NULL, "LOCATORS", filter, &nloc0, status );


  /* Update the application name in the NDF history recording
     to include the version number of the application */
  snprintf( appname, NDF__SZAPP, "%-*s (%s V%s)", PAR__SZNAM,
            taskname, PACKAGE_UPCASE, PACKAGE_VERSION);
  ndfHappn( appname, status );

  /* Begin a GRP NDF history block. This causes the contents of GRP
     groups to be appended to default history text added to any NDFs
     during the block. */
  ndgBeggh( status );


  /* Create an AST KeyMap that can be used to pass global parameters around
     all SMURF routines. Unlock it so it can be accessed by any thread. */
  smurf_global_keymap = astKeyMap( "KeyCase=0" );
  astUnlock( smurf_global_keymap, 1 );

  /* Record the current CPU time in CPUTIM. */
  junk = VAL__BADD;
  kpg1Cputm( cputim, &junk );

  /* Call the subroutine associated with the requested task */
  if (strcmp( taskname, "BADBOLOS" ) == 0 ) {
    smurf_extinction( status );
  } else if (strcmp( taskname, "CALCDARK" ) == 0 ) {
    smurf_calcdark( status );
  } else if (strcmp( taskname, "CALCFLAT" ) == 0 ) {
    smurf_calcflat( status );
  } else if (strcmp( taskname, "CALCNOISE" ) == 0 ) {
    smurf_calcnoise( status );
  } else if (strcmp( taskname, "CALCQU" ) == 0 ) {
    smurf_calcqu( status );
  } else if (strcmp( taskname, "CALCRESP" ) == 0 ) {
    smurf_calcresp( status );
  } else if (strcmp( taskname, "CHECKCOORDS" ) == 0 ) {
    smurf_checkcoords( status );
  } else if (strcmp( taskname, "COPYFLAT" ) == 0 ) {
    smurf_copyflat( status );
  } else if (strcmp( taskname, "DREAMSOLVE" ) == 0 ) {
    smurf_dreamsolve( status );
  } else if (strcmp( taskname, "DREAMWEIGHTS" ) == 0 ) {
    smurf_dreamweights( status );
  } else if (strcmp( taskname, "DSUTILS" ) == 0 ) {
    smurf_dsutils( status );
  } else if (strcmp( taskname, "EXTINCTION" ) == 0 ) {
    smurf_extinction( status );
  } else if (strcmp( taskname, "FIT1D" ) == 0 ) {
    smurf_fit1d( status );
  } else if (strcmp( taskname, "FINDSLICES" ) == 0 ) {
    smurf_findslices( status );
  } else if (strcmp( taskname, "FITSMERGE" ) == 0 ) {
    smurf_fitsmerge( status );
  } else if (strcmp( taskname, "FIXSTEPS" ) == 0 ) {
    smurf_fixsteps( status );
  } else if (strcmp( taskname, "FLATFIELD" ) == 0 ) {
    smurf_flatfield( status );
  } else if (strcmp( taskname, "FTS2DEGLITCH" ) == 0 ) {
    smurf_fts2_deglitch( status );
  } else if (strcmp( taskname, "FTS2FLATFIELD" ) == 0 ) {
    smurf_fts2_flatfield( status );
  } else if (strcmp( taskname, "FTS2FREQCORR" ) == 0 ) {
    smurf_fts2_freqcorr( status );
  } else if (strcmp( taskname, "FTS2SPLIT" ) == 0 ) {
    smurf_fts2_split( status );
  } else if (strcmp( taskname, "FTS2INIT" ) == 0 ) {
    smurf_fts2_init( status );
  } else if (strcmp( taskname, "FTS2MASKMAP" ) == 0 ) {
    smurf_fts2_maskmap( status );
  } else if (strcmp( taskname, "FTS2OPCORR" ) == 0 ) {
    smurf_fts2_spatialwcs( status );
  } else if (strcmp( taskname, "FTS2PHASECORR" ) == 0 ) {
    smurf_fts2_phasecorr( status );
  } else if (strcmp( taskname, "FTS2PHASECORRDS" ) == 0 ) {
    smurf_fts2_phasecorrds( status );
  } else if (strcmp( taskname, "FTS2PORTIMBAL" ) == 0 ) {
    smurf_fts2_portimbal( status );
  } else if (strcmp( taskname, "FTS2REMOVEBSE" ) == 0 ) {
    smurf_fts2_removebse( status );
  } else if (strcmp( taskname, "FTS2SPECTRUM" ) == 0 ) {
    smurf_fts2_spectrum( status );
  } else if (strcmp( taskname, "FTS2TRANSCORR" ) == 0 ) {
    smurf_fts2_transcorr( status );
  } else if (strcmp( taskname, "GAU2FIT" ) == 0 ) {
    smurf_gau2fit( status );
  } else if (strcmp( taskname, "GSD2ACSIS" ) == 0 ) {
    smurf_gsd2acsis( status );
  } else if (strcmp( taskname, "GSDSHOW" ) == 0 ) {
    smurf_gsdshow( status );
  } else if (strcmp( taskname, "IMPAZTEC" ) == 0 ) {
    smurf_impaztec( status );
  } else if (strcmp( taskname, "JSADICER" ) == 0 ) {
    smurf_jsadicer( status );
  } else if (strcmp( taskname, "JSAPASTER" ) == 0 ) {
    smurf_jsapaster( status );
  } else if (strcmp( taskname, "JSATILEINFO" ) == 0 ) {
    smurf_jsatileinfo( status );
  } else if (strcmp( taskname, "JSATILELIST" ) == 0 ) {
    smurf_jsatilelist( status );
  } else if (strcmp( taskname, "MAKECUBE" ) == 0 ) {
    smurf_makecube( status );
  } else if (strcmp( taskname, "MAKEMAP" ) == 0 ) {
    smurf_makemap( status );
  } else if (strcmp( taskname, "NANTEN2ACSIS" ) == 0 ) {
    smurf_nanten2acsis( status );
  } else if (strcmp( taskname, "POL2CHECK" ) == 0 ) {
    smurf_pol2check( status );
  } else if (strcmp( taskname, "POL2IPCOR" ) == 0 ) {
    smurf_pol2ipcor( status );
  } else if (strcmp( taskname, "QUCOVAR" ) == 0 ) {
    smurf_qucovar( status );
  } else if (strcmp( taskname, "RAWFIXMETA" ) == 0 ) {
    smurf_rawfixmeta( status );
  } else if (strcmp( taskname, "RAWPRESS" ) == 0 ) {
    smurf_rawpress( status );
  } else if (strcmp( taskname, "RAWRECREATEWCS" ) == 0 ) {
    smurf_rawrecreatewcs( status );
  } else if (strcmp( taskname, "RAWREWRTSC2WCS" ) == 0 ) {
    smurf_rawrewrtsc2wcs( status );
  } else if (strcmp( taskname, "RAWUNPRESS" ) == 0 ) {
    smurf_rawunpress( status );
  } else if (strcmp( taskname, "REMSKY" ) == 0 ) {
    smurf_remsky( status );
  } else if (strcmp( taskname, "SC2CLEAN" ) == 0 ) {
    smurf_sc2clean( status );
  } else if (strcmp( taskname, "SC2CONCAT" ) == 0 ) {
    smurf_sc2concat( status );
  } else if (strcmp( taskname, "SC2EXPANDMODEL" ) == 0 ) {
    smurf_sc2expandmodel( status );
  } else if (strcmp( taskname, "SC2FFT" ) == 0 ) {
    smurf_sc2fft( status );
  } else if (strcmp( taskname, "SC2FILTERMAP" ) == 0 ) {
    smurf_sc2filtermap( status );
  } else if (strcmp( taskname, "SC2MAPFFT" ) == 0 ) {
    smurf_sc2mapfft( status );
  } else if (strcmp( taskname, "SC2PCA" ) == 0 ) {
    smurf_sc2pca( status );
  } else if (strcmp( taskname, "SC2SIM" ) == 0 ) {
    smurf_sc2sim( status );
  } else if (strcmp( taskname, "SC2THREADTEST" ) == 0 ) {
    smurf_sc2threadtest( status );
  } else if (strcmp( taskname, "SKYNOISE" ) == 0 ) {
    smurf_skynoise( status );
  } else if (strcmp( taskname, "SMURFCOPY" ) == 0 ) {
    smurf_smurfcopy( status );
  } else if (strcmp( taskname, "SMURFHELP" ) == 0 ) {
    smurf_smurfhelp( status );
  } else if (strcmp( taskname, "STACKFRAMES" ) == 0 ) {
    smurf_stackframes( status );
  } else if (strcmp( taskname, "STARECALC" ) == 0 ) {
    smurf_starecalc( status );
  } else if (strcmp( taskname, "SUPERCAM2ACSIS" ) == 0 ) {
    smurf_supercam2acsis( status );
  } else if (strcmp( taskname, "TIMESORT" ) == 0 ) {
    smurf_timesort( status );
  } else if (strcmp( taskname, "UNMAKECUBE" ) == 0 ) {
    smurf_unmakecube( status );
  } else if (strcmp( taskname, "UNMAKEMAP" ) == 0 ) {
    smurf_unmakemap( status );
  } else {
    *status = SAI__ERROR;
    msgSetc( "TASK", taskname );
    errRep( "smurf_mon", "Unrecognized taskname: ^TASK", status);
  }


  /* Lock the global parameters keymap so we can annul it. */
  astLock( smurf_global_keymap, 0 );

  /* End the GRP NDF history block. */
  ndgEndgh( status );

  /* Log the task and its parameters to a log file specified by enviromnent
     variable CUPID_LOG. */
  kpg1Lgcmd( taskname, "SMURF", cputim, status );

  /* Clear cached info from sc2ast_createwcs. */
  sc2ast_createwcs(SC2AST__NULLSUB, NULL, NULL, NULL, NO_FTS, NULL, status);

  /* Clear WVM caches (one for each thread). */
  smf_calc_wvm_clear( status );

  /* Free AST resources */
  astTune( "MemoryCaching", memory_caching );
  astEnd;

  /* Check for GRP leaks Do this in a new error reporting context so
   * that we get the correct value even if an error has occurred. */
  errBegin( status );
  grpInfoi( NULL, 0, "NGRP", &ngrp1, status );

  /* If there are more active groups now than there were on entry,
   * there must be a problem (GRP identifiers are not being freed
   * somewhere). So report it. */
  if (*status == SAI__OK && ngrp1 > ngrp0) {
    msgBlank( status );
    msgSetc( "NAME", taskname );
    msgSeti( "NGRP0", ngrp0 );
    msgSeti( "NGRP1", ngrp1 );
    msgOut( " ", "WARNING: The number of active "
            "GRP identifiers increased from ^NGRP0 to ^NGRP1 "
            "during execution of ^NAME (" PACKAGE_UPCASE " programming "
            " error).", status);
    msgBlank(status);
    grpWatch( 0, status );
  }
  errEnd( status );

  /* The NDF library registers locators with SUBPAR for any NDFs that
     are opened directly using ndfAssoc or ndfExist. These locators are
     only annulled when the associated parameters are cancelled, but most
     smurf applications do not explicitly cancel their NDF parameters.
     This means that such locators are picked up by the following check
     for dangling HDS locators. In order to prevent this, we cancel any
     remaining NDF parameters now, excluding any that were marked by the
     call to ndfCancl at the start of this routine. */
  ndfCancl( " ", status );

  /* Check for HDS leaks Do this in a new error reporting context so
   * that we get the correct value even if an error has occurred. */
  errBegin( status );
  hdsInfoI( NULL, "LOCATORS", filter, &nloc1, status );

  /* If there are more active locators now than there were on entry,
   * there must be a problem (HDS locators are not being freed
   * somewhere). So report it. */
  if (*status == SAI__OK && nloc1 > nloc0) {
    msgBlank( status );
    msgSetc( "NAME", taskname );
    msgSeti( "NLOC0", nloc0 );
    msgSeti( "NLOC1", nloc1 );
    msgOut( " ", "WARNING: The number of active "
            "HDS Locators increased from ^NLOC0 to ^NLOC1 "
            "during execution of ^NAME (" PACKAGE_UPCASE " programming "
            " error).", status);
    msgBlank(status);
    hdsShow("LOCATORS", status);
    hdsShow("FILES", status);
    printf("filter - %s\n",filter);
  }
  errEnd( status );

  /* Read the exitt error message stack level */
  emsLevel( &emslev2 );

  if (*status == SAI__OK && emslev1 != emslev2 ) {
    errMark();
    msgBlank( status );
    msgSetc( "NAME", taskname );
    msgSeti( "LV1", emslev1);
    msgSeti( "LV2", emslev2);
    msgOut( " ", "WARNING: EMS Stack level went from ^LV1 to ^LV2"
            " during execution of ^NAME (" PACKAGE_UPCASE " programming"
            " error).", status );
    msgBlank(status);
    errRlse();
  }

  /* configure AST --with-memdebug, and uncomment the following lines
     to see how much memory usage SMURF hit at its peak */
  /*
  {
    size_t memcurrent,mempeak;
    astMemoryStats( 0, &mempeak, &memcurrent );
    msgOutf( "", "SMURF: === current /peak memory usage: %zu / %zu MiB ===",
             status, memcurrent/SMF__MIB, mempeak/SMF__MIB );
  }
  */

  /* The astCheckMemory function does nothing unless AST has been compiled
   * with the MEM_DEBUG flag. If this is the case, then it reports the number
   * of memory blocks that have not been freed (useful for identifying memory
   * leaks). Use astActiveMemory() below to list all active memory and
   * then use astWatchMemory() at the start of this routine to get reports
   * when a particular ID is used. Set a breakpoint in the debugger for
   * astMemoryAlarm_
   */
  astActiveMemory("Exit:");
  astCheckMemory;
}
