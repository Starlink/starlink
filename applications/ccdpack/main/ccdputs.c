/*
*+
*  Name:
*     ccdlog

*  Type of Module:
*     C extension to Tcl.

*  Language:
*     ANSI C.

*  Purpose:
*     Allows output from Tcl via the CCDPACK logging system.

*  Usage:
*     ccdlog ?options? message ?name?

*  Description:
*     The message supplied to this command is output via the CCDPACK 
*     logging system, which in turn sends its output via the ADAM
*     messaging system.  It will invoke either the CCD1_MSG or the
*     CCD1_ERREP routines, according to whether the '-error' flag is
*     submitted.

*  Arguments:
*     options
*        If present, the '-error' option will cause the output to get
*        sent to the CCD1_ERREP routine.  Otherwise, the output will 
*        get sent to CCD1_MSG.
*     message = string
*        This string will get output as a message.
*     name = string
*        If present, this gives the name of the error message (as passed
*        in the first argument of CCD1_MSG or CCD1_ERREP).  Otherwise the
*        message is given an empty name.

*  Authors:
*     MBT: Mark Taylor (STARLINK)

*  History:
*     21-SEP-2000 (MBT):
*        Original version.

*-
*/

#include <stdio.h>
#include "sae_par.h"
#include "tcl.h"
#include "cnf.h"
#include "mers.h"
#include "msg_par.h"

/**********************************************************************/
   int CcdlogCmd( ClientData clientData, Tcl_Interp *interp, int objc,
                  Tcl_Obj *CONST objv[] ) {
/**********************************************************************/
      int nflag;
      int errmsg;
      int mleng;
      int nleng;
      int status[ 1 ];
      char *msg;
      char *name;
      DECLARE_CHARACTER( fmsg, MSG__SZMSG );
      DECLARE_CHARACTER( fname, MSG__SZMSG );

/* Process flags. */
      errmsg = 0;
      nflag = 0;
      if ( objc > 1 + nflag && 
           ! strcmp( Tcl_GetString( objv[ 1 + nflag ] ), "-error" ) ) {
         errmsg = 1;
         nflag++;
      }

/* Check syntax. */
      if ( objc + nflag < 2 || objc + nflag > 3 ) {
         Tcl_WrongNumArgs( interp, 1, objv, "?options? msg ?name?" );
         return TCL_ERROR;
      }

/* Get string arguments. */
      msg = Tcl_GetStringFromObj( objv[ 1 + nflag ], &mleng );
      if ( objc + nflag == 3 ) {
         name = Tcl_GetStringFromObj( objv[ 2 + nflag ], &nleng );
      }
      else {
         name = " ";
         nleng = 1;
      }

/* Turn string arguments into fortran-friendly strings. */
      cnfExprt( msg, fmsg, mleng );
      cnfExprt( name, fname, nleng );

/* Begin an error context in a clean state. */
      errMark();
      *status = SAI__OK;

/* Call the appropriate routine. */
      if ( errmsg ) {
         F77_CALL(ccd1_errep)( CHARACTER_ARG(fname), CHARACTER_ARG(fmsg),
                               INTEGER_ARG(status)
                               TRAIL_ARG(fname) TRAIL_ARG(fmsg) );
      }
      else {
         F77_CALL(ccd1_msg)( CHARACTER_ARG(fname), CHARACTER_ARG(fmsg),
                             INTEGER_ARG(status)
                             TRAIL_ARG(fname) TRAIL_ARG(fmsg) );
      }

/* The call failed - this is probably because the CCDPACK messaging system
   was not enabled (ccdwish was freestanding rather than this being called
   somewhere within a CCDPACK Atask).  In any case, we arrange to output
   the error to the screen and assume that this will be enough.  This 
   strategy could result in a duplicated error message.  So kill me. */
      if ( *status != SAI__OK ) {
         errAnnul( status );
         printf( "%s\n", msg );
      }

/* Release the error context. */
      errRlse();

/* Set result and exit successfully. */
      Tcl_SetObjResult( interp, Tcl_NewStringObj( "", 0 ) );
      return TCL_OK;
   }


/* $Id$ */
