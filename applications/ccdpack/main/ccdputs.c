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

*  Notes:
*     This command works by writing commands up the pipe to the parent
*     process, if ccdwish is runningin pipes mode.  Therefore its
*     implementation is closely tied to the interprocess communication
*     code in the ccdwish binary.

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
#include "ccdtcl.h"

   extern int ccdifd;
   extern int ccdofd;

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

/* There are two possibilities: either we are running as a subprocess,
   or we are running free standing.  Find out which. */
      if ( ccdofd >= 0 ) {

/* We are running as a subprocess.  Write the message in an appropriate
   format back up the pipe to the parent. */
         int log_flag;
         log_flag = errmsg ? CCD_CCDERR : CCD_CCDMSG;
         write( ccdofd, &log_flag, sizeof( int ) );
         write( ccdofd, name, nleng );
         write( ccdofd, "\n", 1 );
         write( ccdofd, msg, mleng + 1 );
      }
      else {

/* We are running freestanding.  Simply output the message to standard 
   output. */
         printf( "%s\n", msg );
      }

/* Set result and exit successfully. */
      Tcl_SetObjResult( interp, Tcl_NewStringObj( "", 0 ) );
      return TCL_OK;
   }


/* $Id$ */
