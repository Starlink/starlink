#include <stdlib.h>
#include "f77.h"
#include "tcl.h"
#include "tk.h"
#include "tclAdam.h"
#include "tkGwm.h"
#include "tkNbs.h"

/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

#ifdef NEED_MATHERR
extern int matherr();
int *tclDummyMathPtr = (int *) matherr;
#endif

#define SAI__OK 0
#define SAI__ERROR 148013867
#define PACK_DIR "POLPACK_DIR"
#define TCL_SCRIPT "/PolReg.tcl"

extern F77_SUBROUTINE(grp_grpsz)( INTEGER(igrp), INTEGER(size),
                                  INTEGER(status) );
extern F77_SUBROUTINE(grp_infoc)( INTEGER(igrp), INTEGER(index),
                                  CHARACTER(item), CHARACTER(value),
                                  INTEGER(status) TRAIL(item) TRAIL(value) );
extern F77_SUBROUTINE(err_rep)( CHARACTER(param), CHARACTER(mess),
                                INTEGER(STATUS) TRAIL(param) TRAIL(mess) );

void Error( const char *, int * );
const char *Envir( const char *, int * );
void SetVar( Tcl_Interp *, char *, char *, int, int * );
const char *GetVar( Tcl_Interp *, char *, int, int * );
char *GetName( int, int, int * );

F77_SUBROUTINE(doplrg)( INTEGER(IGRP1), INTEGER(IGRP2), INTEGER(IGRP3), 
                        INTEGER(DPI), LOGICAL(HAREA),
                        LOGICAL(SAREA), INTEGER(PSF),
                        CHARACTER(SI), INTEGER(FIT),
                        INTEGER(OEFIT), CHARACTER(LOGFIL),
                        CHARACTER(BADCOL), CHARACTER(CURCOL), 
                        CHARACTER(REFCOL), CHARACTER(SELCOL), 
                        CHARACTER(VIEW), REAL(PLO), REAL(PHI), LOGICAL(NEWCM),
                        LOGICAL(XHAIR), CHARACTER(XHRCOL), LOGICAL(STHLP),
                        INTEGER(STATUS) TRAIL(SI) TRAIL(LOGFIL) TRAIL(BADCOL)
                        TRAIL(CURCOL) TRAIL(REFCOL) TRAIL(SELCOL)
                        TRAIL(VIEW) TRAIL(XHRCOL) ){
/*
*  Name:
*     doplrg

*  Purpose:
*     Activates the main PolReg tcl script.

*  Description:
*     This C function creates a Tcl interpreter to execute the Tcl script
*     which implements the GUI for the PolReg application. Values for 
*     various user preferences are communicated to the Tcl script by 
*     initialising various Tcl variables to hold the supplied options
*     values. When the Tcl script terminates, the (potentially modified)
*     options values are read back from these Tcl variables, and passed 
*     back to the caller.

*  Parameters:
*     IGRP1 = INTEGER (Given)
*        The GRP identifier for the group holding the input image names
*        to pass to the TCL script.
*     IGRP2 = INTEGER (Given)
*        The GRP identifier for the group holding the O-ray output image 
*        names to pass to the TCL script.
*     IGRP3 = INTEGER (Given)
*        The GRP identifier for the group holding the E-ray output image 
*        names to pass to the TCL script. In single-beam mode, this should
*        be supplied equal to IGRP2.
*     DPI = INTEGER (Given)
*        The screen dots per inch to use. If a zero or negative value
*        is supplied, then the TK default is used.
*     HAREA = LOGICAL (Given and Returned)
*        Should the help area be displayed?
*     SAREA = LOGICAL (Given and Returned)
*        Should the status area be displayed?
*     LOG = LOGICAL (Given and Returned)
*        Should the images be displayed logarithmically?
*     LOGFIL = CHARACTER * ( * ) (Given)
*        The name of a log file in which to store all messages generated
*        by any ATASKs activated by the TCL script.
*     PSF = INTEGER (Given and Returned)
*        The size of star-like features in the image (in pixels).
*     SI = CHARACTER * ( * ) (Given and Returned)
*        A character string specifying which status items to display,
*        and in what order.
*     FIT = INTEGER (Given abd Returned)
*        The fit type (1-5) for aligning images.
*     OEFIT = INTEGER (Given abd Returned)
*        The fit type (1-5) for aligning the O and E rays.
*     BADCOL = CHARACTER (Given and Returned)
*        The colour with which to mark missing pixel data. The supplied
*        variable should be long enough to receive the longest colour name.
*     CURCOL = CHARACTER (Given and Returned)
*        The colour with which to mark the current objects. The supplied
*        variable should be long enough to receive the longest colour name.
*     REFCOL = CHARACTER (Given and Returned)
*        The colour with which to mark the reference objects. The supplied
*        variable should be long enough to receive the longest colour name.
*     SELCOL = CHARACTER (Given and Returned)
*        The colour with which to mark the selected area. The supplied
*        variable should be long enough to receive the longest colour name.
*     VIEW = CHARACTER (Given and Returned)
*        How should the images be viewded? : Aligned or Stacked.
*     PLO = REAL (Given and Returned)
*        The lower percentile for the image display.
*     PHI = REAL (Given and Returned)
*        The upper percentile for the image display.
*     NEWCM = LOGICAL (Given)
*        Use a private colour map?
*     XHAIR = LOGICAL (Given and Returned)
*        Is a cross-hair required over the image display area?
*     XHRCOL = CHARACTER (Given and Returned)
*        The colour with which to draw the cross-hair (if required). The 
*        supplied variable should be long enough to receive the longest 
*        colour name.
*     STHLP = LOGICAL (Given)
*        Should a hyper-text browser be created automatically at start-up
*        displaying the help system contents?
*     STATUS = INTEGER (Given and Returned)
*        The inherited global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-APR-1997 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

   GENPTR_INTEGER(IGRP1)
   GENPTR_INTEGER(IGRP2)
   GENPTR_INTEGER(IGRP3)
   GENPTR_INTEGER(DPI)
   GENPTR_LOGICAL(HAREA)
   GENPTR_LOGICAL(SAREA)
   GENPTR_INTEGER(PSF)
   GENPTR_CHARACTER(SI)
   GENPTR_INTEGER(FIT)
   GENPTR_INTEGER(OEFIT)
   GENPTR_INTEGER(STATUS)
   GENPTR_CHARACTER(LOGFIL)
   GENPTR_CHARACTER(BADCOL)
   GENPTR_CHARACTER(CURCOL)
   GENPTR_CHARACTER(REFCOL)
   GENPTR_CHARACTER(SELCOL)
   GENPTR_REAL(PLO)
   GENPTR_REAL(PHI)
   GENPTR_CHARACTER(VIEW)
   GENPTR_LOGICAL(NEWCM)
   GENPTR_LOGICAL(XHAIR)
   GENPTR_CHARACTER(XHRCOL)
   GENPTR_LOGICAL(STHLP)

   Tcl_Interp *interp = NULL;
   char *col = NULL;
   char *dir = NULL;
   char *logfil = NULL;
   char *name = NULL;
   char *script = NULL;
   char *si = NULL;
   char *view = NULL;
   char text[80];
   const char *tp;
   int code;
   int dirlen;
   int i;
   int j;
   int n;                      
   int size;

#if ( (TK_MAJOR_VERSION == 4) && (TK_MINOR_VERSION == 0) )
   Tk_Window main;
#endif

/* Check the global status. */
   if( *STATUS != SAI__OK ) return;

/* Create a TCL interpreter. */
   interp = Tcl_CreateInterp ();

/* Get the number of images to process. */
   F77_CALL(grp_grpsz)( INTEGER_ARG(IGRP1), INTEGER_ARG(&size),
                        INTEGER_ARG(STATUS) );

/* Store the name of the input images in Tcl variable "in_list". */
   for( i = 1; i <= size && *STATUS == SAI__OK; i++ ){
      name = GetName( *IGRP1, i, STATUS );
      SetVar( interp, "in_list", name, TCL_LEAVE_ERR_MSG | TCL_LIST_ELEMENT | TCL_APPEND_VALUE, STATUS );
   }

/* Store the name of the O-ray output images in Tcl variable "o_list". */
   for( i = 1; i <= size && *STATUS == SAI__OK; i++ ){
      name = GetName( *IGRP2, i, STATUS );
      SetVar( interp, "o_list", name, TCL_LEAVE_ERR_MSG | TCL_LIST_ELEMENT | TCL_APPEND_VALUE, STATUS );
   }

/* Do the same for the E-ray output images (if in dual-beam mode). */
   if( *IGRP3 != *IGRP2 ) {
      for( i = 1; i <= size && *STATUS == SAI__OK; i++ ){
         name = GetName( *IGRP3, i, STATUS );
         SetVar( interp, "e_list", name, TCL_LEAVE_ERR_MSG | TCL_LIST_ELEMENT | TCL_APPEND_VALUE, STATUS );
      }
   }

/* If a positive value has been supplied, store the screen dots per inch
   to use in TCL variable "dpi". */
   if ( *DPI > 0 ) { 
      sprintf( text, "%d", *DPI );
      SetVar( interp, "dpi", text, TCL_LEAVE_ERR_MSG, STATUS );
   }

/* Store the number of "argv" values in "argc". */
   SetVar( interp, "argc", "3", TCL_LEAVE_ERR_MSG, STATUS );

/* If a WWW browser is to be created at start-up define the START_HELP
   variable. */
   if( F77_ISTRUE(*STHLP) ) {
      SetVar( interp, "START_HELP", "1", TCL_LEAVE_ERR_MSG, STATUS );
   }

/* Set the Tcl variables storing the options values to use. */
   SetVar( interp, "ATASK_XHAIR", ( F77_ISTRUE(*XHAIR) ? "1" : "0" ), 
           TCL_LEAVE_ERR_MSG, STATUS );

   SetVar( interp, "ATASK_HAREA", ( F77_ISTRUE(*HAREA) ? "1" : "0" ), 
           TCL_LEAVE_ERR_MSG, STATUS );

   SetVar( interp, "ATASK_SAREA", ( F77_ISTRUE(*SAREA) ? "1" : "0" ), 
           TCL_LEAVE_ERR_MSG, STATUS );

   sprintf( text, "%d", *PSF );
   SetVar( interp, "ATASK_PSF", text, TCL_LEAVE_ERR_MSG, STATUS );

   si = (char *) malloc ( sizeof(char)*(size_t) ( SI_length + 1 ) );
   if ( si ) {
      memcpy( si, SI, SI_length );
      si[ SI_length ] = 0;
      SetVar( interp, "ATASK_SI", si, TCL_LEAVE_ERR_MSG, STATUS );
      free( si );
   }

   if( LOGFIL_length > 0 ) {
      logfil = (char *) malloc ( sizeof(char)*(size_t) ( LOGFIL_length + 1 ) );
      if ( logfil ) {
         memcpy( logfil, LOGFIL, LOGFIL_length );
         logfil[ LOGFIL_length ] = 0;
         SetVar( interp, "ATASK_LOGFILE", logfil, TCL_LEAVE_ERR_MSG, STATUS );
         free( logfil );
      }
   }

   view = (char *) malloc ( sizeof(char)*(size_t) ( VIEW_length + 1 ) );
   if ( view ) {
      memcpy( view, VIEW, VIEW_length );
      view[ VIEW_length ] = 0;
      SetVar( interp, "ATASK_VIEW", view, TCL_LEAVE_ERR_MSG, STATUS );
      free( view );
   }

   col = (char *) malloc ( sizeof(char)*(size_t) ( XHRCOL_length + 1 ) );
   if ( col ) {
      memcpy( col, XHRCOL, XHRCOL_length );
      col[ XHRCOL_length ] = 0;
      SetVar( interp, "ATASK_XHRCOL", col, TCL_LEAVE_ERR_MSG, STATUS );
      free( col );
   }

   col = (char *) malloc ( sizeof(char)*(size_t) ( BADCOL_length + 1 ) );
   if ( col ) {
      memcpy( col, BADCOL, BADCOL_length );
      col[ BADCOL_length ] = 0;
      SetVar( interp, "ATASK_BADCOL", col, TCL_LEAVE_ERR_MSG, STATUS );
      free( col );
   }

   col = (char *) malloc ( sizeof(char)*(size_t) ( CURCOL_length + 1 ) );
   if ( col ) {
      memcpy( col, CURCOL, CURCOL_length );
      col[ CURCOL_length ] = 0;
      SetVar( interp, "ATASK_CURCOL", col, TCL_LEAVE_ERR_MSG, STATUS );
      free( col );
   }

   col = (char *) malloc ( sizeof(char)*(size_t) ( REFCOL_length + 1 ) );
   if ( col ) {
      memcpy( col, REFCOL, REFCOL_length );
      col[ REFCOL_length ] = 0;
      SetVar( interp, "ATASK_REFCOL", col, TCL_LEAVE_ERR_MSG, STATUS );
      free( col );
   }

   col = (char *) malloc ( sizeof(char)*(size_t) ( SELCOL_length + 1 ) );
   if ( col ) {
      memcpy( col, SELCOL, SELCOL_length );
      col[ SELCOL_length ] = 0;
      SetVar( interp, "ATASK_SELCOL", col, TCL_LEAVE_ERR_MSG, STATUS );
      free( col );
   }

   sprintf( text, "%d", *FIT );
   SetVar( interp, "ATASK_FIT", text, TCL_LEAVE_ERR_MSG, STATUS );

   sprintf( text, "%d", *OEFIT );
   SetVar( interp, "ATASK_OEFIT", text, TCL_LEAVE_ERR_MSG, STATUS );

   sprintf( text, "%g", *PLO );
   SetVar( interp, "ATASK_PLO", text, TCL_LEAVE_ERR_MSG, STATUS );

   sprintf( text, "%g", *PHI );
   SetVar( interp, "ATASK_PHI", text, TCL_LEAVE_ERR_MSG, STATUS );

/* If required, indicate that a private colour map should be used by
   Tcl creating the variable NEWCOLMAP. */
   if( F77_ISTRUE(*NEWCM) ) {
      SetVar( interp, "NEWCOLMAP", "1", TCL_LEAVE_ERR_MSG, STATUS );
   }

#if ( (TK_MAJOR_VERSION == 4) && (TK_MINOR_VERSION == 0) )

/* Create the main window, and report an error if it fails. */
   if( *STATUS == SAI__OK ) {
      main = Tk_CreateMainWindow(interp, NULL, "PolReg", "POLREG" );
      if( !main ) {
         *STATUS = SAI__ERROR;
         Error( "Unable to crete main Tk window.", STATUS );
         Error( interp->result, STATUS );
      }
   }

#endif

/* Get the value of the package directory environment variable, and
   construct the full name of the TCL script. Also, set the TCL variable
   POLREG_DIR to the directory path. */
   dir = (char *) Envir( PACK_DIR, STATUS );      
   if( *STATUS == SAI__OK ){
      dirlen = strlen( dir );
      script = (char *) malloc( (size_t) ( dirlen + strlen( TCL_SCRIPT )  
                                           + 1 ) );
      if( !script ) {
         *STATUS = SAI__ERROR;
         Error( "Failed to allocate memory for full TCL script name.", 
                 STATUS );
      } else {
         strcpy( script, dir );
         strcpy( script + dirlen, TCL_SCRIPT );
         SetVar( interp, "POLREG_DIR", dir, TCL_LEAVE_ERR_MSG |
                 TCL_GLOBAL_ONLY, STATUS );
      }
   }

/* Initialise Tcl, Tk and StarTcl commands. */
   if( *STATUS == SAI__OK ) {

      if( Tcl_Init( interp ) != TCL_OK ) {
         *STATUS = SAI__ERROR;
         Error( "Failed to initialise Tcl commands.", STATUS );
         Error( interp->result, STATUS );

      } else if( Tk_Init( interp ) != TCL_OK ) {
         *STATUS = SAI__ERROR;
         Error( "Failed to initialise Tk commands.", STATUS );
         Error( interp->result, STATUS );

      } else if( Tcladam_Init( interp ) != TCL_OK ) {
         *STATUS = SAI__ERROR;
         Error( "Failed to initialise ADAM Tcl commands.", STATUS );
         Error( interp->result, STATUS );

      } else if( Tkgwm_Init( interp ) != TCL_OK ) {
         *STATUS = SAI__ERROR;
         Error( "Failed to initialise GWM Tk commands.", STATUS );
         Error( interp->result, STATUS );

      } else if( Tknbs_Init( interp ) != TCL_OK ) {
         *STATUS = SAI__ERROR;
         Error( "Failed to initialise NBS Tk commands.", STATUS );
         Error( interp->result, STATUS );
      }
   }

/* Execute the TCL script. */
   if( *STATUS == SAI__OK ){
      if( Tcl_EvalFile( interp, script ) != TCL_OK ){
         *STATUS = SAI__ERROR;
         Error( "Failed to execute the TCL script...", STATUS );
         Error( interp->result, STATUS );

/* If succesfull, loop infinitely, waiting for commands to execute.  When 
   there are no windows left, the loop exits. NOTE, it seems that an
   "exit" command in the tcl script causes the current process to be
   killed. In order to shutdown the script and return control to this
   procedure, use "destroy ." in the script instead of "exit". */
      } else {
         Tk_MainLoop(); 
      }
   }

/* Get the current value of the Tcl options variables. */
   tp = GetVar( interp, "ATASK_XHAIR", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp && !strcmp( tp, "0" ) ) {
      *XHAIR = F77_FALSE;
   } else {
      *XHAIR = F77_TRUE;
   }

   tp = GetVar( interp, "ATASK_HAREA", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp && !strcmp( tp, "0" ) ) {
      *HAREA = F77_FALSE;
   } else {
      *HAREA = F77_TRUE;
   }

   tp = GetVar( interp, "ATASK_SAREA", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp && !strcmp( tp, "0" ) ) {
      *SAREA = F77_FALSE;
   } else {
      *SAREA = F77_TRUE;
   }

   tp = GetVar( interp, "ATASK_PSF", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      sscanf( tp, "%d", PSF );
   }

   tp = GetVar( interp, "ATASK_SI", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      for( i = 0; i < SI_length; i++ ) SI[ i ] = ' ';
      memcpy( SI, tp, strlen( tp ) );
   }

   tp = GetVar( interp, "ATASK_FIT", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      sscanf( tp, "%d", FIT );
   }

   tp = GetVar( interp, "ATASK_OEFIT", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      sscanf( tp, "%d", OEFIT );
   }

   tp = GetVar( interp, "ATASK_PLO", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      sscanf( tp, "%g", PLO );
   }

   tp = GetVar( interp, "ATASK_PHI", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      sscanf( tp, "%g", PHI );
   }

   tp = GetVar( interp, "ATASK_VIEW", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      for( i = 0; i < VIEW_length; i++ ) VIEW[ i ] = ' ';
      memcpy( VIEW, tp, strlen( tp ) );
   }

   tp = GetVar( interp, "ATASK_XHRCOL", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      for( i = 0; i < XHRCOL_length; i++ ) XHRCOL[ i ] = ' ';
      memcpy( XHRCOL, tp, strlen( tp ) );
   }

   tp = GetVar( interp, "ATASK_BADCOL", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      for( i = 0; i < BADCOL_length; i++ ) BADCOL[ i ] = ' ';
      memcpy( BADCOL, tp, strlen( tp ) );
   }

   tp = GetVar( interp, "ATASK_CURCOL", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      for( i = 0; i < CURCOL_length; i++ ) CURCOL[ i ] = ' ';
      memcpy( CURCOL, tp, strlen( tp ) );
   }

   tp = GetVar( interp, "ATASK_REFCOL", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      for( i = 0; i < REFCOL_length; i++ ) REFCOL[ i ] = ' ';
      memcpy( REFCOL, tp, strlen( tp ) );
   }

   tp = GetVar( interp, "ATASK_SELCOL", TCL_LEAVE_ERR_MSG, STATUS );
   if ( tp ) {
      for( i = 0; i < SELCOL_length; i++ ) SELCOL[ i ] = ' ';
      memcpy( SELCOL, tp, strlen( tp ) );
   }

#if ( (TK_MAJOR_VERSION == 4) && (TK_MINOR_VERSION == 0) )

/* If an error has occurred, ensure that the main Tk window has been
   destroyed. */
   if( *STATUS != SAI__OK && main ) Tk_DestroyWindow( main );

#endif

/* Delete the TCL interpreter. */
   if( interp && *STATUS == SAI__OK ) Tcl_DeleteInterp( interp );

/* Free the memory holding the TCL script name. */
   if( script ) free( script );

}

void Error( const char *text, int *STATUS ) {
/*
*  Name:
*     Error

*  Purpose:
*     Report an error using EMS.

*  Description:
*     The supplied text is used as the text of the error message.
*     A blank parameter name is used.

*  Parameters:
*     text
*        The error message text. Only the first 80 characters are used.
*     STATUS
*        A pointer to the global status value. This should have been set
*        to a suitable error value before calling this function.

*  Notes:
*     - If a NULL pointer is supplied for "text", no error is reported.
*/

   DECLARE_CHARACTER(param,1);
   DECLARE_CHARACTER(mess,80);
   int j;

/* Check the supplied pointer. */
   if( text ) {

/* Set the parameter name to a blank string. */
      param[0] = ' ';

/* Copy the first "mess_length" characters of the supplied message into 
      "mess". */
      strncpy( mess, text, mess_length );

/* Pad any remaining bytes with spaces (and replace the terminating null
   character with a space). */
      for( j = strlen(mess); j < mess_length; j++ ) {
         mess[ j ] = ' ';
      }

/* Report the error. */
      F77_CALL(err_rep)( CHARACTER_ARG(param), CHARACTER_ARG(mess),
                         INTEGER_ARG(STATUS) TRAIL_ARG(param) 
                         TRAIL_ARG(mess) );
   }
}

const char *Envir( const char *var, int *STATUS ){
/*
*  Name:
*     Envir

*  Purpose:
*     Get an environment variable.

*  Description:
*     A pointer to the a string holding the value of the specified 
*     environment variable is returned. A NULL pointer is returned an an
*     error is reported if the variable does not exist.

*  Parameters:
*     var
*        The variable name.
*     STATUS
*        A pointer to the global status value. 

*/
   const char *ret;
   char mess[80];

   if( *STATUS != SAI__OK || !var ) return NULL;

   ret = getenv( var );      
   if( !ret ) {
      *STATUS = SAI__ERROR;
      sprintf( mess, "Failed to get environment variable \"%s\".", var );
      Error( mess, STATUS );
   }

   return ret;
}

void SetVar( Tcl_Interp *interp,  char *name,  char *value, 
             int flags, int *STATUS ){
/*
*  Name:
*     SetVar

*  Purpose:
*     Sets a Tcl variable.

*  Description:
*     This is equivalent to the Tcl function Tcl_SetVar, except that
*     it checks the global status before executing, and reports an error
*     if anything goes wrong.

*  Parameters:
*     As for Tcl_SetVar, except for addition of final STATUS argument.
*     
*/

   char mess[80];

   if( *STATUS != SAI__OK ) return;

   if( !Tcl_SetVar( interp, name, value, flags) ){
      *STATUS = SAI__ERROR;
      sprintf( mess, "Error setting TCL variable \"%s\".", name );
      Error( mess, STATUS );
      Error( interp->result,  STATUS );     
   }
}

const char *GetVar( Tcl_Interp *interp,  char *name,  int flags, int *STATUS ){
/*
*  Name:
*     GetVar

*  Purpose:
*     Gets a Tcl variable.

*  Description:
*     This is equivalent to the Tcl function Tcl_GetVar, except that
*     it checks the global status before executing, and reports an error
*     if anything goes wrong.

*  Parameters:
*     As for Tcl_GetVar, except for addition of final STATUS argument.
*     
*/

   char mess[80];
   const char *ret;

   if( *STATUS != SAI__OK ) return NULL;

   ret = Tcl_GetVar( interp, name, flags );
   if ( !ret ) {
      *STATUS = SAI__ERROR;
      sprintf( mess, "Error getting TCL variable \"%s\".", name );
      Error( mess, STATUS );
      Error( interp->result,  STATUS );     
   }

   return ret;

}

char *GetName( int igrp, int i, int *STATUS ) {
/*
*  Name:
*     GetName

*  Purpose:
*     Gets an element out of a GRP group.

*  Description:
*     This function returns a pointer to a null-terminated C string holding 
*     an element of a supplied GRP group.

*  Parameters:
*     igrp = int (Given)
*        The GRP identifier for the group.
*     i = int (Given)
*        The index of the element to return.
*     STATUS = *int (Given and Returned)
*        The inherited status.

*  Returned Value:
*     A pointer to a static string holding the element. This string should not 
*     be modified or freed by the caller.
*     
*/

   DECLARE_CHARACTER(item,4);
   DECLARE_CHARACTER(name,256);
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(I);
   static char buffer[256];
   char *ret;
   int j;

/* Check the inherited status. */
   if( *STATUS != SAI__OK ) return NULL;

/* Store a Fortran string with the value "NAME" for use with GRP_INFOC. */
   item[0] = 'N';
   item[1] = 'A';
   item[2] = 'M';
   item[3] = 'E';

/* Get the name from the group. */
   IGRP = igrp;
   I = i;

   F77_CALL(grp_infoc)( INTEGER_ARG(&IGRP), INTEGER_ARG(&I),
                        CHARACTER_ARG(item), CHARACTER_ARG(name),
                        INTEGER_ARG(STATUS) TRAIL_ARG(item)
                        TRAIL_ARG(name) );

/* Replace all trailing blank characters in the returned Fortran string with 
   null characters. */
   if( *STATUS == SAI__OK ) {
      strcpy( buffer, name );
      for( j = name_length - 1; j >= 0; j-- ) {
         if( isspace( (int) buffer[j] ) ) {
            buffer[j] = 0;
         } else {
            break;
         }
      }
      ret = buffer;
   } else {
      ret = NULL;
   }

/* Return the pointer. */
   return ret;
}
