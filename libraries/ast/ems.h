/*
*+
*  Name:
*     ems.h

*  Purpose:
*     Define a dummy interface to the Starlink EMS library.

*  Description:
*     This file defines a dummy interface to the Starlink EMS (Error
*     Message Service) library, as described in Starlink System Note
*     4.  This dummy interface is used to allow the EMS interface for
*     the AST library to build without error, even on systems where
*     EMS is not installed. On systems where EMS is installed, the
*     proper "ems.h" file should be located and used during the build,
*     and this one should not be used.

*  Notes:
*     - Only those EMS functions actually used by the AST library are
*     defined here.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     15-NOV-1996 (RFWS):
*        Original version.
*-
*/

/* Function prototypes. */
/* ==================== */
void ems_rep_c( const char *, const char *, int * );
