*+
*  Name:
*     CCD1_CLOG

*  Purpose:
*     CCDPACK log file system common block

*  Language:
*     Starlink Fortran 77

*  Type of module:
*     Include file for declaring common block.

*  Description:
*     CCDPACK log file system include file. This file declares the
*     common block  /CCD1_CLOG/CCD1_BUFF,CCD1_ILEV which controls the
*     echoing of any output strings to the logfile and terminal.

*  Notes:
*     - includes the MSG system parameters to define the size of the
*       character buffer

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-JUN-1991 (PDRAPER):
*        Original version.
*     1-NOV-1991 (PDRAPER):
*        Extended system to include new options for not getting any
*        output.
*     {enter_further_changes_here}

*-

*  Global Constants:
      INCLUDE 'MSG_PAR'            ! Size of the MSG system buffer

*  Global Variables:
      CHARACTER * ( MSG__SZMSG ) CCD1_BUFF ! MSG system buffer
      INTEGER CCD1_ILEV          ! Log system interaction level

*  Global common block:
*  Separate characters and others
      COMMON/ CCD1_CLOG1 / CCD1_BUFF
      COMMON/ CCD1_CLOG2 / CCD1_ILEV
* $Id$
