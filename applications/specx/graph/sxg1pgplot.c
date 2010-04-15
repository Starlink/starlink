/*+
* Name: SXG_WRDEV
*
* Purpose:
*    A Fortran-callable function to enter a device into the SPECX devices
*    structure.
*
* Language:
*    C
*
* Invocation:
*    CALL SXG_WRDEV(
*   :     ENTRY, DEVNO, DEVNAME, PROMPT, TERM, DUAL, COLOUR, HARD, FILE, IERR )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*    DEVNO = INTEGER (Given)
*       The device number
*    DEVNAME = CHARACTER*(*) (Given)
*       The name of the device
*    PROMPT = CHARACTER*(*) (Given)
*       The device prompt
*    TERM = LOGICAL (Given)
*       If it's a terminal
*    DUAL = LOGICAL (Given)
*       If it's a dual screen
*    COLOUR = LOGICAL (Given)
*       If it's a colour device
*    HARD = LOGICAL (Given)
*       If it's a hardcopy device
*    FILE = CHARACTER*(*) (Given)
*       The name of the file
*    IERR = INTEGER (Returned)
*       Non-zero if error
*-
*/
#include "f77.h"
#include "sxg_graphcap.h"

F77_SUBROUTINE(sxg_wrdev)( INTEGER(entry), INTEGER(devno),
   CHARACTER(devname), CHARACTER(prompt),
   LOGICAL(term), LOGICAL(dual), LOGICAL(colour), LOGICAL(hard),
   CHARACTER(file), INTEGER(ierr)
   TRAIL(devname) TRAIL(prompt) TRAIL(file) ) {

   *ierr = 0;
   devices[*entry].dev_no = *devno;
   cnf_imprt( devname, devname_length, devices[*entry].devname );
   cnf_imprt( prompt, prompt_length, devices[*entry].prompt );
   devices[*entry].term = *term;
   devices[*entry].dual = *dual;
   devices[*entry].colour = *colour;
   devices[*entry].hard = *hard;
   cnf_imprt( file, file_length, devices[*entry].file );

}
/*+
* Name: SXG_GTDEV
*
* Purpose:
*    A Fortran-callable function to get device info from the SPECX devices
*    structure.
*
* Language:
*    C
*
* Invocation:
*    CALL SXG_GTDEV(
*   :     ENTRY, DEVNO, DEVNAME, PROMPT, TERM, DUAL, COLOUR, HARD, FILE, IERR )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*    DEVNO = INTEGER (Returned)
*       The device number
*    DEVNAME = CHARACTER*(*) (Returned)
*       The name of the device
*    PROMPT = CHARACTER*(*) (Returned)
*       The device prompt
*    TERM = LOGICAL (Returned)
*       If it's a terminal
*    DUAL = LOGICAL (Returned)
*       If it's a dual screen
*    COLOUR = LOGICAL (Returned)
*       If it's a colour device
*    HARD = LOGICAL (Returned)
*       If it's a hardcopy device
*    FILE = CHARACTER*(*) (Returned)
*       The name of the file
*    IERR = INTEGER (Returned)
*       Non-zero if error
*-
*/

F77_SUBROUTINE(sxg_gtdev)( INTEGER(entry), INTEGER(devno),
   CHARACTER(devname), CHARACTER(prompt),
   LOGICAL(term), LOGICAL(dual), LOGICAL(colour), LOGICAL(hard),
   CHARACTER(file), INTEGER(ierr)
   TRAIL(devname) TRAIL(prompt) TRAIL(file) ) {

   *ierr = 0;
   *devno = devices[*entry].dev_no;
   cnf_exprt( devices[*entry].devname, devname, devname_length );
   cnf_exprt( devices[*entry].prompt, prompt, prompt_length );
   *term = devices[*entry].term;
   *dual = devices[*entry].dual;
   *colour = devices[*entry].colour;
   *hard = devices[*entry].hard;
   cnf_exprt( devices[*entry].file, file, file_length );

}
/*+
* Name: SXG_GTPR
*
* Purpose:
*    A Fortran-callable function to get the prompt for a device from the
*    SPECX devices structure.
*
* Language:
*    C
*
* Invocation:
*    CALL SXG_GTPR( ENTRY, PROMPT, IERR )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*    PROMPT = CHARACTER*(*) (Returned)
*       The device prompt
*    IERR = INTEGER (Returned)
*       Non-zero if error
*-
*/

F77_SUBROUTINE(sxg_gtpr)( INTEGER(entry), CHARACTER(prompt), INTEGER(ierr)
   TRAIL(prompt) ) {

   *ierr = 0;
   cnf_exprt( devices[*entry].prompt, prompt, prompt_length );

}
/*+
* Name: SXG_INQDEVNO
*
* Purpose:
*    A Fortran-callable function to get a device num,ber from the SPECX devices
*    structure.
*
* Language:
*    C
*
* Invocation:
*    CALL SXG_INQDEVNO( ENTRY )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*
* Return value:
*    SXG_INQDEVNO = INTEGER
*       The device number of the ENTRY.
*-
*/

F77_INTEGER_FUNCTION(sxg_inqdevno)( INTEGER(entry) ) {

   return devices[*entry].dev_no;

}
/*+
* Name: SXG_INQTERM
*
* Purpose:
*    A Fortran-callable function to inquire from the SPECX devices
*    structure whether the device is a terminal.
*
* Language:
*    C
*
* Invocation:
*    CALL SXG_INQTERM( ENTRY )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*
* Return value:
*    SXG_INQTERM = LOGICAL
*       .TRUE if the device is a terminal
*-
*/

F77_LOGICAL_FUNCTION(sxg_inqterm)( INTEGER(entry) ) {

   return devices[*entry].term;

}
/*+
* Name: SXG_INQHARD
*
* Purpose:
*    A Fortran-callable function to inquire from the SPECX devices
*    structure whether the device is a hardcopy device.
*
* Language:
*    C
*
* Invocation:
*    CALL SXG_INQHARD( ENTRY )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*
* Return value:
*    SXG_INQHARD = LOGICAL
*       .TRUE if the device is a hardcopy device
*-
*/

F77_LOGICAL_FUNCTION(sxg_inqhard)( INTEGER(entry) ) {

   return devices[*entry].hard;

}
