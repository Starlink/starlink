      SUBROUTINE PARSECON_UPKC( LU, ARRAY, START, END, STATUS )
*+
*  Name:
*     PARSECON_UPKC

*  Purpose:
*     To decode elements of the new-style (packed) compiled form of an 
*     interface file for a 1-D CHARACTER array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PARSECON_UPKC( LU, ARRAY, START, END, STATUS )

*  Description:
*     The routine reads a record from a new-style .IFC and decodes it
*     into the elements of the given array starting at the STARTth
*     element and ending at the ENDth element. The record will have
*     been packed using the PARSECON_PACKC routine such that an array
*     NBUFF gives the used length of the corresponding element of the
*     character array.
*     The remainder of each element of ARRAY must then be space-filled.

*  Arguments:
*     LU = INTEGER (Given)
*        The logical unit number to read from
*     ARRAY(*) = <TYPE> (Given)
*        The array of values 
*     START = INTEGER (Given)
*        The first element to be inserted
*     END = INTEGER (Given)
*        The last element to be inserted
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JUL-1991 (AJC):
*        Original version.
*    21-JAN-1992 (AJC):
*        Remove unused declarations
*     24-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'     ! PARSECON status values

*  Arguments Given:
      INTEGER LU
      CHARACTER*(*) ARRAY( * )
      INTEGER START
      INTEGER END

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! Needed for SUBPAR__MAXPAR

*  Local Variables:
      INTEGER IOSTAT             ! IO status
      INTEGER NBUFF( SUBPAR__MAXPAR ) ! String length buffer
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the encoded record
      READ ( LU, IOSTAT=IOSTAT ) (NBUFF(I),I=START,END),
     : (ARRAY(I)(1:NBUFF(I)),I=START,END)

      IF ( IOSTAT .NE. 0 ) THEN
*     Read failed
         STATUS = PARSE__READERR
         CALL EMS_FIOER( 'IOSTAT', IOSTAT )
         CALL EMS_REP( 'PCN_UPKC1', 'Read error: ^IOSTAT', STATUS )

      ELSE
*     Space fill the elements
         DO 10, I = START, END
            ARRAY(I)(NBUFF(I)+1:) = ' '
10       CONTINUE

      ENDIF

100   CONTINUE

      END
