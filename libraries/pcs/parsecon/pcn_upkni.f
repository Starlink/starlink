      SUBROUTINE PARSECON_UPKNI( LU, N, ARRAY, START, END, STATUS )
*+
*  Name:
*     PARSECON_UPKNI
 
*  Purpose:
*     To deccode elements of the new-style (packed) compiled form of an
*     interface file for a 2-D INTEGER array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL PARSECON_UPKNI( LU, N, ARRAY, START, END, STATUS )
 
*  Description:
*     The routine reads a record from a new-style .IFC and decodes it
*     into the elements of the given array starting at the STARTth
*     element and ending at the ENDth element in the second dimension.
*     The record will have been packed using the PARSECON_PAKNI
*     routines such that an array NBUFF gives the number of consecutive
*     occurrences of the corresponding element in the array VBUFF.
 
*  Arguments:
*     LU = INTEGER (Given)
*        The logical unit number to read from
*     N = INTEGER (Given)
*        The size of the first dimension
*     ARRAY(N, *) = INTEGER (Given)
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
      INTEGER N
      INTEGER ARRAY( N, * )
      INTEGER START
      INTEGER END
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! Needed for SUBPAR__MAXPAR
 
*  Local Variables:
      INTEGER IOSTAT             ! IO status
      INTEGER VBUFF( 3*SUBPAR__MAXPAR ) ! Values buffer
      INTEGER NBUFF( 3*SUBPAR__MAXPAR ) ! Values count buffer
      INTEGER LASTV               ! The last value handled
      INTEGER NV                 ! The consecutive value counter
      INTEGER BPT                ! Pointer to element in encoded array
      INTEGER APT                ! Pointer to element in ARRAY
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER K                  ! Array index
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Read the encoded record
      READ ( LU, IOSTAT=IOSTAT ) BPT, (NBUFF(I),I=1,BPT),
     : (VBUFF(I),I=1,BPT)
 
      IF ( IOSTAT .NE. 0 ) THEN
*     Read failed
         STATUS = PARSE__READERR
         CALL EMS_FIOER( 'IOSTAT', IOSTAT )
         CALL EMS_REP( 'PCN_UPKN1', 'PARSECON: Read error: ^IOSTAT',
     :    STATUS )
 
      ELSEIF ( BPT .EQ. 0 ) THEN
*     Illegal record
         STATUS = PARSE__READERR
         CALL EMS_REP( 'PCN_UPKN2',
     :   'PARSECON: Invalid compiled interface file record', STATUS )
 
      ELSE
*     Read OK - unpack the record
*     Initialise array indices
         APT = START
         K = 1
 
*     For each element in the encoded arrays
         DO 20, I = 1, BPT
 
*        Get the value count
            NV = NBUFF( I )
*        and the value
            LASTV = VBUFF( I )
 
*        Fill in the required number of elements of ARRAY
            DO 10, J = 1, NV
 
               IF ( APT .GT. END ) THEN
*              Too many elements provided
                  STATUS = PARSE__READERR
                  CALL EMS_REP( 'PCN_UPKN3',
     :            'PARSECON: Too many elements in compiled '//
     :            'interface file', STATUS )
 
*              Exit from loops
                  GO TO 100
 
               ELSE
*              Still within ARRAY - store the value
                  ARRAY( K, APT ) = LASTV
*              Increment indices
                  K = K + 1
                  IF ( K .GT. N ) THEN
                     APT = APT + 1
                     K = 1
                  ENDIF
 
               ENDIF
 
10          CONTINUE
 
20       CONTINUE
 
      ENDIF
 
*  Check that ARRAY is filled
      IF (( K .NE. 1 ) .OR. ( APT-1 .NE. END )) THEN
         STATUS = PARSE__READERR
         CALL EMS_REP( 'PCN_UPKN4',
     :   'PARSECON: Insufficient elements in compiled interface file',
     :    STATUS )
      ENDIF
 
100   CONTINUE
 
      END
