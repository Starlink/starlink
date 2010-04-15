************************************************************************

      SUBROUTINE SKYCON (SKYEST, XCEN, YCEN, A2, A3, E, THETA,
     :                   NX, NY, IMAGE, IMVAR, USEVAR, MASK,
     :                   USEMSK, VALUES, NV, LOCSKY, SIGMA,
     :                   VSKY, MAXSKY, SKY, SKYSIG, AREA,
     :                   SKYARE, EFACT, ASKY, STATUS )

*+
*  Name :
*     SKYCON
*
*  Purpose :
*     This performs the sky measurement for concentric apertures
*
*  Language :
*     FORTRAN
*
*  Invocation :
*
*      CALL SKYCON (SKYEST, XCEN, YCEN, A2, A3, E, THETA,
*     :             NX, NY, IMAGE, IMVAR, USEVAR, MASK,
*     :             USEMSK, VALUES, NV, LOCSKY, SIGMA,
*     :             VSKY, MAXSKY, SKY,SKYSIG, AREA,
*     :             SKYARE, EFACT, ASKY, STATUS  )
*
*  Description :
*     {routine_description}...
*
*  Arguments :
*
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     {enter_new_authors_here}
*
*  History :
*     6-JAN-1998 (AA)
*        Shifted SKY measurement to this subroutine from MEASUR.F
*     07-SEP-2004 (PWD):
*        Changed to use CNF pointers.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'

*  Arguments Given :

      INTEGER MAXSKY
      INTEGER SKYEST
      REAL XCEN, YCEN
      REAL A2, A3, E, THETA
      INTEGER NX, NY
      REAL IMAGE( * )
      REAL IMVAR( * )
      REAL MASK( * )
      LOGICAL USEVAR, USEMSK
      INTEGER NV
      REAL VALUES( MAXSKY )
      REAL LOCSKY, SIGMA, VSKY, SKY
      REAL SKYSIG, AREA, SKYARE, EFACT
      INTEGER ASKY

*  Status :

      INTEGER STATUS

*  Local Variables :

      INTEGER NXH, NXL, NYH, NYL
      INTEGER IV

      CHARACTER * ( DAT__SZLOC ) VLOC
      CHARACTER CODE * 2, TEXT * 72

*.

*   Check status on entry - return if not o.k.
*      WRITE(*,*) ' DEBUG --- --- Entering SKYCON()'
*      WRITE(*,*) ' DEBUG --- --- STATUS = ', STATUS

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Use the user supplied value of sky if requested
      IF ( SKYEST .EQ. 4 ) THEN
         LOCSKY = SKY
         SIGMA = SKYSIG
         SKYARE = AREA

*   Otherwise estimate it from a concentric aperture
      ELSE

*   Calculate the useful area of the grid array to be a box of size
*   twice the largest radius
         NXL = INT( XCEN - A2 - 0.5 )
         NXH = INT( XCEN + A2 + 1.5 )
         NYL = INT( YCEN - A2 - 0.5 )
         NYH = INT( YCEN + A2 + 1.5 )
         IF ( NXL .LT. 1 ) NXL = 1
         IF ( NXH .GT. NX ) NXH = NX
         IF ( NYL .LT. 1 ) NYL = 1
         IF ( NYH .GT. NY ) NYH = NY

*   Estimate the number of pixels that will be used in the sky annulus
*   Add on a few for luck
         ASKY = NINT( EFACT * ( A2 ** 2 - A3 ** 2 ) )
         ASKY = ASKY + 13

*   Find the modal sky value in a ragged annulus
*   If the sky area is less than MAXSKY then pass the VALUES array
         IF ( ASKY .LT. MAXSKY ) THEN
              NV = MAXSKY
*              WRITE(*,*) ' DEBUG --- --- calling RAGGED() asky < maxsky'
              CALL RAGGED ( SKYEST, NX, NY, IMAGE, IMVAR, USEVAR,
     :                      MASK, USEMSK, NXL, NXH, NYL, NYH, XCEN,
     :                      YCEN, A2, A3, E, THETA, VALUES, NV,
     :                      LOCSKY, SIGMA, VSKY )

         ELSE
*   Otherwise get some temporary workspace
              NV = ASKY
              CALL DAT_TEMP('_REAL', 1, NV, VLOC, STATUS)
              CALL DAT_MAPR(VLOC, 'WRITE', 1, NV, IV, STATUS)
*              WRITE(*,*) ' DEBUG --- --- calling RAGGED() asky > maxsky'
              CALL RAGGED( SKYEST, NX, NY, IMAGE, IMVAR, USEVAR,
     :                     MASK, USEMSK, NXL, NXH, NYL, NYH, XCEN,
     :                     YCEN, A2, A3, E, THETA,
     :                     %VAL( CNF_PVAL( IV ) ), NV,
     :                     LOCSKY, SIGMA, VSKY )
              CALL DAT_UNMAP( VLOC, STATUS )
              CALL DAT_ANNUL( VLOC, STATUS )
         ENDIF

*   Use the number of sky pixels as the sky area
         SKYARE = REAL ( NV )

      ENDIF
*
*   End of routine

  99  CONTINUE
*      WRITE(*,*) ' DEBUG --- --- STATUS = ', STATUS
*      WRITE(*,*) ' DEBUG --- --- Leaving SKYCON()'

      END
