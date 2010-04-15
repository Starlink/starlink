      SUBROUTINE IFLDZ2(RA, DEC, SIZE, RACNR, DECCNR, STATUS)
*+

*  Name:
*     IFLDZ2

*  Purpose:
*     Calculates the coordinates of the 4 corners of a box

*  Language:
*     Starlink Fortran 77

*  Type of module:
*     Subroutine to an ADAM A-task

*  Invocation:
*     CALL IFLDZ2(RA, DEC, SIZE, RACNR, DECCNR, STATUS)

*  Description:
*     This routine takes as input equatorial coordinate and a box
*     size, and returns the coordinates of the 4 corners of the box,
*     centred on the input coordinate.
*     The indices of the corners are as follows:
*          C1    C3
*
*             C
*
*          C2    C4

*  Notes:
*     This routine uses the sla_lib routine SLA_DTP2S to translate from the
*     box size which is on the tangent plane at the specified RA and Dec, to
*     the spherical equivalent. If one were to give the SLA_DTP2S routine a
*     box size of 1 degree it would take this as if it were at the equator,
*     it would give spherical coordinate extremities of 4deg RA x 1deg Dec at
*     a Dec of 75deg (nb. cos(75)=.25). Though this is technically correct
*     we felt that users were likely to want a box of approx 1deg square at
*     75deg Dec and therefore the RA size is multiplied by cos(dec) in each
*     case.

*  Parameters:
*     RA = DOUBLEPRECISION (Read)
*       The RA of the input coordinate in equatorial radians.
*
*     DEC = DOUBLEPRECISION (Read)
*       The DEC of the input coordinate in equatorial radians.
*
*     SIZE = DOUBLEPRECISION (Read)
*       The box size in radians.
*
*     RACNR = DOUBLEPRECISION(4) (Write)
*       The RA of the 4 corners in equatorial radians.
*
*     DECCNR = DOUBLEPRECISION(4) (Write)
*       The DEC of the 4 corners in equatorial radians.
*
*     STATUS = INTEGER (Read/Write)
*       The global status.

*  Authors:
*     HM: Huw Morris (IPMAF)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     20-DEC-1994:
*       Original version.
*     9-FEB-1995:
*       Original version gave wrong values modified to use SLA_DTP2S
*     {enter_changes_here}

*  Bugs:
*     None known.
*     {enter_any_bugs_here}

*-


c  No implicit typing
      IMPLICIT NONE

c  Standard includes
      INCLUDE 'SAE_PAR'
      INCLUDE 'IRA_PAR'

c  Parameters
      DOUBLEPRECISION RA,       ! RA and DEC(Eq 1950) in radians
     *                DEC       !  of the box centre

      DOUBLEPRECISION SIZE      ! Box size in radians

      DOUBLEPRECISION RACNR(4), ! RA in radians of the corners
     *                DECCNR(4) ! DEC in radians of the corners

      INTEGER STATUS            ! Global status

c  Local variables
      DOUBLEPRECISION RATMP,    ! Unnormalised RA in radians
     *                DECTMP    ! Unnormalised DEC in radians

      DOUBLEPRECISION I2, J2    ! Corner coordinates
      INTEGER INDEX             ! Corner number

      INTEGER I, J              ! Loop variables

      DOUBLEPRECISION RA2, DEC2 ! Temporary variable used for testing

c  Procedures used
      DOUBLEPRECISION sla_DRANGE, sla_DRANRM

*  Check global status
       IF (STATUS .NE. SAI__OK) GOTO 999

*  Calculate the cartesian coordinates of the 4 corners
       DO I = 0, 1
         DO J = 0, 1

           INDEX = (I*2) + J + 1            ! The index of the current corner

*  If in the northern hemisphere, the axes are reversed
           IF (DEC .GT. 0.0) INDEX = 5 - INDEX

*  Get the cartesian coordinates of the corners
           I2 = ( (I * SIZE) - (SIZE / 2.0) ) * COS( DEC )
           J2 = (J * SIZE) - (SIZE / 2.0)

*  Use SLA_DTP2S to transform the tangent plane coordinates to spherical
*  coordinates
           CALL SLA_DTP2S( I2, J2, RA, DEC, RATMP, DECTMP )

*  Normalise the RA into 0-2PI
           RACNR(INDEX) = SLA_DRANRM(RATMP)

*  Normalise the DEC into +-PI
           DECCNR(INDEX) = SLA_DRANGE(DECTMP)

*  The following 3 lines are for testing purposes.
           RA2  = RACNR(INDEX)  * IRA__RTOD
           DEC2 = DECCNR(INDEX) * IRA__RTOD
*           PRINT *, 'INDEX: C',INDEX,'RA:', RA2, ' DEC:',DEC2

         ENDDO
       ENDDO

 999   CONTINUE

       END
