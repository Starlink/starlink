      SUBROUTINE IFLDZ0 (LON, LAT, PLATE, CRA, CDEC, NUMPLT, STATUS)

*+

*  Name:
*     IFLDZ0

*  Purpose:
*     Calculates which ISSA plates contain a given coordinate

*  Language:
*     Starlink Fortran 77

*  Type of module:
*     Subroutine to an ADAM A-task

*  Invocation:
*     CALL IFLDZ0 (LON, LAT, PLATE, CRA, CDEC, NUMPLT, STATUS)

*  Description:
*     This routine takes an Eq(1950) co-ordinate and calculates all
*     of the ISSA plate which contain that co-ordinate. The list is
*     ordered such that the best plate is first in the list.

*  Notes:
*     - This program works by the fact that ISSA plates are seperated
*       by 10 arcminutes between centres. The plates are numbered in
*       'rows', where each row covers an area of 10 arcminutes of latitude.
*       The lowest plate number on each row is centered on 0h 0m 0s longitude.
*       Obviously, the nearer the equator, the more plates there are in the
*       row. By calculating which row the plate lies on, and then how many
*       plates along from 0h, the plate number can be determined.

*  Parameters:
*     LON = DOUBLEPRECISION (Read)
*       The longitude of the co-ordinate used to determine the plate
*
*     LAT = DOUBLEPRECISION (Read)
*       The latitude of the co-ordinate used to determine the plate
*
*     PLATE = INTEGER(4) (Write)
*       The plate number of the desired plate
*
*     CRA = DOUBLE(4) (Write)
*       The RA of the centre of each plate found
*
*     CDEC = DOUBLE(4) (Write)
*       The DEC of the centre of each plate found
*
*     NUMPLT = INTEGER (Write)
*       Number of plates found
*
*     STATUS = INTEGER (Read and Write)
*       The global status

*  Authors:
*     HM: Huw Morris (IPMAF)

*  History:
*     07-MAR-1994:
*       Original version.
*     30-NOV-1994:
*       Added release II information
*     30-JAN-1995:
*       Calls sperate routine to determine if a given point lies on
*       a given plate

*  Bugs:
*     {note_any_bugs_here}

*-

*  No implicit typing
      IMPLICIT NONE

*  Include various constants used by ISFIELD
      INCLUDE 'IRA_PAR'
      INCLUDE 'IRA_ERR'
      INCLUDE 'SAE_PAR'


c  Parameters
      DOUBLEPRECISION LON,    ! Input longitude, in radians
     *                LAT     ! Input latitude, in radians


      INTEGER PLATE (4)       ! The desired plate number
      INTEGER NUMPLT          ! Number of plates found (0 - 4)

      DOUBLEPRECISION CRA(4), ! |The centre of the plate (in degrees)
     *                CDEC(4) ! |

      INTEGER STATUS          ! Global status


c  Internal variables
      INTEGER PLAROW (19)     ! The number of plates in each row
      INTEGER STAROW (19)     ! The plate number in each row centre 0h 0m
      DOUBLEPRECISION WIDTHS(19) ! The widths of the plates in each row

      DOUBLEPRECISION RA      ! The Eq.RA (degrees) of the point required
      DOUBLEPRECISION DEC     ! The Eq.DEC (degrees) of the point required

      DOUBLEPRECISION D2     ! Transformation of the DEC
      INTEGER ROW(2)         ! The row number 1-19 the desired plate is on
                             ! A point may be on two different rows

      DOUBLEPRECISION SEP    ! The span on hours of each plate on the row
      DOUBLEPRECISION SEP2   ! Half the span
      DOUBLEPRECISION RA2    ! The RA offset by SEP2
      INTEGER OFFSET(2,2)    ! The number of plates along the current row
                             ! The point my be on two offsets for each row

      DOUBLEPRECISION CENROW ! The DEC of the centre of the current row

      DOUBLEPRECISION CNTRA(2,2), ! The RA of the centre of each plate per row
     *                CNTDEC(2)   ! The DEC of the centre of each row

      DOUBLEPRECISION SPAN,   ! The size of the centre to the edge of the plate
     *                OFFBAK, ! Temporary offset
     *                RABAK,  ! Temporary RA
     *                CENTRE, ! The centre RA, offset by SEP2
     *                OVERLAP ! The overlap of the last plate in each row

      LOGICAL INSIDE          ! Whether a point lies in a given plate or not

      INTEGER I, J           ! Loop variables

c  Set up the data
c  The number of plates in each row
      DATA PLAROW / 1, 8, 14, 19, 24, 28, 32, 35, 36, 36, 36, 35,
     *              32, 28, 24, 19, 14, 8, 1 /

c  The plate number of the first plate in each row
      DATA STAROW / 1, 2, 10, 24, 43, 67, 95, 127, 162, 198, 234,
     *              270, 305, 337, 365, 389, 408, 422, 430 /

c  Widths in arcminutes of the plate centres in each row
      DATA WIDTHS / 360.0, 45.0, 26.0, 19.0, 15.0, 13.0, 11.5, 10.5,
     *              10.0, 10.0, 10.0, 10.5, 11.5, 13.0, 15.0, 19.0,
     *              26.0, 45.0, 360.0 /

c  Check global status
      IF (STATUS .NE. SAI__OK) GOTO 999

c  No plates found yet
      NUMPLT = 0

c  Mark rows and offsets as not found
      DO  I = 1, 2
        ROW(I) = -1
        DO J = 1, 2
          OFFSET(I, J) = -1
          PLATE(I*2+J-2) = -1
        ENDDO
      ENDDO

c  Convert RA and DEC from radians into degrees
      RA = LON * IRA__RTOD
      DEC = LAT * IRA__RTOD

c  First find out which row has the best plate (1-19) from the declination
c  Each row covers 10 degrees of declination

c  Scale the declination from 0 - 180, where 0 is the south pole,
c  90 is the equator and 180 is the north pole
      D2 = DEC + 90.0

c  Find the nearest multiple of 10 to the scaled declination
c  and store that as the best row number
      ROW(1) = INT((D2+5.0)/10.0) + 1

c  Calculate the centre DEC of this plate
      CNTDEC(1) = (ROW(1)-10) * 10.0

c  ROW(1) is the row number of the desired plate, in the range 1 - 19

c  Check that 1 <= ROW(1) <= 19
      IF ((ROW(1) .LT. 1) .OR. (ROW(1) .GT. 19)) THEN

c  Abort if the row is out of range
        STATUS = SAI__ERROR
        CALL ERR_REP('IFLD_BADR',
     *       'Arithmetic screwed up calculating the row.', STATUS)
        GOTO 999
      ENDIF

c  If the point also lies in another row, see which row it will be
      ROW(2) = -1
      IF (DEC .GT. CNTDEC(1)) THEN
        IF (ROW(1) .LT. 19) THEN
          ROW(2) = ROW(1) + 1
          CNTDEC(2) = (ROW(2)-10) * 10.0
        ENDIF
      ELSE
        IF (ROW(1) .GT. 1) THEN
          ROW(2) = ROW(1) - 1
          CNTDEC(2) = (ROW(2)-10) * 10.0
        ENDIF
      ENDIF

c  Now find which column in each row from the right ascension

c  Calculate the number of plates found thus far
      NUMPLT = 1
      IF (ROW(2) .NE. -1) NUMPLT = 2

c  Loop over all the rows in which the point is found
      DO I = 1, NUMPLT

c  Calculate the span in degrees of each plate in row ROW
        SEP = WIDTHS(ROW(I))

c  Some rows do not have have a number of plates that divides into 360.
c  This means the last plate in each row is not quite at the same seperation
c  with the first plate, as with all the other plates on the row
        OVERLAP = (SEP * PLAROW(ROW(I))) - 360.0

c  Calculate half of the span of each plate
        SEP2 = SEP / 2.0

c  By adding half of the span to the RA we shift from the plate
c  centre to the plate left hand edge.
        RA2 = (RA + SEP2)

c  Keep the RA in the range 0 <= RA < 360
        IF (RA2 .GE. 360.0) THEN
          RA2 = RA2 - 360.0
        ENDIF

c  Calculate how many plates along the current row the desired plate lies
c  This is the best offset for the current row
        OFFSET(I,1) = INT (RA2/SEP)

c  Check the validity of OFFSET
        IF ((OFFSET(I,1) .LT. 0) .OR.
     *         (OFFSET(I,1) .GE. PLAROW(ROW(I)))) THEN

c  Abort if an error calculating the offset
          STATUS = SAI__ERROR
          CALL ERR_REP('IFLD_BADO',
     *     'Arithmetic screwed up calculating the offset.',STATUS)
          GOTO 999
        ENDIF

c  Calculate the RA of the centre of the plate
        CNTRA(I,1) = OFFSET(I,1) * SEP
        CENTRE = CNTRA(I,1) + SEP2

c  If the current row is not the top or bottom row, there is a chance
c  that the point also lies on a plate to the left or the right of the
c  current one.
        IF (ROW(I) .NE. 1 .AND. ROW(I) .NE. 19) THEN

          IF (CENTRE .GT. RA2) THEN
c  If at all, point also lies on right hand plate
              OFFSET(I,2) = OFFSET(I,1) - 1

c  Check for looping round
              IF (OFFSET(I,2) .LT. 0.0)
     +             OFFSET(I,2) = PLAROW(ROW(I)) -1
              CNTRA(I,2) = OFFSET(I,2) * SEP

          ELSE

c  Do the same for the left hand plate
            OFFSET(I,2) = OFFSET(I,1) + 1

c  Check for looping around
            IF (OFFSET(I,2) .GE. PLAROW(ROW(I)))
     +             OFFSET(I,2) = 0
            CNTRA(I,2) = OFFSET(I,2) * SEP

          ENDIF
        ENDIF

      ENDDO

c  Now we have all the plate rows and offsets, we just need to sort them
c  and count how many plates

      NUMPLT = 0
      DO I = 1, 2                           ! Loop over each row
        DO J = 1, 2                         ! Loop over each offset in the row

c  If the offset at this row is set, this check if this is a valid plate
          IF (OFFSET(I,J) .NE. -1) THEN
            CALL IFLDZ3(CNTRA(I,J), CNTDEC(I), RA, DEC,
     +                  INSIDE, STATUS)
            IF (INSIDE) THEN
              NUMPLT = NUMPLT + 1
              PLATE(NUMPLT) = STAROW(ROW(I)) + OFFSET(I,J)
              CRA(NUMPLT) = CNTRA(I,J)
              CDEC(NUMPLT) = CNTDEC(I)
              ENDIF
          ENDIF

        ENDDO
      ENDDO

 999  CONTINUE

      END
