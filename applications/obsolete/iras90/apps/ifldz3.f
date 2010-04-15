      SUBROUTINE IFLDZ3(RACNT, DECCNT, RA, DEC, INSIDE, STATUS)
*+
*  Name:
*    IFLDZ3

*  Purpose:
*    Calculates whether a given point is within a given plate

*-

*  No implicit typing
      IMPLICIT NONE

*  Includes
      INCLUDE 'SAE_PAR'
      INCLUDE 'IRA_PAR'

*  Parameters:
      DOUBLEPRECISION RACNT,         ! RA and Dec of the plate centre
     +                DECCNT         !  in degrees

      DOUBLEPRECISION RA,            ! RA and dec of the point in question
     +                DEC            !  in degrees

      LOGICAL INSIDE                 ! Is the point inside the plate?

      INTEGER STATUS                 ! Global status

*  Local variables:
      DOUBLEPRECISION RMAT(3,3)      ! Rotation matrix

      DOUBLEPRECISION VLOC(3)        ! The Cartesian form of the local position

      DOUBLEPRECISION VSKY(3)        ! The Cartesian form of the sky position

      DOUBLEPRECISION X,Y            ! Image co-ordinates

      DOUBLEPRECISION LOCX, LOCY     ! Local co-ordinates

      DOUBLEPRECISION U, V           ! (U,V) co-ordinates

      DOUBLEPRECISION COSX           ! Cosine of the local x co-ordinate

      DOUBLEPRECISION RAR, DECR      ! Co-ordinates of the point, in radians

      DOUBLEPRECISION RACR, DECCR    ! Co-ordinates of the plate centre
     +                               !   in radians

c  Abort if status not OK
      IF (STATUS .NE. SAI__OK) RETURN

c  Initialise the point as being inside the image
      INSIDE = .FALSE.

c  Convert all the input points into radians
      RAR = RA * IRA__DTOR
      DECR = DEC * IRA__DTOR

      RACR = RACNT * IRA__DTOR
      DECCR = DECCNT * IRA__DTOR

c  Form a rotation matrix
      CALL SLA_DEULER('ZYX', RACR, -DECCR, 0, RMAT)

c  Convert the point into local coordinates, with the origin on the
c  plate centre.
      CALL SLA_DCS2C(RAR , DECR, VSKY)
      CALL SLA_DMXV(RMAT, VSKY, VLOC)
      CALL SLA_DCC2S(VLOC, LOCX, LOCY)

c  Convert local coordinates to (U,V) coordinates
      COSX = COS(LOCX)
      IF (COSX .GT. 0.0D0) THEN
        U = -TAN(LOCX)
        V = TAN(LOCY) / COSX

      ELSE

c  Flag an error, as this should not happen
        STATUS = SAI__ERROR
        CALL ERR_REP('IFLDZ3',' Error converting co-ordinate system',
     +                STATUS)
        GOTO 999

      ENDIF

c  Convert the (U,V) co-ordinates into image co-ordinates
      X = U * 2291.8312 + 249.5
      Y = V * 2291.8312 + 249.5

c  Since the plates are 500 pixels square, calculate whether the point
c  lies inside the image
      IF (X .GE. 0.0 .AND. X .LE. 500.0 .AND. Y .GE. 0.0 .AND.
     +    Y .LE. 500.0) INSIDE = .TRUE.

 999  CONTINUE

      END
