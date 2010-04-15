      SUBROUTINE JCMT_FAKE_DATA (FAKE_TYPE, XOFF, YOFF, PIXSIZE,
     :   NPIXEL, RACENTRE, DECCENTRE, RA, DEC, DATA, FBAD, STATUS)
*+
*  Name:
*      JCMT_FAKE_DATA

*  Purpose:
*     generate fake data set

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL JCMT_FAKE_DATA (FAKE_TYPE, XOFF, YOFF, PIXSIZE, NPIXEL,
*     :   RACENTRE, DECCENTRE, RA, DEC, DATA, FBAD, STATUS)

*  Description:

*  Arguments:
*     FAKE_TYPE = CHAR*(*) (Given)
*        Type of fake data wanted, FLAT or AIRY
*        The number of scans in the input data
*     XOFF = REAL (Given)
*        The RA offset of the Airy function required from the map centre
*        (radians)
*     YOFF = REAL (Given)
*        The Dec offset of the Airy function required from the map centre
*        (radians)
*     PIXSIZE = DOUBLE PRECISION (Given)
*        The size of the pixels in the map that would just fully sample the
*        the fake Airy function (radians)
*     NPIXEL = INTEGER (Given)
*        The number of pixels in the data
*     RACENTRE = DOUBLE PRECISION (Given)
*        The RA of the map centre (radians)
*     DECCENTRE = DOUBLE PRECISION (Given)
*        The Dec of the map centre (radians)
*     RA (NPIXEL) = DOUBLE PRECISION (Given)
*        The RAs of the input pixels (radians)
*     DEC (NPIXEL) = DOUBLE PRECISION (Given)
*        The Decs of the input pixels (radians)
*     DATA (NPIXEL) = REAL (Given and returned)
*        Given is the real data, returned is the fake
*     FBAD = REAL (Given)
*        The value to which unspecified, `bad', pixels are set.
*     STATUS = INTEGER (Given and returned)
*        Global status

*  Implementation Deficiencies:
*     [routine_deficiencies]...

*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: J.Lightfoot
*     {enter_new_authors_here}

*  History:
*     11-NOV-1991: REVAD::JFL: Fixed major bug that left out cos(dec)
*                              effect on RA arc angles.

*  Bugs:
*     {note_any_bugs_here}
*-

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'ASTRO_PAR'


*  Local Constants:

*  External functions:

      DOUBLE PRECISION PDA_DBESJ1                 ! NAG Bessel function


      CHARACTER*(*) FAKE_TYPE
      REAL XOFF, YOFF
      DOUBLE PRECISION PIXSIZE
      INTEGER NPIXEL
      DOUBLE PRECISION RACENTRE
      DOUBLE PRECISION DECCENTRE
      DOUBLE PRECISION RA (NPIXEL)
      DOUBLE PRECISION DEC (NPIXEL)
      REAL DATA (NPIXEL)
      REAL FBAD
      INTEGER STATUS


*  Local Variables:

      INTEGER IPIX                               ! DO loop
      INTEGER IFAIL                              ! PDA routine IFAIL
      INTEGER IGNORE                             !
      REAL WT                                    ! value of weighting function
                                                 ! at output pixel
      DOUBLE PRECISION RPIX                      ! distance of current output
                                                 ! pixel from current input pixel
      DOUBLE PRECISION XX                        ! argument of PDA_DBESJ1

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF ((FAKE_TYPE.NE.'AIRY') .AND. (FAKE_TYPE.NE.'FLAT')) THEN
         IGNORE = 0
         CALL PAR_WRUSER ('JCMT_FAKE_DATA - Unknown type of fake '//
     :     'wanted', IGNORE)
         STATUS = SAI__ERROR
         RETURN
      END IF


*  do the faking

      IF (FAKE_TYPE .EQ. 'AIRY') THEN

         DO IPIX = 1, NPIXEL

*  work out distance of pixel from centre, then corresponding
*  value of Airy function. Airy function is such that would be fully
*  sampled by measurements at PIXSIZE spacing.

            RPIX = (RA(IPIX) - RACENTRE) * COS (DEC(IPIX))
            RPIX = SQRT ((RPIX-XOFF)**2 +
     :         (DEC(IPIX)-DECCENTRE-YOFF)**2)
            RPIX = RPIX / PIXSIZE
            XX = RPIX * DPI / (2.0D0)
            IF (XX .NE. 0.0D0) THEN
               IFAIL = 0
               WT = (2.0* PDA_DBESJ1(XX, IFAIL) / XX) ** 2
            ELSE
               WT = 1.0D0
            END IF

*  set data

            DATA (IPIX) = WT

         END DO

      ELSE IF (FAKE_TYPE .EQ. 'FLAT') THEN

         DO IPIX = 1, NPIXEL
            DATA (IPIX) = 1.0
         END DO

      END IF

      END
