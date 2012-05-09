      SUBROUTINE CCD1_CLCOR( CTYPE, DTYPE, BAD, EL, IPDIN, IPVIN,
     :                       IPDCAL, IPVCAL, HAVDV, HAVCV, EXPOSE,
     :                       SETSAT, SATVAL, IPDOUT, IPVOUT, STATUS )
*+
*  Name:
*     CCD1_CLCOR

*  Purpose:
*     To subtract a scaled NDF of one type from another NDF of a
*     possibly different type, allowing for saturated values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CLCOR( CTYPE, DTYPE, BAD, EL, IPDIN, IPVIN, IPDCAL,
*                      IPVCAL, HAVDV, HAVCV, EXPOSE, SETSAT, SATVAL,
*                      IPDOUT, IPVOUT, STATUS )

*  Description:
*     This routine mainly just procrastinates the complex typing,
*     calling the appropriate routine to do the actual work. The
*     routines handles the case of no calibration variance (a noiseless
*     case) and if saturated values are present.

*  Arguments:
*     CTYPE = CHARACTER * ( * ) (Given)
*        The type of the calibration data (i.e. the one to be
*        subtracted).
*     DTYPE = CHARACTER * ( * ) (Given)
*        The type of the input data (i.e. the one to be subtracted
*        from).
*     BAD = LOGICAL (Given and Returned)
*        Whether BAD pixels are present or not.
*     EL = INTEGER (Given)
*        Number of pixels in the input arrays.
*     IPDIN = INTEGER (Given)
*        Pointer to the input Data component.
*     IPVIN = INTEGER (Given)
*        Pointer to the input Variance component.
*     IPDCAL = INTEGER (Given)
*        Pointer to Data component of the calibration NDF.
*     IPVCAL = INTEGER (Given)
*        Pointer to Variance component of the calibration NDF.
*     HAVDV = LOGICAL (Given)
*        Whether or not the input NDF has a Variance component.
*     HAVCV = LOGICAL (Given)
*        Whether or not the input calibration NDF has a Variance
*        component.
*     EXPOSE = DOUBLE PRECISION (Given)
*        The exposure factor required to scale the calibration frame to
*        the data frame. This must be the dark time if the input is a
*        dark frame or the flash exposure time if the input is a flash
*        frame. The units are those used to scale the input calibration
*        frame to unity.
*     SETSAT = LOGICAL (Given)
*        Whether the input Data has had a saturation value applied or
*        not.
*     SATVAL =  DOUBLE PRECISION (Given)
*        The saturation value if used.
*     IPDOUT = INTEGER (Given and Returned)
*        Pointer to the output Data component.
*     IPVOUT = INTEGER (Given and Returned)
*        Pointer to the output Variance component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses pointers to arrays.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-MAY-1991 (PDRAPER):
*        Original Version.
*     7-JAN-1992 (PDRAPER):
*        Changed to fully doubly generic.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * (* ) CTYPE
      CHARACTER * (* ) DTYPE
      INTEGER EL
      INTEGER IPDIN
      INTEGER IPVIN
      INTEGER IPDCAL
      INTEGER IPVCAL
      LOGICAL HAVDV
      LOGICAL HAVCV
      LOGICAL SETSAT
      DOUBLE PRECISION SATVAL
      DOUBLE PRECISION EXPOSE

*  Arguments Given and Returned:
      LOGICAL BAD
      INTEGER IPDOUT
      INTEGER IPVOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ERROR              ! Set true if unsupported data type is
                                 ! given
      LOGICAL ALLBAD             ! Determines if all output pixels are
                                 ! BAD.
      INTEGER NERR               ! Number of numeric errors which
                                 ! occur when processing date
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set unsupported numeric type flag.
      ERROR = .FALSE.

*  Call the appropriate routine.
      IF ( CTYPE .EQ. '_UBYTE' ) THEN
         IF ( DTYPE .EQ. '_UBYTE' ) THEN

*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UBCLUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                           %VAL( CNF_PVAL( IPVIN ) ),
     :                           %VAL( CNF_PVAL( IPDCAL ) ),
     :                           %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                           HAVCV, EXPOSE,
     :                           %VAL( CNF_PVAL( IPDOUT ) ),
     :                           %VAL( CNF_PVAL( IPVOUT ) ),
     :                           NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UBCSUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                           %VAL( CNF_PVAL( IPVIN ) ),
     :                           %VAL( CNF_PVAL( IPDCAL ) ),
     :                           %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                           HAVCV, EXPOSE, SATVAL,
     :                           %VAL( CNF_PVAL( IPDOUT ) ),
     :                           %VAL( CNF_PVAL( IPVOUT ) ),
     :                           NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UBCLB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UBCSB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UBCLW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UBCSW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UBCLUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                           %VAL( CNF_PVAL( IPVIN ) ),
     :                           %VAL( CNF_PVAL( IPDCAL ) ),
     :                           %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                           HAVCV, EXPOSE,
     :                           %VAL( CNF_PVAL( IPDOUT ) ),
     :                           %VAL( CNF_PVAL( IPVOUT ) ),
     :                           NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UBCSUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                           %VAL( CNF_PVAL( IPVIN ) ),
     :                           %VAL( CNF_PVAL( IPDCAL ) ),
     :                           %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                           HAVCV, EXPOSE, SATVAL,
     :                           %VAL( CNF_PVAL( IPDOUT ) ),
     :                           %VAL( CNF_PVAL( IPVOUT ) ),
     :                           NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UBCLI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UBCSI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INT64' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UBCLK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UBCSK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_REAL' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UBCLR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UBCSR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UBCLD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UBCSD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE
            ERROR = .TRUE.
         END IF
      ELSE IF ( CTYPE .EQ. '_BYTE' ) THEN
         IF ( DTYPE .EQ. '_UBYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_BCLUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_BCSUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_BCLB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_BCSB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_BCLW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_BCSW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_BCLUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_BCSUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_BCLI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_BCSI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INT64' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_BCLK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_BCSK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_REAL' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_BCLR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_BCSR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_BCLD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_BCSD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE
            ERROR = .TRUE.
         END IF
      ELSE IF ( CTYPE .EQ. '_WORD' ) THEN
         IF ( DTYPE .EQ. '_UBYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_WCLUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_WCSUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_WCLB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_WCSB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_WCLW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_WCSW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_WCLUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_WCSUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_WCLI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_WCSI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INT64' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_WCLK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_WCSK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_REAL' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_WCLR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_WCSR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_WCLD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_WCSR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE
            ERROR = .TRUE.
         END IF
      ELSE IF ( CTYPE .EQ. '_UWORD' ) THEN
         IF ( DTYPE .EQ. '_UBYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UWCLUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                           %VAL( CNF_PVAL( IPVIN ) ),
     :                           %VAL( CNF_PVAL( IPDCAL ) ),
     :                           %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                           HAVCV, EXPOSE,
     :                           %VAL( CNF_PVAL( IPDOUT ) ),
     :                           %VAL( CNF_PVAL( IPVOUT ) ),
     :                           NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UWCSUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                           %VAL( CNF_PVAL( IPVIN ) ),
     :                           %VAL( CNF_PVAL( IPDCAL ) ),
     :                           %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                           HAVCV, EXPOSE, SATVAL,
     :                           %VAL( CNF_PVAL( IPDOUT ) ),
     :                           %VAL( CNF_PVAL( IPVOUT ) ),
     :                           NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UWCLB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UWCSB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UWCLW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UWCSW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UWCLUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                           %VAL( CNF_PVAL( IPVIN ) ),
     :                           %VAL( CNF_PVAL( IPDCAL ) ),
     :                           %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                           HAVCV, EXPOSE,
     :                           %VAL( CNF_PVAL( IPDOUT ) ),
     :                           %VAL( CNF_PVAL( IPVOUT ) ),
     :                           NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UWCSUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                           %VAL( CNF_PVAL( IPVIN ) ),
     :                           %VAL( CNF_PVAL( IPDCAL ) ),
     :                           %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                           HAVCV, EXPOSE, SATVAL,
     :                           %VAL( CNF_PVAL( IPDOUT ) ),
     :                           %VAL( CNF_PVAL( IPVOUT ) ),
     :                           NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UWCLI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UWCSI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INT64' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UWCLK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UWCSK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_REAL' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UWCLR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UWCSR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_UWCLD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_UWCSD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE
            ERROR = .TRUE.
         END IF
      ELSE IF ( CTYPE .EQ. '_INTEGER' ) THEN
         IF ( DTYPE .EQ. '_UBYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_ICLUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_ICSUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_ICLB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_ICSB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_ICLW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_ICSW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_ICLUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_ICSUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_ICLI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_ICSI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INT64' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_ICLK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_ICSK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_REAL' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_ICLR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_ICSR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_ICLD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_ICSD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE
            ERROR = .TRUE.
         END IF
      ELSE IF ( CTYPE .EQ. '_INT64' ) THEN
         IF ( DTYPE .EQ. '_UBYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_KCLUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_KCSUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_KCLB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_KCSB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_KCLW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_KCSW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_KCLUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_KCSUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_KCLI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_KCSI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INT64' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_KCLK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_KCSK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_REAL' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_KCLR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_KCSR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_KCLD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_KCSD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE
            ERROR = .TRUE.
         END IF
      ELSE IF ( CTYPE .EQ. '_REAL' ) THEN
         IF ( DTYPE .EQ. '_UBYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RCLUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RCSUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RCLB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RCSB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RCLW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RCSW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RCLUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RCSUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RCLI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RCSI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INT64' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RCLK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RCSK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_REAL' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RCLR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RCSR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RCLD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RCSD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE
            ERROR = .TRUE.
         END IF
      ELSE IF ( CTYPE .EQ. '_DOUBLE' ) THEN
         IF ( DTYPE .EQ. '_UBYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DCLUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DCSUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DCLB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DCSB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DCLW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DCSW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DCLUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DCSUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDCAL ) ),
     :                          %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                          HAVCV, EXPOSE, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DCLI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DCSI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INT64' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DCLK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DCSK( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_REAL' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DCLR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DCSR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
*  Call a routine to do the scaled subtractions. If saturations are
*  present then these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DCLD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DCSD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDCAL ) ),
     :                         %VAL( CNF_PVAL( IPVCAL ) ), HAVDV,
     :                         HAVCV, EXPOSE, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE
            ERROR = .TRUE.
         END IF
      ELSE
         ERROR = .TRUE.
      END IF

      IF ( ERROR ) THEN
*  Unsupported type.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_CLCOR1',
     :   '  CCD1_CLCOR: Input data has unsupported numeric type',
     :   STATUS )
      END IF

*  report the number of numeric errors which have occurred. If the
*  number is equal to a significant fraction of the value EL then test
*  the whole array for complete BADness and issue a warning.
      IF ( NERR .NE. 0 ) THEN

*  If nerr/el gt 0.75 test for complete BADness.
         IF ( REAL( NERR ) / REAL( EL ) .GT. 0.75 ) THEN
            CALL CCD1_TSTB( DTYPE, IPDOUT, EL, ALLBAD,
     :      STATUS )
            IF ( ALLBAD ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'DTYPE', DTYPE )
               CALL ERR_REP( 'CCD1_CLCOR2',
     :         '  CCD1_CLCOR: Complete numeric failure, ALL'//
     :         ' elements of array set BAD. Output data cannot be'//
     :         ' represented within the permissable range of HDS'//
     :         ' data type ^DTYPE', STATUS )
            END IF
         END IF

*  Just issue a warning about number of numeric failures.
         CALL MSG_SETR( 'PERCENT', REAL( NERR ) / REAL ( EL ) * 100.0 )
         CALL MSG_SETI( 'NERR', NERR )
         CALL CCD1_MSG( ' ',
     :   '  Warning -- ^NERR numeric errors occurred'//
     :   ' (^PERCENT %% of total), when subtracting'//
     :   ' calibration frame.', STATUS )
      END IF


      END
* $Id$
