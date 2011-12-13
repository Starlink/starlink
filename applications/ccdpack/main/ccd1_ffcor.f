      SUBROUTINE CCD1_FFCOR( FTYPE, DTYPE, BAD, EL, IPDIN, IPVIN,
     :                       IPDFLT, IPVFLT, HAVDV, HAVFV, SETSAT,
     :                       SATVAL, IPDOUT, IPVOUT, STATUS )
*+
*  Name:
*     CCD1_FFCOR

*  Purpose:
*     To divide one NDF by a Flatfield NDF, allowing for saturated
*     values and different types.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_FFCOR( FTYPE, DTYPE, BAD, EL, IPDIN, IPVIN, IPDFLT,
*                      IPVFLT, HAVDV, HAVFV, SETSAT, SATVAL,
*                      IPDOUT, IPVOUT, STATUS )

*  Description:
*     This routine mainly just procrastinates the typing, calling the
*     appropriate routine to do the actual work. The routines handle the
*     case of no flatfield variance (a noiseless case) and if saturated
*     values are present. The flatfield data types are restricted to
*     _REAL and _DOUBLE. The other data can be of any HDS numeric
*     (non-complex) type.

*  Arguments:
*     FTYPE = CHARACTER * ( * ) (Given)
*        The flatfield data type, must be one of _REAL or _DOUBLE.
*     DTYPE = CHARACTER * ( * ) (Given)
*        The data type. Can be any HDS (non-complex) numeric data type.
*     BAD = LOGICAL (Given and Returned)
*        Whether BAD pixels are present or not.
*     EL = INTEGER (Given)
*        Number of pixels in the input arrays.
*     IPDIN = INTEGER (Given)
*        Pointer to the input Data component.
*     IPVIN = INTEGER (Given)
*        Pointer to the input Variance component.
*     IPDFLT = INTEGER (Given)
*        Pointer to Data component of the flatfield NDF.
*     IPVFLT = INTEGER (Given)
*        Pointer to Variance component of the flatfield NDF.
*     HAVDV = LOGICAL (Given)
*        Whether or not the input NDF has a Variance component.
*     HAVFV = LOGICAL (Given)
*        Whether or not the input flatfield NDF has a Variance
*        component.
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
*     14-MAY-1991 (PDRAPER):
*        Original version.
*     13-JAN-1992 (PDRAPER):
*        Changed to handle a fully generic case.
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
      CHARACTER * (* ) FTYPE
      CHARACTER * (* ) DTYPE
      INTEGER EL
      INTEGER IPDIN
      INTEGER IPVIN
      INTEGER IPDFLT
      INTEGER IPVFLT
      LOGICAL HAVDV
      LOGICAL HAVFV
      LOGICAL SETSAT
      DOUBLE PRECISION SATVAL

*  Arguments Given and Returned:
      LOGICAL BAD
      INTEGER IPDOUT
      INTEGER IPVOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER NERR               ! Number of numeric errors
      LOGICAL ERROR              ! Set true in an invalif numeric type
                                 ! is given
      LOGICAL ALLBAD             ! Set true if all output pixels are BAD
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set BAD numeric type flag.
      ERROR = .FALSE.
*  Call the appropriate routine.

*  SINGLE PRECISION flatfield.
*  ===========================

      IF ( FTYPE .EQ. '_REAL' ) THEN
         IF ( DTYPE .EQ. '_UBYTE' ) THEN

*  Call a routine to do the divisions. If saturations are present then
*  these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RFFUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RFSUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                          %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
*  Call a routine to do the divisions. If saturations are present then
*  these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RFFB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RFSB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
*  Call a routine to do the divisions. If saturations are present then
*  these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RFFUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RFSUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                          %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
*  Call a routine to do the divisions. If saturations are present then
*  these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RFFW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RFSW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
*  Call a routine to do the divisions. If saturations are present then
*  these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RFFI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RFSI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_REAL' ) THEN
*  Call a routine to do the divisions. If saturations are present then
*  these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RFFR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RFSR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
*  Call a routine to do the divisions. If saturations are present then
*  these values should not be modified.
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_RFFD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_RFSD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE
            ERROR = .TRUE.
         END IF

*  DOUBLE PRECISION flatfield.
*  ==========================

      ELSE IF ( FTYPE .EQ. '_DOUBLE' ) THEN
         IF ( DTYPE .EQ. '_UBYTE' ) THEN
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DFFUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DFSUB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                          %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DFFB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DFSB( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DFFUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DFSUW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                          %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                          %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                          %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                          NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DFFW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DFSW( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DFFI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DFSI( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_REAL' ) THEN
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DFFR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DFSR( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
            IF ( .NOT. SETSAT ) THEN
               CALL CCG1_DFFD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                         %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                         HAVFV, %VAL( CNF_PVAL( IPDOUT ) ),
     :                         %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            ELSE

*  Have saturated values
               CALL CCG1_DFSD( BAD, EL, %VAL( CNF_PVAL( IPDIN ) ),
     :                         %VAL( CNF_PVAL( IPVIN ) ),
     :                          %VAL( CNF_PVAL( IPDFLT ) ),
     :                         %VAL( CNF_PVAL( IPVFLT ) ), HAVDV,
     :                          HAVFV, SATVAL,
     :                         %VAL( CNF_PVAL( IPDOUT ) ),
     :                          %VAL( CNF_PVAL( IPVOUT ) ),
     :                         NERR, STATUS )
            END IF
         ELSE
            ERROR = .TRUE.
         END IF
      ELSE

*  Unsupported flatfield type.
         ERROR = .TRUE.
      END IF

      IF ( ERROR ) THEN
*  Unsupported type.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_FFCOR1',
     :   '  CCD1_FFCOR: Input data has unsupported numeric type',
     :   STATUS )
      END IF

*  Report the number of numeric errors which have occurred. If the
*  number is equal to a significant fraction of the value EL then test
*  the whole array for complete BADness and issue a warning.
      IF ( NERR .NE. 0 ) THEN

*  If nerr/el gt 0.75 test for complete BADness.
         IF ( REAL( NERR ) / REAL( EL ) .GT. 0.75 ) THEN
            CALL CCD1_TSTB( DTYPE, IPDOUT, EL, ALLBAD,
     :                      STATUS )
            IF ( ALLBAD ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'DTYPE', DTYPE )
               CALL ERR_REP( 'CCD1_FFCOR2',
     :         '  CCD1_FFCOR: Complete numeric failure -- ALL'//
     :         ' elements of output array set BAD (invalid).', STATUS )
            END IF
         END IF

*  Just issue a warning about number of numeric failures.
         CALL MSG_SETI( 'NERR', NERR )
         CALL CCD1_MSG( ' ',
     :   '  Warning -- ^NERR numeric errors occurred'//
     :   ' when dividing by flatfield.', STATUS )
      END IF



      END
* $Id$
