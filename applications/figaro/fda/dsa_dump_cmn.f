      SUBROUTINE DSA_DUMP_COMMON( )
*+
*  Name:
*     DSA_DUMP_COMMON

*  Purpose:
*     Diagnostic routine to dump the DSA system common variables.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_DUMP_COMMON( )

*  Description:
*     Diagnostic routine to dump the DSA system common variables.

*  Arguments:
*     None.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     25 Nov 1995 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*Use B & Q:           L

*Reference slot no.: II        Slot used:   L
*Use bad values:      L        Use quality: L
*Principal NDF:  IIIIIIIIII    D/Q buffer NDF:  IIIIIIIIII
*Place holder:   IIIIIIIIII    V/E buffer NDF:  IIIIIIIIII
*Data pointer:   IIIIIIIIII    Quality pointer: IIIIIIIIII
*Locator:        AAAAAAAAAAAAAAAA
*FITS items:     II
*FITS pointer:   IIIIIIIIII    FITS locator:    AAAAAAAAAAAAAAAA
*Reference name: AAAAAAAAAAAAAAAA

*Map slot no.: II            Slot used: L
*NDF to unmap: IIIIIIIIII    Reference slot: II
*Component to unmap: AAAAAAAAAAAAAAAA
*Axis number:  II
*First pointer:  IIIIIIIIII  First locator:      AAAAAAAAAAAAAAAA
*Second pointer: IIIIIIIIII  Second locator:     AAAAAAAAAAAAAAAA

*Logical unit slot no: II    Slot used: L    Unit no: III

 107  FORMAT( / ' Use B & Q:           ', L1 )
 101  FORMAT( / ' Reference slot no.: ', I2,
     :        '        Slot used:   ', L1 )
 102  FORMAT( ' Use bad values:      ', L1,
     :        '        Use quality: ', L1 )
 103  FORMAT( ' Principal NDF: ', I10, '     D/Q buffer NDF:  ', I10 )
 104  FORMAT( ' Place holder:  ', I10, '     V/E buffer NDF:  ', I10 )
 105  FORMAT( ' Data pointer:  ', I10, '     Quality pointer: ', I10 )
 108  FORMAT( ' Locator:       ', A16 )
 109  FORMAT( ' FITS items:    ', I2  )
 110  FORMAT( ' FITS pointer:  ', I10, '     FITS locator:    ', A16 )
 106  FORMAT( ' Reference name: ', A16 )
 201  FORMAT( / ' Map slot no.: ', I2, '            Slot used: ', L1 )
 202  FORMAT( ' NDF to unmap: ', I10, '    Reference slot: ', I2 )
 203  FORMAT( ' Component to unmap: ', A16 )
 204  FORMAT( ' Axis number:  ', I2 )
 205  FORMAT( ' First pointer:  ', I10, '  First locator:      ', A16 )
 206  FORMAT( ' Second pointer: ', I10, '  Second locator:     ', A16 )
 301  FORMAT( ' Logical unit slot no: ', I2,
     :        '    Slot used: ', L1, '    Unit no: ', I2 )

      WRITE( *, 107 ) DSA__BADQUA
      DO 1 I = 1, DSA__MAXREF
         IF ( DSA__REFUSD(I) ) THEN
            WRITE( *, 101 ) I, DSA__REFUSD(I)
            WRITE( *, 102 ) DSA__REFBAD(I), DSA__REFQUA(I)
            WRITE( *, 103 ) DSA__REFID1(I), DSA__REFID2(I)
            WRITE( *, 104 ) DSA__REFPLC(I), DSA__REFID3(I)
            WRITE( *, 105 ) DSA__REFDPT(I), DSA__REFQPT(I)
            WRITE( *, 108 ) DSA__REFLOC(I)
            WRITE( *, 109 ) DSA__REFFNE(I)
            WRITE( *, 110 ) DSA__REFFPT(I), DSA__REFFLO(I)
            WRITE( *, 106 ) DSA__REFNAM(I)
         END IF
 1    CONTINUE
      DO 2 I = 1, DSA__MAXMAP
         IF ( DSA__MAPUSD(I) ) THEN
            WRITE( *, 201 ) I, DSA__MAPUSD(I)
            WRITE( *, 202 ) DSA__MAPID1(I), DSA__MAPREF(I)
            WRITE( *, 203 ) DSA__MAPNAM(I)
            WRITE( *, 204 ) DSA__MAPAXI(I)
            WRITE( *, 205 ) DSA__MAPPT1(I), DSA__MAPLO1(I)
            WRITE( *, 206 ) DSA__MAPPT2(I), DSA__MAPLO2(I)
         END IF
 2    CONTINUE
      DO 3 I = 1, DSA__MAXLU
         IF ( DSA__LUUSD(I) )
     :      WRITE( *, 301 ) I, DSA__LUUSD(I), DSA__LUNUM(I)
 3    CONTINUE

      END
