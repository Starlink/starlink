*+  HCOPY - Copy a primitive or structured object
      SUBROUTINE HCOPY( STATUS )
*
*    Description :
*
*     The input data object is copied to another named object
*
*    Parameters :
*
*     INP=UNIV
*           Object being copied from
*     OUT=CHAR
*           Object being copied to
*
*    Method :
*    Deficiencies :
*
*     1) Structure to slice copying isn't allowed. This would be useful,
*        eg.
*                hcopy file1.axis(1) file.axis(2)
*
*    Bugs :
*    Authors :
*
*     Jim Peden (BHVAD::JCMP)
*     R J Vallance (BHVAD::RJV)
*
*    History :
*
*     19 Jan 84 : Original (JCMP)
*     27 Jan 86 : V0.4-1 ADAM version (JCMP)
*     25 Sep 86 : V0.5-1 Copies primitives too (JCMP)
*      4 May 89 : V1.0-1 Major rewrite (RJV)
*      9 Jul 91 : V1.5-0 Copies elements of structure arrays ok now. Also
*                        writes files to directories specs containing DOT (DJA)
*     22 Jun 92 : V1.6-0 DAT_PAR included explicitly (DJA)
*     14 Apr 93 : V1.7-0 Incorrect DAT_ANNUL replaced with HDS_CLOSE (DJA)
*     11 Jun 93 : V1.7-1 Fixed bug in copy to container file with length > 15
*                        characters (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     30 Nov 94 : V1.8-1 Allow primitive to primitive slice copying (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    External references :
*
      INTEGER CHR_LEN
*
*    Local Constants :
*
      CHARACTER*1 DOT
        PARAMETER (DOT='.')
*
*    Local variables :
*
        CHARACTER*(DAT__SZLOC) ILOC	! input locator
	CHARACTER*(DAT__SZLOC) OLOC	! output dataset locator
	CHARACTER*(DAT__SZLOC) CLOC	! locator to level above output object
        CHARACTER*132 OUT	        ! output specification string
	CHARACTER*50 NAME	! Name of the output object (with slice bnds)
	CHARACTER*(DAT__SZTYP) TYPE	! Type of the input object

        INTEGER I			! Loop over OUT
        INTEGER IDOT,JDOT		! Dot positions
        INTEGER IKET                    ! ] position
        INTEGER L			! Len of output specifier
        INTEGER NDIM                    ! Input dimensionality
        INTEGER NELM                    ! Input dimensionality
        INTEGER ONDIM                    ! Input dimensionality
        INTEGER DIMS(DAT__MXDIM)        ! Input dimensions
        INTEGER ODIMS(DAT__MXDIM)        ! Input dimensions
        INTEGER IDIM, PPOS, IPTR

      LOGICAL 			STRUC, PRIM		! Input attributes
      LOGICAL			SAME			! Dimensions the same?
      LOGICAL			THERE			! Object exists?
*
*    Version id :
*
      CHARACTER*(20) VERSION
	PARAMETER(VERSION= 'HCOPY Version 1.8-1')
*-

*    Version id
      CALL MSG_PRNT( VERSION )

*    Start asterix
      CALL AST_INIT()

*    Associate the input object file
      CALL USI_DASSOC( 'INP', 'READ', ILOC, STATUS )

*    Get specification of output object
      CALL USI_GET0C( 'OUT', OUT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Length of output object name
      L = CHR_LEN(OUT)

*    See if output string is structured. Find first DOT after first ">". Note
*    that ADAM kindly converts ] to > in VMS.
      IKET = INDEX(OUT,'>')
      IF ( IKET .EQ. 0 ) THEN
        I = L
        DO WHILE ( (I.GT.0) .AND. (IKET.EQ.0) )
          IF ( OUT(I:I) .EQ. '/' ) THEN
            IKET = I
          ELSE
            I = I - 1
          END IF
        END DO
      END IF
      IDOT = INDEX(OUT(IKET+1:),DOT)+IKET

*    Is the input primitive?
      CALL DAT_PRIM( ILOC, PRIM, STATUS )

*    Get name of input object
      CALL DAT_NAME( ILOC, NAME, STATUS )

*    Output to structured object
      IF ( IDOT .EQ. IKET ) THEN

*      Should always use HDS_COPY here but it doesn't work for elements
*      of structure arrays. It works for primitives though.
        IF ( PRIM ) THEN

          CALL HDS_COPY( ILOC, OUT, NAME, STATUS )

        ELSE

          CALL DAT_TYPE( ILOC, TYPE, STATUS )
          CALL DAT_NAME( ILOC, NAME, STATUS )
          CALL DAT_SHAPE( ILOC, DAT__MXDIM, NDIM, DIMS, STATUS )
          CALL HDS_NEW( OUT, NAME, TYPE, NDIM, DIMS, OLOC, STATUS )

*        Copy all sub-components
          CALL HDX_COPY( ILOC, OLOC, STATUS )
          CALL HDS_CLOSE( OLOC, STATUS )

        END IF

*    Not structured so output direct to top-level object
      ELSE

*      Structured so get locator to top level of output file
        CALL HDS_OPEN( OUT(:IDOT-1), 'UPDATE', OLOC, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Extract name of bottom-level object
        JDOT = L
        DO WHILE ( OUT(JDOT:JDOT) .NE. DOT )
          JDOT = JDOT - 1
        END DO
        NAME = OUT(JDOT+1:L)

*      Has a slice been supplied?
        PPOS = INDEX( NAME, '(' )
        IF ( PPOS .GT. 0 ) THEN

*        If the input is primitive then the copy is valid if the output
*        slice is primitive and matches the dimensions of the input.
          IF ( PRIM ) THEN

*          Locate output
            CALL HDX_FIND( OLOC, OUT(IDOT+1:L), CLOC, STATUS )

*          Get input shape. Adjust scalars to be 1-D arrays of length 1
            CALL DAT_SHAPE( ILOC, DAT__MXDIM, NDIM, DIMS, STATUS )
            IF ( NDIM .EQ. 0 ) THEN
              NDIM = 1
              DIMS(1) = 1
            END IF

*          Get output shape
            CALL DAT_SHAPE( CLOC, DAT__MXDIM, ONDIM, ODIMS, STATUS )

*          Check they're the same
            SAME = (NDIM.EQ.ONDIM)
            IDIM = 1
            DO WHILE ( (IDIM.LE.NDIM) .AND. SAME )
              SAME = (DIMS(IDIM).EQ.ODIMS(IDIM))
              IDIM = IDIM + 1
            END DO
            IF ( SAME ) THEN

*            Get output type and map input data with that type
              CALL DAT_TYPE( ILOC, TYPE, STATUS )
              CALL DAT_MAPV( ILOC, TYPE, 'READ', IPTR, NELM, STATUS )

*            Write data to output
              CALL DAT_PUT( CLOC, TYPE, ONDIM, ODIMS, %VAL(IPTR),
     :                      STATUS )

*            Release input
              CALL DAT_UNMAP( ILOC, STATUS )

            ELSE
              STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'Invalid copy operation - input '/
     :                      /'object dimensions are not the same as '/
     :                      /'those of the output slice', STATUS )
            END IF

*        At the moment we don't allow structure to slice copying. It would
*        be possible to allow this by parsing the input object's path and
*        testing to see if its a slice.
          ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Invalid copy operation - HCOPY cannot'/
     :                    /' copy structures to slices', STATUS )

          END IF

*      Not copying to a slice
        ELSE

          IF ( JDOT .GT. IDOT ) THEN
*          If more than two levels find containing level
            CALL HDX_FIND(OLOC,OUT(IDOT+1:JDOT-1),CLOC,STATUS)
          ELSE
*          Otherwise top level is container level
            CALL DAT_CLONE(OLOC,CLOC,STATUS)
          END IF

          IF (STATUS.EQ.SAI__OK) THEN
*  Check if containing level is a structure
            CALL DAT_STRUC(CLOC,STRUC,STATUS)
            IF (STRUC) THEN
*  See if object of same name already exists
              CALL DAT_THERE(CLOC,NAME,THERE,STATUS)
*  If so get rid of it
              IF (THERE) THEN
                CALL DAT_ERASE(CLOC,NAME,STATUS)
              END IF

*            Now ready to copy
              CALL DAT_COPY(ILOC,CLOC,NAME,STATUS)

            ELSE
              STATUS=SAI__ERROR
              CALL ERR_REP( ' ',
     :            'Level to contain copied object is not a structure',
     :            STATUS )
            END IF

          END IF

*        Release output containing object
          CALL DAT_ANNUL( CLOC, STATUS )

        END IF

*      Release output file
        CALL HDS_CLOSE( OLOC, STATUS )

      END IF

*    Release input
      CALL USI_ANNUL( ILOC, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
