*+  HCOPY - COPY a primitive or structured object
      SUBROUTINE HCOPY(STATUS)
*    Description :
*     The input data object is copied to another named object
*    Parameters :
*     INP=UNIV
*           Object being copied from
*     OUT=CHAR
*           Object being copied to
*    Method :
*    Deficiencies :
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
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
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
	CHARACTER*(DAT__SZNAM) NAME	! Name of the output object
	CHARACTER*(DAT__SZTYP) TYPE	! Type of the input object

        INTEGER I			! Loop over OUT
        INTEGER IDOT,JDOT		! Dot positions
        INTEGER IKET                    ! ] position
        INTEGER L			! Len of output specifier
        INTEGER NDIM                    ! Input dimensionality
        INTEGER DIMS(DAT__MXDIM)        ! Input dimensions

        LOGICAL STRUC,PRIM
        LOGICAL THERE
*
*      Version id :
*
      CHARACTER*(20) VERSION
	PARAMETER(VERSION= 'HCOPY Version 1.7-1')
*-

	CALL MSG_PRNT( VERSION )

* Associate the input object file
        CALL DAT_ASSOC('INP','READ',ILOC,STATUS)

* Get specification of output object
        CALL PAR_GET0C('OUT',OUT,STATUS)

        IF (STATUS.EQ.SAI__OK) THEN

*        Length of output object name
          L = CHR_LEN(OUT)

* See if output string is structured. Find first DOT after first ">". Note
* that ADAM kindly converts ] to > in VMS.
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

          IF (IDOT.EQ.IKET) THEN

*           Get name of input object
             CALL DAT_NAME( ILOC, NAME, STATUS )

*           Should always use HDS_COPY here but it doesn't work for elements
*           of structure arrays. It works for primitives though.
             CALL DAT_PRIM( ILOC, PRIM, STATUS )
             IF ( PRIM ) THEN

               CALL HDS_COPY( ILOC, OUT, NAME, STATUS )

             ELSE

               CALL DAT_TYPE( ILOC, TYPE, STATUS )
               CALL DAT_NAME( ILOC, NAME, STATUS )
               CALL DAT_SHAPE( ILOC, DAT__MXDIM, NDIM, DIMS, STATUS )
               CALL HDS_NEW( OUT, NAME, TYPE, NDIM, DIMS, OLOC, STATUS )

*             Copy all sub-components
               CALL HDX_COPY( ILOC, OLOC, STATUS )
               CALL HDS_CLOSE( OLOC, STATUS )

             END IF

* Not structured so output direct to top-level object

          ELSE

*          Structured so get locator to top level of output file
            CALL HDS_OPEN(OUT(:IDOT-1),'UPDATE',OLOC,STATUS)
            IF (STATUS.EQ.SAI__OK) THEN
* Extract name of bottom-level object
              JDOT=L
              DO WHILE (OUT(JDOT:JDOT).NE.DOT)
                JDOT=JDOT-1
              ENDDO
              NAME=OUT(JDOT+1:L)

              IF (JDOT.GT.IDOT) THEN
* If more than two levels find containing level
                CALL HDX_FIND(OLOC,OUT(IDOT+1:JDOT-1),CLOC,STATUS)
              ELSE
* Otherwise top level is container level
                CALL DAT_CLONE(OLOC,CLOC,STATUS)
              ENDIF

              IF (STATUS.EQ.SAI__OK) THEN
*  Check if containing level is a structure
                CALL DAT_STRUC(CLOC,STRUC,STATUS)
                IF (STRUC) THEN
*  See if object of same name already exists
                  CALL DAT_THERE(CLOC,NAME,THERE,STATUS)
*  If so get rid of it
                  IF (THERE) THEN
                    CALL DAT_ERASE(CLOC,NAME,STATUS)
                  ENDIF

* Now ready to copy
                  CALL DAT_COPY(ILOC,CLOC,NAME,STATUS)


                ELSE
                  CALL MSG_PRNT(
     :            '! Level to contain copied object is not a structure')
                  STATUS=SAI__ERROR
                ENDIF
                CALL DAT_ANNUL(CLOC,STATUS)
              ENDIF
              CALL HDS_CLOSE(OLOC,STATUS)
            ENDIF
          ENDIF
          CALL DAT_ANNUL(ILOC,STATUS)
	ENDIF

        CALL AST_ERR( STATUS )

	END
