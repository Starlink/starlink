*+  FIT_MSPECFLAT - Flatten a model specification
      SUBROUTINE FIT_MSPECFLAT( MODEL, MAXTERM, NTERM, TERMS,
     :                          SIGNS, STATUS )
*
*    Description :
*
*     Takes a MODEL_SPEC containing an arbitrary model specification and
*     flattens that specification (ie. removes parentheses). The output is
*     NTERM additive terms, each of which may contain up to MAXCOMP
*     elements. The indices of the elements of the I'th term are stored in
*     TERMS(1..MAXCOMP,I). The number of elements in the term can be found
*     by counting the non-zero elements in this array. The additive model
*     component present in each term is always stored last. The array SIGNS
*     stores the connecting operator between that term and the preceding
*     term. The value is 1 for addition and -1 for subtraction.
*
*     Because of the distinction between multiplicative and additive spectral
*     fitting models this routine cannot legally return more than MAXCOMP
*     terms. If this system was changed, or if the routine was used in (say)
*     a function fitting program, then the maximum number of terms can be as
*     high as 2^(MAXCOMP/2).
*
*    Example :
*
*     Consider the model specification AB*(RZ-BB*ED). The AB and ED pmodels
*     are multiplicative, the other two additive. The spectral fitting
*     system numbers these components from the left, starting at one. The
*     output from the flattening process is,
*
*       NTERM   = 2
*       TERMS() = [1 2 0 0 ...]   ie.  [AB RZ __ __ ...]
*                 [1 4 3 0 ...]        [AB ED BB __ ...]
*       SIGNS() = [1 -1]
*
*     The value of an additive term can be constructed using the routine
*     FIT_MCALCTERM.
*
*    Method :
*
*     The C routine FIT_MSPECFLAT_INT does the flattening of the expressions.
*     This routine performs the interaction with the MODEL data structure and
*     the ordering of the output data. It also stores copies of the data
*     returned by the internal routine so that this routine can be called
*     repeatedly by fitting code with little overhead (because the C code is
*     rather slow).
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (ROSAT,University of Birmingham)
*
*    History :
*
*     24 Jan 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      RECORD /MODEL_SPEC/	MODEL			! Model specification
      INTEGER			MAXTERM			! Max no additive terms
*
*    Export :
*
      INTEGER			NTERM			! No. additive terms
      INTEGER			TERMS(MAXCOMP,MAXTERM)	! Additive terms
      INTEGER			SIGNS(MAXTERM)		! Signs on terms (+-1)
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER			CHR_LEN
*
*    Local constants :
*
      INTEGER			LMAXTERM	      	! Local maximum terms
	PARAMETER		( LMAXTERM = MAXCOMP )
*
*    Local variables :
*
      CHARACTER*80		STORED_SPEC		! Stored specification
        SAVE			STORED_SPEC

      INTEGER			AMODCOMP		! Additive comp id
      INTEGER			ICOMP			! Loop over components
      INTEGER			ITERM			! Loop over terms
      INTEGER			MLEN			! Length of MODEL.SPEC
      INTEGER			MODCOMP			! Component id
      INTEGER			NCOMP			! Components in a term
      INTEGER			SLEN			! Length of STORED_SPEC
        SAVE                    SLEN
      INTEGER			STORED_NTERM		! Number of terms
        SAVE			STORED_NTERM
      INTEGER			STORED_SIGNS(LMAXTERM)	! Signs of terms
        SAVE			STORED_SIGNS
      INTEGER			STORED_TERMS(MAXCOMP,   ! Terms' elements
     :					     LMAXTERM)
        SAVE			STORED_TERMS
      INTEGER			TCOMP			! Components use in swap

      LOGICAL			FOUND			! Found last compenent
							! in a term?
      LOGICAL			SAME			! Same model spec as
							! last time?
*
*    Local data :
*
      DATA                      SLEN/0/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Specification same as last one?
      SAME = .FALSE.
      MLEN = CHR_LEN(MODEL.SPEC)
      IF ( (MLEN.EQ.SLEN) .AND. (MLEN.GT.1) ) THEN
        SAME = ( MODEL.SPEC(:MLEN) .EQ. STORED_SPEC(:SLEN) )
      END IF

*    If it is the same, return stored values
      IF ( .NOT. SAME ) THEN

*      Store specification
        STORED_SPEC = MODEL.SPEC
        SLEN = MLEN

*      Flatten the expression
        CALL FIT_MSPECFLAT_INT( MODEL.SPEC, STORED_NTERM, STORED_TERMS,
     :                          STORED_SIGNS, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Order the elements of each term so that the multiplicative bits
*      come first.
        DO ITERM = 1, STORED_NTERM

*        First we locate the additive element in the list of the
*        elements of a term.
          AMODCOMP = -1
          FOUND = .FALSE.
          ICOMP = 1
          DO WHILE ( (ICOMP .LE. MAXCOMP) .AND. .NOT. FOUND )

*          Extract the ICOMP'th compenent in the ITERM'th term
            MODCOMP = STORED_TERMS(ICOMP,ITERM)

*          Reached the end of the list yet?
            IF ( MODCOMP .EQ. 0 ) THEN
              FOUND = .TRUE.

*          Found the additive component?
            ELSE IF ( MODEL.ADDITIVE(MODCOMP) ) THEN

*            Make sure its the only one
              IF ( AMODCOMP .GT. 0 ) THEN
                STATUS = SAI__ERROR
                CALL ERR_REP( ' ', 'Illegal model specification - '/
     :                        /'additive models must only appear on'/
     :                        /' one side of * operator', STATUS )
                GOTO 99
              ELSE
                AMODCOMP = ICOMP
              END IF

*            Next component
              ICOMP = ICOMP + 1

            ELSE

*            Next component
              ICOMP = ICOMP + 1

            END IF

*        End loop over a terms components
          END DO
          NCOMP = ICOMP - 1

*        Move the additive term to the end of the list of elements unless
*        it's already there.
          IF ( AMODCOMP .NE. NCOMP ) THEN
            TCOMP = STORED_TERMS(NCOMP,ITERM)
            STORED_TERMS(NCOMP,ITERM) = STORED_TERMS(AMODCOMP,ITERM)
            STORED_TERMS(AMODCOMP,ITERM) = TCOMP
          END IF

*      End loop all terms
        END DO

      END IF

*    Copy to output
      NTERM = STORED_NTERM
      DO ITERM = 1, NTERM
        DO ICOMP = 1, MAXCOMP
          TERMS(ICOMP,ITERM) = STORED_TERMS(ICOMP,ITERM)
        END DO
        SIGNS(ITERM) = STORED_SIGNS(ITERM)
      END DO

*    Abort point
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MSPECFLAT', STATUS )
      END IF

      END
