*+  FIT_MCALCTERM - Evaluate additive term of a compound model
      SUBROUTINE FIT_MCALCTERM( IMOD, NTERM, TERMS, SIGNS, ITERM,
     :                          PARAM, ND, NMDIM, IDIMM, NMDAT,
     :                          NMBOUND, MLBOUND, MUBOUND, STACK,
     :                          TMOD, PRED, STATUS )
*
*    Description :
*
*     Evaluates the ITERM'th additive term of the model described by MODEL.
*
*    Method :
*
*     The routine constructs a local TMODEL which describes the ITERM'th
*     additive term. Parameters are copied from PARAM and the model
*     prescription from the necessary components of MODEL. The model is
*     evaluated using FIT_MCALC.
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
*      9 Feb 94 : Changed interface to FIT_MCALC (DJA)
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
c     RECORD /MODEL_SPEC/	MODEL			! Model specification
      INTEGER			IMOD
      INTEGER			NTERM			! No. additive terms
      INTEGER			TERMS(MAXCOMP,NTERM)	! Additive terms
      INTEGER			SIGNS(NTERM)		! Signs on terms (+-1)
      INTEGER			ITERM			! Term to evaluate
      REAL			PARAM(*)		! MODEL parameters
      INTEGER			ND			! Current dataset number
      INTEGER			NMDIM			! Model dimensionality
      INTEGER			IDIMM(NMDIM)		! Model dimensions
      INTEGER			NMDAT			! No. model data points
      INTEGER			NMBOUND			! No. model bounds
      REAL			MLBOUND(NMBOUND)	! Model bin lower bounds
      REAL			MUBOUND(NMBOUND)	! Model bin upper bounds
*
*    Export :
*
      REAL   			STACK(NMDAT,MAXSTACK)	! Model stack
c     RECORD /MODEL_SPEC/	TMODEL			! Term model spec
      INTEGER			TMOD
      REAL			PRED(*)			! Output data
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER			CHR_LEN
*
*    Local variables :
*
      REAL			TPARAM(NPAMAX)		! Term parameters

      INTEGER			ICOMP			! Loop over components
      INTEGER			KLEN			! Length of MODEL key
      INTEGER			LASTPAR			! Last model parameter
      INTEGER			MCOMP			! Term model component
      INTEGER			TLEN			! Length of TMODEL.SPEC

      LOGICAL			FOUND			! Found last compenent
							! in a term?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Construct term specification from TERMS array
      FOUND = .FALSE.
      MODEL_SPEC_SPEC(TMOD) = ' '
      TLEN = 1
      ICOMP = 1
      DO WHILE ( (ICOMP.LE.MAXCOMP) .AND. .NOT. FOUND )
        IF ( TERMS(ICOMP,ITERM) .EQ. 0 ) THEN
          FOUND = .TRUE.
        ELSE
          KLEN = CHR_LEN(MODEL_SPEC_KEY(IMOD,TERMS(ICOMP,ITERM)))
          MODEL_SPEC_SPEC(TMOD) = MODEL_SPEC_SPEC(TMOD)(:TLEN)//'*'//
     :                 MODEL_SPEC_KEY(IMOD,TERMS(ICOMP,ITERM))(:KLEN)
          TLEN = TLEN + KLEN + 1
          ICOMP = ICOMP + 1
        END IF
      END DO
      MODEL_SPEC_SPEC(TMOD) = MODEL_SPEC_SPEC(TMOD)(3:TLEN)

*    Number of components in term model
      MODEL_SPEC_NCOMP(TMOD) = ICOMP - 1

*    Calculate the polish string. Something seriously wrong if this doesn't
*    work!
      CALL FIT_POLTRAN( MODEL_SPEC_SPEC(TMOD), MODEL_SPEC_POLISH(TMOD),
     :                  STATUS )

*    Top-level MODEL components
      MODEL_SPEC_GENUS(TMOD) = MODEL_SPEC_GENUS(IMOD)
      MODEL_SPEC_STACKPTR(TMOD) = MODEL_SPEC_STACKPTR(IMOD)

*    Copy parameter stuff from MODEL
      MODEL_SPEC_NPAR(TMOD) = 0
      DO ICOMP = 1, MODEL_SPEC_NCOMP(TMOD)

*      Original model component number
        MCOMP = TERMS(ICOMP,ITERM)

*      First the stuff which is stored for each pmodel
        MODEL_SPEC_KEY(TMOD,ICOMP) = MODEL_SPEC_KEY(IMOD,MCOMP)
        MODEL_SPEC_ADDITIVE(TMOD,ICOMP) =
     :                                  MODEL_SPEC_ADDITIVE(IMOD,MCOMP)

*      Last MODEL parameter in this pmodel
        IF ( MCOMP .EQ. MODEL_SPEC_NCOMP(IMOD) ) THEN
          LASTPAR = MODEL_SPEC_NPAR(IMOD)
        ELSE
          LASTPAR = MODEL_SPEC_ISTART(IMOD,MCOMP+1)-1
        END IF

*      First parameter for this pmodel
        MODEL_SPEC_ISTART(TMOD,ICOMP) = MODEL_SPEC_NPAR(TMOD) + 1

*      Copy parameters into local array
        CALL ARR_COP1R( LASTPAR-MODEL_SPEC_ISTART(IMOD,MCOMP)+1,
     :                  PARAM(MODEL_SPEC_ISTART(IMOD,MCOMP)),
     :                  TPARAM(MODEL_SPEC_ISTART(TMOD,ICOMP)), STATUS )

*      Increment local parameter counter
        MODEL_SPEC_NPAR(TMOD) = MODEL_SPEC_NPAR(TMOD) + LASTPAR -
     :                          MODEL_SPEC_ISTART(IMOD,MCOMP) + 1

      END DO

*    Perform the calculation
      CALL FIT_MCALC( TMOD, TPARAM, ND, NMDIM, IDIMM, NMDAT, NMBOUND,
     :                MLBOUND, MUBOUND, STACK, PRED, STATUS )

*    Does model have a negative sign?
      IF ( SIGNS(ITERM) .LT. 0 ) THEN
        CALL ARR_MULTR( -1.0, NMDAT, PRED, STATUS )
      END IF

*    Abort point
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_MCALCTERM', STATUS )
      END IF

      END
