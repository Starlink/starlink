C
C ---------------------------------------------------------------------
C
      SUBROUTINE AGEXUS (SVAL,ZMIN,ZMAX,ZLOW,ZHGH,
     +                               ZDRA,NVIZ,IIVZ,NEVZ,IIEZ,UMIN,UMAX)
C
      DIMENSION SVAL(2),ZDRA(1)
C
C The routine AGEXUS is used by AGSTUP to determine tentative values of
C the user-window edge coordinates.  Its arguments are as follows:
C
C -- SVAL is the array of special values.
C
C -- ZMIN and ZMAX are user-supplied minimum and maximum values of the
C    data x (or y) coordinates.
C
C -- ZLOW and ZHGH are, respectively, the smallest and largest data
C    values to be considered in choosing the minimum and maximum, if
C    those values, as given by the user, are null.
C
C -- ZDRA, NVIZ, IIVZ, NEVZ, and IIEZ specify the array of x (or y)
C    data coordinates (see AGMAXI or AGMINI for complete description).
C
C -- UMIN and UMAX are returned with tentative minimum and maximum
C    values for use at the appropriate user-window edges (left/right
C    or bottom/top).
C
C The following common block contains AUTOGRAPH variables which are
C not control parameters.  The only one used here is SMRL, which is a
C (machine-dependent) small real which, when added to a number in the
C range (1,10), will round it upward without seriously affecting the
C leading significant digits.  The object of this is to get rid of
C strings of nines.
C
      COMMON /AGORIP/ SMRL , ISLD , MWCL,MWCM,MWCE,MDLA,MWCD,MWDQ ,
     +                INIF
C
C Assume initially that the user has provided actual values to be used.
C
      UMIN=ZMIN
      UMAX=ZMAX
C
C If either of the values is null, replace it by a data-based value.
C
      IF (UMIN.EQ.SVAL(1).OR.UMIN.EQ.SVAL(2))
     +                UMIN=AGMINI(SVAL(1),ZLOW,ZDRA,NVIZ,IIVZ,NEVZ,IIEZ)
      IF (UMAX.EQ.SVAL(1).OR.UMAX.EQ.SVAL(2))
     +                UMAX=AGMAXI(SVAL(1),ZHGH,ZDRA,NVIZ,IIVZ,NEVZ,IIEZ)
C
C Either or both values might still be null (if the user data was null).
C
      IF (UMIN.EQ.SVAL(1)) UMIN=UMAX
      IF (UMAX.EQ.SVAL(1)) UMAX=UMIN
C
C Check the relative values of UMIN and UMAX for problems.
C
      IF (ABS(UMIN-UMAX).LT.50.*SMRL*(ABS(UMIN)+ABS(UMAX))) GO TO 102
      IF (UMAX-UMIN) 101,102,103
  101 IF (ZMIN.NE.SVAL(1).AND.ZMIN.NE.SVAL(2)) UMAX=UMIN
      IF (ZMAX.NE.SVAL(1).AND.ZMAX.NE.SVAL(2)) UMIN=UMAX
C
  102 UMIN=UMIN-.5*ABS(UMIN)
      UMAX=UMAX+.5*ABS(UMAX)
      IF (UMIN.NE.UMAX) GO TO 103
      UMIN=-1.
      UMAX=+1.
C
C If the user wanted these values back-stored, do it.
C
  103 IF (ZMIN.EQ.SVAL(2)) ZMIN=UMIN
      IF (ZMAX.EQ.SVAL(2)) ZMAX=UMAX
C
C Done.
C
      RETURN
C
      END
