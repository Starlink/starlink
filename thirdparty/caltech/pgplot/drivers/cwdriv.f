C*CWDRIV -- PGPLOT driver for Colorwriter 6320 plotter
C+
      SUBROUTINE CWDRIV (OPCODE, RBUF, NBUF, CHR, LCHR)
      INTEGER       OPCODE,NBUF,LCHR
      REAL          RBUF(*)
      CHARACTER*(*)   CHR
C
C Supported device: Gould (now Bryans) Colourwriter 6320 or any
C device obeying Gould Plotter Language. [This appears to be very
C similar to HP-GL, Hewlett-Packard Graphics Language.]
C
C Device type code: /CW6320
C
C Default file name: pgplot.cwpl
C
C Default view surface dimensions: 280mm by 360mm (A3)
C
C Resolution: 0.025mm
C
C Colour Capability: Up to 10 pens. Default is pen 1 which is picked up
C on initialization without a call to PGSCI. Calls to PGSCI are
C interpreted as the pen number and colours therefore depend on how the
C pens have been loaded into the stalls. If a call is made for a pen
C higher than 10 the selected pen defaults to 1.
C
C Input Capability: Possible but not supported.
C
C File format: Ascii character strings.It is possible to send the
C data to a file which can then be copied to the plotter or
C on a terminal.
C--
C Version dated 880314.  Written by Len Pointon (Jodrell Bank).
C Revised 941201 by T. Pearson (standard Fortran-77).
C-----------------------------------------------------------------------
      INTEGER       LUN, IER, IC
      INTEGER       X1,Y1,X2,Y2,XOLD,YOLD
      INTEGER       GROPTX
      CHARACTER*10    MSG
      CHARACTER*(*)   DEVTYP,DEFNAM
      PARAMETER       (DEVTYP = 'CW6320 (Colorwriter 6320 plotter)')
      PARAMETER       (DEFNAM = 'pgplot.cwpl')
 
*             Go to the function specified by OPCODE
 
      GOTO (10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), OPCODE
 
*               Error return ,unrecognised value for OPCODE
 
      WRITE (MSG,'(I10)') OPCODE
      CALL GRWARN('Unknown opcode in '//DEVTYP//' device driver:'//MSG)
      NBUF = -1
      RETURN
 
*               OPCODE = 1, Return device name
 
  10  CHR = DEVTYP
      LCHR = LEN(DEVTYP)
      RETURN
 
*               OPCODE = 2, Return physical max & min for plot device and
*                           range of colour indices.
 
  20  RBUF(1) = 0.0
      RBUF(2) = 14400.0
      RBUF(3) = 0.0
      RBUF(4) = 11200.0
      RBUF(5) = 0.0
      RBUF(6) = 10.0
      NBUF = 6
      RETURN
 
*               OPCODE = 3, Return device resolution
 
  30  RBUF(1) = 1016.0
      RBUF(2) = 1016.0
      RBUF(3) = 12.0
      NBUF = 3
      RETURN

*               OPCODE = 4, Return misc. device information
*               (hardcopy,No cursor,No dashed lines,No area fill,
*                No thick lines)
 
  40  CHR = 'HNNNNNNNNN'
      LCHR = 10
      RETURN
 
*               OPCODE = 5, Return default file name
 
  50  CHR = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
 
*               OPCODE = 6, Return default physical size of plot
 
  60  RBUF(1) = 0.0
      RBUF(2) = 14400.0
      RBUF(3) = 0.0
      RBUF(4) = 11200.0
      NBUF = 4
      RETURN
 
*               OPCODE = 7, Return misc. defaults
*               (Character scale factor,not sure what to put here so
*                try value pinched from QMDRIVER)
 
  70  RBUF(1) = 8.0
      NBUF = 1
      RETURN
 
*               OPCODE = 8, Select plot
*               (Code set aside for future option,ignore it now)
 
  80  CONTINUE
      RETURN
 
*               OPCODE = 9, Open workstation
 
*               Get a logical unit number
 
  90  CALL GRGLUN(LUN)
      IF (LUN.EQ.-1) THEN
          CALL GRWARN('Failed to allocate I/O unit')
          RBUF(2) = 0.0
          NBUF = 2
          RETURN
      END IF
 
*               Open the file 
 
      NBUF = 2
      RBUF(1) = LUN

      IER = GROPTX(LUN, CHR(1:LCHR), DEFNAM, 1)
      IF (IER .NE. 0) THEN
          CALL GRWARN('Cannot open graphics device '//DEVTYP)
          RBUF(2) = 0.0
          CALL GRFLUN(LUN)
          RETURN
      ELSE
*         -- Initialize and select pen 1
          WRITE (LUN,*) 'IN;SP1;'
          RBUF(2) = 1.0
      END IF
      RETURN
 
*               OPCODE = 10, Close workstation
 
 100  CLOSE (LUN)
      CALL GRFLUN (LUN)
      RETURN
 
*               OPCODE = 11, Begin picture
*               (make sure pen is up and we are at the origin)
 
110   WRITE (LUN,*) 'PU;PA0,0'
      RETURN
 
*               OPCODE = 12, Draw line
 
 120  X1 = NINT(RBUF(1))
      Y1 = NINT(RBUF(2))
      X2 = NINT(RBUF(3))
      Y2 = NINT(RBUF(4))
 
*               Decide if move is to be with pen up or down.
*               If first new x,y is same as old x,y move with pen down.
*               If not, move to first x,y with pen up then to second
*               x,y with pen down.
      IF (X1 .EQ. XOLD .AND. Y1 .EQ. YOLD) THEN
          WRITE (LUN,'(A,I5,A,I5,A)') 'PD;PA',X2,',',Y2,';'
      ELSE
          WRITE (LUN,'(A,I5,A,I5,A)') 'PU;PA',X1,',',Y1,';'
          WRITE (LUN,'(A,I5,A,I5,A)') 'PD;PA',X2,',',Y2,';'
      END IF
 
*               Save last position
 
      XOLD=X2
      YOLD=Y2
      RETURN
 
*               OPCODE = 13, Draw dot
 
 130  X1 = NINT(RBUF(1))
      Y1 = NINT(RBUF(2))
      WRITE (LUN,'(A,I5,A,I5,A)') 'PU;PA',X1,',',Y1,';PD;PU;'
      RETURN
 
*               OPCODE = 14, End picture
*               (Advances the page one complete page)
 
 140  WRITE (LUN,*) 'AF;'
      RETURN
 
*               OPCODE = 15, Select colour index
 
 150  IC = NINT(RBUF(1))
      IF (IC .LT. 1) IC = 1
      IF (IC .GT. 10) IC = 1
      WRITE (LUN,'(A,I5,A)')  'SP',IC,';'
      RETURN
 
*               OPCODE = 16, Flush buffer
*               (Not applicable to hard copy device)
 
 160  CONTINUE
      RETURN
 
*               OPCODE = 17, Read cursor
*               (If we get here it must be an error)
 
 170  NBUF = -1
      LCHR = 0
      RETURN
 
*               OPCODE = 18, Erase alpha screen
*               (Not applicable)
 
 180  CONTINUE
      RETURN
 
*               OPCODE = 19, Set line style
*               (Ignore this)
 
 190  CONTINUE
      RETURN
 
*               OPCODE = 20, Polygon fill
*               (Ignore this)
 
 200  CONTINUE
      RETURN
 
*               OPCODE = 21, Set colour representation
*               (Ignore this)
 
 210  CONTINUE
      RETURN
 
*               OPCODE = 22, Set line width
*               (Ignore this)
 
 220  CONTINUE
      RETURN
 
*               OPCODE =23, Escape
*               (Ignore this)
 
 230  CONTINUE
      RETURN
 
      END
