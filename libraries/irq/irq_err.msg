! IRQ now uses a different facility code to IRA
! This allows us to use messgen
.TITLE        IRQ
.FACILITY     IRQ,1517/PREFIX=IRQ__

.SEVERITY     ERROR

! Historically IRQ has IRAS90 codes 31-60
.BASE         31
BADBT  <Bit value outside range [1,IRQ__QBITS] supplied>
BADDM  <Incorrect NDF dimensions supplied>
BADSL  <Invalid slot number supplied>
CMPLX  <Too many symbols in quality expression>
INCOM  <Supplied vector has different size to the NDF>
INTER  <Internal IRQ error>
INVID  <Invalid IRQ identifier supplied>
QBAD   <BAD values exists in the QUALITY component>
IVNDF  <Invalid NDF identifier found>
LSHRT  <Character variable too short>
MSOPD  <Missing operand>
MSOPT  <Missing or invalid operator>
MSPAR  <Unpaired parentheses>
MULT   <Multiple quality names information found>
NOMOR  <No free IRQ identifiers>
NOQNI  <No QUALITY_NAMES structure found in the NDF>
NOQNM  <A quality name could not be found>
NOWRT  <Write access to the NDF is unavailable>
QEXPL  <Too many characters in quality expression>
QIEXS  <Extension already contains a QUALITY_NAMES structure>
QLONG  <Quality name too long>
QNEXS  <Quality name is already defined>
QREFS  <Too many quality names in quality expression>
STKOV  <Evaluation stack overflow>
STKUN  <Evaluation stack underflow>
XBITS  <No bits left in QUALITY component>
BADNM  <An illegal quality name has been given>
BADQN  <Incomplete QUALITY_NAMES structure found>
BADST  <Incomplete slot structure found>

! Historically IRQ has IRAS90 codes 231 - 241
.BASE  231
QUNDF  <QUALITY component is undefined>
NOSPA  <Can't reclaim space in an instruction array>
MSDOT  <Missing delimiter "." in quality expression>
BADSY  <Unrecognised logical operator or constant>
NOOPS  <No operands can be found>

RDONL  <Quality name is read-only>

.END
