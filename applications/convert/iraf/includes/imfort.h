# IMFORT.H -- IMFORT global definitions

define	MAX_NAXIS	3		# max axes in an imfort image
define	LEN_USERAREA	8192		# max space for user header keywords
define	SZ_KEYWORD	8		# max chars in a keyword name (FITS)
define	SZ_VALSTR	80		# max chars in a keyword record (FITS)
define	SZ_CMDLINE	256		# max length host command line
define	MAX_ARGS	32		# max command line arguments
define	SZ_DEVBLK	256		# alignment factor for pixel file

define	RO		1		# read only
define	WO		2		# write only
define	RW		3		# read write
define	NF		5		# new file

define	IM_HDRFP	Memi[$1]	# header file descriptor
define	IM_PIXFP	Memi[$1+1]	# pixel file descriptor
define	IM_UPDATE	Memi[$1+2]	# need to update image header on disk
define	IM_LINESIZE	Memi[$1+3]	# image physical line length, chars
define	IM_LINEBUFP	Memi[$1+4]	# line buffer pointer
define	IM_SZPIXEL	Memi[$1+5]	# pixel size, chars
define	IM_LENHDRMEM	Memi[$1+6]	# buffer length of std hdr + user area
define	IM_UABLOCKED	Memi[$1+7]	# is user area blocked to 80 cols/card

define	IE_ACCPIX	01		# error codes
define	IE_ALCPIX	02
define	IE_CLSHDR	03
define	IE_CLSPIX	04
define	IE_CREHDR	05
define	IE_IMDELETE	06
define	IE_IMDELNEXIM	07
define	IE_IMRENAME	08
define	IE_IMRNAMNEXIM	09
define	IE_EOF		10
define	IE_FLUSH	11
define	IE_GCMDLN	12
define	IE_MAGIC	13
define	IE_NEXARG	14
define	IE_NEXKW	15
define	IE_NONNUMARG	16
define	IE_NOTIMH	17
define	IE_NOTSHORT	18
define	IE_OPEN		19
define	IE_OPNPIX	20
define	IE_PIXTYPE	21
define	IE_RDPIX	22
define	IE_UPDHDR	23
define	IE_UPDRO	24
define	IE_WRHDR	25
define	IE_WRPIX	26
define	IE_XOOB		27
define	IE_YOOB		28
define	IE_ZOOB		29
define	IE_NAXIS	30
define	IE_AXLEN	31
define	IE_MKDIR	32
define	IE_PFNNUNIQ	33
define	IE_CLOBBER	34

define	IE_EOF		99
