$!
$!  IUECNV.COM
$!
$!  Thanks to Tim Gledhill for ADAM_IFL correction.
@!
$IF "''p1'" .EQS. ""
$THEN
$ TYPE SYS$INPUT

	This procedure converts IUEDR datasets from the old VMS specific
	format into the new STARLINK standard NDF format.

	To use it you should specify the dataset to be converted e.g.

	    @IUECNV LWP12345

	The procedure will produce a new formatted version of the
	.UEC file and NDF versions of any other files in the
	dataset.  The new _UED file combines old style .UED and .UEQ files.

	Once the dataset is converted it can be transferred to a UNIX
	machine using NFS/FTP.

	If you use FTP:
	- all the .sdf files MUST be transferred using binary mode.
	- the new .UEC files MUST be transferred using ascii mode.

$ELSE
$ IF "''ADAMDEV'" .EQS. ""
$ THEN
$   ADAMSTART
$ ENDIF
$ FILE = F$SEARCH("IUECNV.IFL")
$ IF FILE .EQS. ""
$ THEN
$   COPY SYS$INPUT IUECNV.IFL;1
    interface IUECNV
    endinterface
$
$ ENDIF
$ WHEREAMI = F$PARSE ( F$ENVIRONMENT ( "PROCEDURE" ) ,,, "DEVICE" ) + -
	     F$PARSE ( F$ENVIRONMENT ( "PROCEDURE" ) ,,, "DIRECTORY" )
$ IUECNV:=="$''WHEREAMI'IUECNV"
$ DEFINE IUE_DATASET 'P1'\
$ DEFINE/USER ADAM_IFL []
$ IUECNV
$ENDIF
