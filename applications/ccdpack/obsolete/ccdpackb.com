$!
$! CCDPACK batch job procedure.
$!
$! Generated: 24-NOV-1991 12:51:42.72.
$!
$! Set up batch job to run CCDPACK, restoring saved environment.
$!
$@CCDPACK_DIR:CCDBSTART DUVS4$DKA300:[PDRAPER.ADAM_12510181]  DISK$USER1:[PDRAPER.CCDPACK.BATCH]
$!
$! Execute user supplied procedure.
$!
$@DISK$USER1:[PDRAPER.CCDPACK.BATCH]CCDPACK_BATCH.COM;1
$!
$! Clear up the mess. Delete the saved environment
$! (remove the next command to retain the environment).
$!
$@CCDPACK_DIR:CCDBEND DUVS4$DKA300:[PDRAPER.ADAM_12510181] ADAM_12510181
$EXIT
$! $Id$
