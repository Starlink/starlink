.TITLE       PGP
.FACILITY    PGP,1508/PREFIX=PGP__


! These are internal PGP error codes

.SEVERITY	ERROR
GRGQFA	<GKS inquiry failed>
GRNODO	<No PGPLOT device open>
GRNOIN	<Cursor not available in non-interactive job>
GRNOCU	<PGPLOT Device does not have a cursor>
GRCUPO	<Error getting cursor position>
GRTMCF	<Too many cursor failures>
GRUACH	<Unable to assign channel to terminal>
GREWT	<Error writing to terminal>
GRUNET	<Unable to erase text screen>
GROBRC	<Obsolete routine called>
GRTMAP	<Too many active plots>
GRIWKI	<Invalid workstation id>
GRIWKN	<Invalid workstation name>
GRIWKT	<Invalid workstation type>
GRICAT	<Invalid workstation catagory>
GRUOWK	<Unable to open workstation>
GRIPLI	<Invalid PGPLOT plot identifier>
GRILCO	<Not a valid colour>
GRILLS	<Not a valid linestyle>
GRILLW	<Not a valid line width>
GRNOPG	<Workstation was not opened by PGPLOT>
GRILFO	<Not a valid font>
GRURFF	<Unable to read font file>
GRUGTN	<Unable to get terminal name>
GRURT	<Unable to read from terminal>
GRNOSA	<Restore not preceeded by save>
GRPGER	<PGPLOT error>
GRILFS	<Not a valid fill area style>
GROUON	<PGPLOT Device is output only>
GRNOKE	<PGPLOT Device does not have a keyboard>
GRDYNE	<Error allocating dynamic memory>
GRNOAP	<Append qualifier not supported>
GRUNKQ	<Unknown qualifier>

! The next four error codes are used by the PGPLOT ADAM
! parameter routines (PGPPAR i nthe GRAPHPAR package).
! Included here since this will be the source of the
! installed PGP_ERR and associated fac data files.

.BASE		100
TOOUD   <No more available unit descriptors>
ILLAC   <Illegal access mode>
UNKPA   <Parameter not found>
ISACT   <Parameter currently active>

.END
