$! Builds a VMS release of IDI starting from a directory of source files
$!
$! The source directory is specified by P1 (defaults to [.SOURCE])
$!
$!
$ IF P1.EQS."" THEN P1 = "[.SOURCE]"
$ DEFINE IDI_SOURCE 'p1'
$ DEFINE IDI_DIR SYS$DISK:[]
$!
$! Fetch the include files
$!
$  call fetch DEVICE.DEP
$  call fetch IDI.H
$  call fetch IDIFUNCS.H
$  call fetch IDISTRUCT_E.H
$  call fetch IDI_ERR.H
$  call fetch III.H
$  call fetch KWM.H
$  call fetch VDM.H
$  call fetch X11DEFS.H
$  call fetch IDI_DID
$  RENAME IDI_DID. IDI_DID.FOR
$  call fetch IDI_ERR
$  RENAME IDI_ERR. IDI_ERR.FOR
$  call fetch IDI_PAR
$  RENAME IDI_PAR. IDI_PAR.FOR
$  call fetch IDI_COMPAR
$  RENAME IDI_COMPAR. IDI_COMPAR.FOR
$  call fetch IKN_COMBUF.FOR
$  call fetch IKN_COMCH.FOR
$  call fetch IKN_COMEM.FOR
$  call fetch IKN_COMID.FOR
$  call fetch IKN_COMINT.FOR
$  call fetch IKN_COMLUT.FOR
$  call fetch IKN_COMPOS.FOR
$  call fetch IKN_COMTIM.FOR
$  call fetch IKN_PAR.FOR
$!
$  LIBRARY/CREATE IDIINC/TEXT
$  LIBRARY/LOG IDIINC/TEXT IDI_DID.FOR
$  LIBRARY/LOG IDIINC/TEXT IDI_ERR.FOR
$  LIBRARY/LOG IDIINC/TEXT IDI_PAR.FOR
$  LIBRARY/LOG IDIINC/TEXT IKN_COMBUF.FOR
$  LIBRARY/LOG IDIINC/TEXT IKN_COMCH.FOR
$  LIBRARY/LOG IDIINC/TEXT IKN_COMEM.FOR
$  LIBRARY/LOG IDIINC/TEXT IKN_COMID.FOR
$  LIBRARY/LOG IDIINC/TEXT IKN_COMINT.FOR
$  LIBRARY/LOG IDIINC/TEXT IKN_COMLUT.FOR
$  LIBRARY/LOG IDIINC/TEXT IKN_COMPOS.FOR
$  LIBRARY/LOG IDIINC/TEXT IKN_COMTIM.FOR
$  LIBRARY/LOG IDIINC/TEXT IKN_PAR.FOR
$  DELETE/LOG IKN_*.FOR;*
$  PURGE IDIINC.TLB
$!
$! Compile the library
$!
$!
$  lib = "IDI_X"
$  call compile IDIENV.C
$  call compile IDIEXTRA.C
$  call compile IDIIC.C
$  call compile IDILOCAL.C
$  call compile IDIOTHER.C
$  call compile IDIUTIL.C
$  call compile IIC.C
$  call compile IID.C
$  call compile IIG.C
$  call compile III.C
$  call compile IIL.C
$  call compile IIM.C
$  call compile IIR.C
$  call compile IIZ.C
$  call compile KWM.C
$  call compile RINT.C
$  call compile VDM.C
$  call compile VDMDD.C
$!
$  PURGE IDI_X.OLB, IDI_X.TLB
$!
$!  Build the X description file
$!
$  CALL FETCH DCTPREP.C
$  CC DCTPREP
$  LINK DCTPREP,SYS$INPUT/OPT
IDI_X/LIBRARY
SYS$SHARE:VAXCRTL/SHARE
$  DELETE DCTPREP.OBJ;*
$  call fetch XWORKS.DCT
$  SET NOON
$  RUN DCTPREP
y
$  ON ERROR THEN EXIT
$  PURGE XWORKS.DAT
$!
$  LIBRARY/CREATE IDI_XINC/TEXT
$  LIBRARY/LOG IDI_XINC/TEXT *.H, DEVICE.DEP
$  DELETE/LOG *.H;*, DEVICE.DEP;*
$!
$  call fetch IDI_DEV.COM
$  @SSC:LOGIN
$  ADAMSTART
$  ADAMDEV
$!
$  lib = "IDI"
$  call compile IDI.F
$  call compile IDBAND.FOR
$  call compile IDIPIB.FOR
$  call compile IDIPIL.FOR
$  call compile IDLMAC.FOR
$  call compile IDLROT.FOR
$  call compile IDNAME.FOR
$  call compile IDRECO.FOR
$  call compile IDSACO.FOR
$  call compile IDSRCO.FOR
$  call compile IDTBEG.FOR
$  call compile IDTCON.FOR
$  call compile IKNAMY.FOR
$  call compile IKNBLM.FOR
$  call compile IKNCLO.FOR
$  call compile IKNCMY.FOR
$  call compile IKNCON.FOR
$  call compile IKNEBM.FOR
$  call compile IKNEIW.FOR
$  call compile IKNENC.FOR
$  call compile IKNENI.FOR
$  call compile IKNERR.FOR
$  call compile IKNGEP.FOR
$  call compile IKNGIE.FOR
$  call compile IKNGLD.FOR
$  call compile IKNGLE.FOR
$  call compile IKNGRE.FOR
$  call compile IKNGSE.FOR
$  call compile IKNIAG.FOR
$  call compile IKNIBB.FOR
$  call compile IKNIBW.FOR
$  call compile IKNINC.FOR
$  call compile IKNINR.FOR
$  call compile IKNINT.FOR
$  call compile IKNOBB.FOR
$  call compile IKNOBW.FOR
$  call compile IKNOLT.FOR
$  call compile IKNOPN.FOR
$  call compile IKNOUT.FOR
$  call compile IKNPEP.FOR
$  call compile IKNPLY.FOR
$  call compile IKNQCI.FOR
$  call compile IKNQCR.FOR
$  call compile IKNQDC.FOR
$  call compile IKNQDV.FOR
$  call compile IKNQID.FOR
$  call compile IKNRCO.FOR
$  call compile IKNRCP.FOR
$  call compile IKNRIT.FOR
$  call compile IKNRLC.FOR
$  call compile IKNRLT.FOR
$  call compile IKNRMY.FOR
$  call compile IKNRRI.FOR
$  call compile IKNRST.FOR
$  call compile IKNRSZ.FOR
$  call compile IKNRZP.FOR
$  call compile IKNSBV.FOR
$  call compile IKNSCV.FOR
$  call compile IKNSDP.FOR
$  call compile IKNSEL.FOR
$  call compile IKNSLT.FOR
$  call compile IKNSMV.FOR
$  call compile IKNSNP.FOR
$  call compile IKNSRV.FOR
$  call compile IKNSSS.FOR
$  call compile IKNSTC.FOR
$  call compile IKNSTI.FOR
$  call compile IKNSTW.FOR
$  call compile IKNTXT.FOR
$  call compile IKNUPD.FOR
$  call compile IKNWCP.FOR
$  call compile IKNWIT.FOR
$  call compile IKNWLT.FOR
$  call compile IKNWMY.FOR
$  call compile IKNWRI.FOR
$  call compile IKNWSC.FOR
$  call compile IKNWZM.FOR
$  call compile IKNWZP.FOR
$  call compile IDI_ADAM.F
$!
$  PURGE IDI.OLB, IDI.TLB
$  DELETE IDI_COMPAR.FOR;*
$!
$!  Build the shareable libraries
$!
$  call fetch BUILD_IDI.COM
$  call fetch BUILD_IDI.MAR
$  call fetch BUILD_IDI.OPT
$ @BUILD_IDI
$  DELETE BUILD_IDI.OBJ;*
$  call fetch BUILD_IDI_ADAM.COM
$  call fetch BUILD_IDI_ADAM.MAR
$  call fetch BUILD_IDI_ADAM.OPT
$  @BUILD_IDI_ADAM
$  DELETE BUILD_IDI_ADAM.OBJ;*
$!
$!  Other misc stuff
$!
$  call fetch IDI.LSE
$  call fetch IDI_HELP.HLP
$  LIBRARY/LOG/CREATE IDI_HELP/HELP IDI_HELP
$!
$  call fetch IDI_ERR.HTXT
$  call fetch IDI_ERR.MSG
$  call fetch IKNWDT.DAT
$!
$  call fetch IDI_LINK.OPT
$  call fetch IDI_LINK_ADAM.OPT
$  call fetch IDI_TEST.F
$  RENAME IDI_TEST.F IDI_TEST.FOR
$  call fetch IDI_TEST.IFL
$!
$  SET PROTECTION=W:RE *.*
$!
$fetch: subroutine
$  COPY/LOG IDI_SOURCE:'p1' []
$  PURGE 'p1'
$endsubroutine
$!
$compile: subroutine
$  if f$search("''lib'.olb").eqs."" THEN LIBRARY/CREATE 'lib'
$  if f$search("''lib'.tlb").eqs."" THEN LIBRARY/CREATE/TEXT 'lib'
$  call fetch 'p1'
$  filename = f$parse(p1,,,"NAME")
$  ext = f$parse(p1,,,"TYPE")
$  if ext.eqs.".C" THEN CC/INCLUDE=CNF_DIR 'filename'
$  if ext.eqs.".F" THEN FORTRAN 'filename'.F
$  if ext.eqs.".FOR" THEN FORTRAN 'filename'
$  LIBRARY/LOG 'lib' 'filename'
$  LIBRARY/LOG 'lib'/TEXT 'p1'
$  DELETE/LOG 'filename'.OBJ;*
$  DELETE/LOG 'filename''ext';*
$ endsubroutine
