.TITLE		AST_ERR
.FACILITY	AST,1521/PREFIX=AST__
!author		R.F. Warren-Smith & D.S. Berry (STARLINK)

.SEVERITY	ERROR
.BASE 300

ATGER           <attribute getting error>
ATSER           <attribute setting error>
ATTIN           <attribute value invalid>
AXIIN           <axis index invalid>
BADAT           <bad attribute name>
BADBX           <zero-sized box given>
BADIN           <bad input data>
BADNI           <bad number of input coordinates>
BADNO           <bad number of output coordinates>
BADPW           <PolyMap contains illegal power value>
BADSM           <ShiftMap contains no shift information>
BADWM           <WinMap contains no bounds information>
BDBRK           <bad break index>
BDFMT           <bad field specifier>
BDFTS           <invalid FITS keyword value found>
BDOBJ           <inappropriate Object supplied>
CLPAX           <wrong number of clipping axes>
CORNG           <range of coordinates invalid>
CVBRK           <too many breaks in a curve>
DIMIN           <array dimensions invalid>
DTERR           <date/time error>
ENDIN           <invalid use of astEnd>
EOCHN           <end of input Channel encountered>
EXPIN           <attempt to export Object pointer from level zero>
FCRPT           <corrupted FitsChan supplied>
FMTER           <error while formatting coordinate value>
FRMIN           <Frame index invalid>
FRSIN           <FrameSet invalid>
FTCNV           <cannot convert FITS data value type>
GRFER           <low level graphics error>
INHAN           <invalid Handle>
INNCO           <incompatible numbers of coordinates>
INTER           <internal programming error>
INTRD           <incompatible transformation directions>
KYCIR           <circular dependency between KeyMaps>
LDERR           <class loader error>
LUTII           <invalid lookup table increment>
LUTIN           <invalid number of lookup table elements>
MEMIN           <requested memory size invalid>
MTR23           <not a 2d or 3d MatrixMap>
MTRAX           <null rotation axis supplied>
MTRML           <bad matrix shapes for multiplication>
MTRMT           <null matrix supplied>
NAXIN           <number of axes invalid>
NCHIN           <number of characters invalid>
NCOIN           <number of coordinates invalid>
NCPIN           <number of coordinates per point invalid>
NELIN           <number of array elements invalid>
NOCTS           <number of output coordinates too small>
NODEF           <transformation not defined>
NOFTS           <required FITS keywords missing>
NOMEM           <unable to allocate memory>
NOPTS           <number of output points too small>
NOWRT           <attribute is read-only>
NPTIN           <number of points invalid>
OBJIN           <Object invalid>
OPT             <invalid Plot option>
PDSIN           <points data structure invalid>
PLFMT           <no numerical labels can be produced>
PRMIN           <permutation invalid>
PTRIN           <pointer invalid>
PTRNG           <range of points invalid>
RDERR           <read error>
REGIN           <invalid or corrupted Region structure supplied>
REMIN           <invalid attempt to remove last Frame>
SCSIN           <sky coordinate system invalid>
SELIN           <axis selection invalid>
SLAIN           <bad SLALIB transformation type>
TRNND           <coordinate transformation not defined>
UNMQT           <unmatched quotes>
VSMAL           <valid area too small>
WCSAX           <non-existent longitude or latitude axis>
WCSNC           <too few mapping coordinates>
WCSPA           <invalid projection parameters>
WCSTY           <unknown projection type>
XSOBJ           <too many Objects in use at once>
ZOOMI           <zoom factor invalid>

!  New codes introduced for V1.1.
BADCI           <bad coordinate index>
ILOST           <FrameSet integrity lost>
ITFER           <error in IntraMap transformation function>
ITFNI           <IntraMap transformation function name invalid>
MBBNF           <Mapping bounding box not found>
MRITF           <multiple registration of IntraMap transformation function>
OCLUK           <Object class unknown>
UNFER           <error while unformatting a coordinate value>
URITF           <unregistered IntraMap transformation function>

!  New codes introduced for V1.2.
GBDIN           <grid bounds invalid>
NGDIN           <number of grid dimensions invalid>
PATIN           <positional accuracy tolerance invalid>
SISIN           <sub-pixel interpolation scheme invalid>
SSPIN           <scale size in pixels invalid>

!  New codes introduced for V1.3.
UINER           <error in user-supplied sub-pixel interpolation function>
UK1ER           <error in user-supplied 1-d sub-pixel interpolation kernel>

!  New codes introduced for V1.4.
COMIN           <invalid comma in expression>
CONIN           <invalid constant in expression>
DUVAR           <duplicate variable name>
INNTF           <invalid number of transformation functions>
MIOPA           <missing or invalid operand in expression>
MIOPR           <missing or invalid operator in expression>
MISVN           <missing variable name>
MLPAR           <missing left parenthesis in expression>
MRPAR           <missing right parenthesis in expression>
NORHS           <missing right hand side in function>
UDVOF           <undefined variable or function in expression>
VARIN           <variable name invalid>
WRNFA           <wrong number of function arguments in expression>

! New codes introduced for V2.0.
BADUN           <invalid units specification>
NORSF           <no rest frequency is defined>
NOSOR           <no standard of rest is defined>
SPCIN           <invalid SpecMap>

! New codes introduced for V3.1
XMLNM           <invalid XML name or prefix>
XMLCM           <invalid XML comment text>
XMLPT           <invalid XML processing instruction target text>
XMLIT           <invalid XML content item index>
XMLWF           <supplied XML document is not well formed>

! New codes introduced for V3.2
ZERAX           <Range of log axis scale includes zero>

! New codes introduced for V3.3
BADOC           <Invalid parameters for offset sky coordinate system>

! New codes introduced for V3.5-2
MPGER           <error getting a named value from a KeyMap>
MPIND           <invalid integer index supplied for a KeyMap entry>

! New codes introduced for V3.6
REGCN           <region cannot be re-centred>
NOVAL           <attribute has no usable value>
INCTS           <incompatible time scales>
TIMIN           <invalid TimeMap>
STCKEY          <cannot use supplied AstroCoords info>
STCIND          <invalid AstroCoords index>

! New codes introduced for V4.0
CNFLX           <cannot conserve flux whilst resampling an array of data>
TUNAM           <Unknown AST tuning parameter name supplied>
BDPAR           <Bad value supplied for a public function parameter>


! New codes introduced for V4.6
3DFSET          <Supplied FrameSet does not contain any independent axes>
PXFRRM          <Attempt to delete original Plot3D base Frame>
BADSUB          <Illegal syntax for string substitution template>
BADFLG          <Incompatible flags for re-sampling or re-binning>

! New codes introduced for V5.0
LCKERR          <Error locking or unlocking an AST Object>
FUNDEF          <FITS keyword had undefined value>

! New codes introduced for V5.2
MPVIN           <invalid integer index supplied for a KeyMap vector element>
OPRIN           <operation specifier invalid>
NONIN           <no inside point found>
MPKER           <requested key not found in KeyMap>
MPPER           <error putting a named value into a KeyMap>
BADKEY	        <Attempt made to add an entry to a locked KeyMap>

! New codes introduced for V5.4
BADTYP          <Bad data type>
OLDCOL          <Column already exists with different properties>
BADNULL         <Bad null value for a FITS table column>
BIGKEY          <Key string is too long>
BADCOL          <No such column exists in the table>
BIGTAB          <Table is too large>
BADSIZ          <Invalid array size>
BADTAB          <Error reading WCS from FITS binary table>
NOTAB           <Cannot access FITS binary table>

! New codes introduced for V5.7
LEVMAR          <Error in levmar Levenberg-Marquardt code>
NOFIT           <Fit failed>
ISNAN           <A transformation generated one or more NaN values>
WRERR           <write error>

! New codes introduced for V7.3
BDVNM           <Bad variant Mapping name>
MIRRO           <Attempt to add a variant Mapping to a mirror Frame>

! New codes introduced for V8.0
MNPCK           <Error in cminpack Levenberg-Marquardt code>

! New codes introduced for V8.0.3
EXSPIX          <Supplied array has too many pixels>

! New codes introduced for V8.0.4
NOCNV           <No mapping found between coordinate systems>

! New codes introduced for V8.2.0
IMMUT           <Attempt to change an immutable attribute>

.END
