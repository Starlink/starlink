{+

{   Name:
{      STARMAN.ICL

{   Purpose:
{      STARMAN start-up procedure.

{   Language:
{      ADAM ICL

{   Type of module:
{      ICL file

{   Arguments:

{   Invocation:
{      LOAD STARMAN

{   Description:
{      The STARMAN commands are defined.

{   Authors:
{      GJP: Grant Privett (Starlink, Cardiff)
{      BLY: Martin Bly (Starlink, RAL)
{      AJP: Alan Penny (RAL)

{   History:
{      SEP-1994 (GJP)
{         Originial version.
{      21-NOV-1994 (BLY):
{         Added: Help, extra command names, banner.
{      FEB-1995 (AJP)
{         Starman version.
{      29-NOV-1995 (BLY):
{         Modified help invokations.

{   Bugs:

{-

{   Define main package help

defhelp starman     $STARMAN_HELP/starman_help_a 0

{   Basic command definitions for programs.

define addstars       $STARMAN_DIR/starman1_mon addstars
define automag        $STARMAN_DIR/starman1_mon automag
define average        $STARMAN_DIR/starman1_mon average
define chi            $STARMAN_DIR/starman1_mon chi
define diagram        $STARMAN_DIR/starman1_mon diagram
define dustring       $STARMAN_DIR/starman1_mon dustring
define imcalc         $STARMAN_DIR/starman1_mon imcalc
define imcube         $STARMAN_DIR/starman1_mon imcube
define imcut          $STARMAN_DIR/starman1_mon imcut
define imdes          $STARMAN_DIR/starman1_mon imdes
define imfits_dr      $STARMAN_DIR/starman1_mon imfits_dr
define imflash        $STARMAN_DIR/starman1_mon imflash

define imjoin         $STARMAN_DIR/starman2_mon imjoin
define imkey          $STARMAN_DIR/starman2_mon imkey
define import         $STARMAN_DIR/starman2_mon import
define imrotate       $STARMAN_DIR/starman2_mon imrotate
define imsmooth       $STARMAN_DIR/starman2_mon imsmooth
define imstat         $STARMAN_DIR/starman2_mon imstat
define imtype         $STARMAN_DIR/starman2_mon imtype
define imweed         $STARMAN_DIR/starman2_mon imweed
define interact       $STARMAN_DIR/starman2_mon interact
define measure        $STARMAN_DIR/starman2_mon measure
define profile        $STARMAN_DIR/starman2_mon profile
define simplemag      $STARMAN_DIR/starman2_mon simplemag
define sprinkle       $STARMAN_DIR/starman2_mon sprinkle
define starfind       $STARMAN_DIR/starman2_mon starfind

define tbcalc         $STARMAN_DIR/starman3_mon tbcalc
define tbchart        $STARMAN_DIR/starman3_mon tbchart
define tbcomps        $STARMAN_DIR/starman3_mon tbcomps
define tbcut          $STARMAN_DIR/starman3_mon tbcut
define tbdes          $STARMAN_DIR/starman3_mon tbdes
define tbjoin         $STARMAN_DIR/starman3_mon tbjoin
define tbkey          $STARMAN_DIR/starman3_mon tbkey
define tblist         $STARMAN_DIR/starman3_mon tblist
define tbload         $STARMAN_DIR/starman3_mon tbload
define tbmatch        $STARMAN_DIR/starman3_mon tbmatch
define tbnative       $STARMAN_DIR/starman3_mon tbnative
define tbnmatch       $STARMAN_DIR/starman3_mon tbnmatch
define tbplot         $STARMAN_DIR/starman3_mon tbplot
define tbpmatch       $STARMAN_DIR/starman3_mon tbpmatch
define tbrenum        $STARMAN_DIR/starman3_mon tbrenum

define tbsheet        $STARMAN_DIR/starman4_mon tbsheet
define tbsort         $STARMAN_DIR/starman4_mon tbsort
define tbstat         $STARMAN_DIR/starman4_mon tbstat
define tbtran_auto    $STARMAN_DIR/starman4_mon tbtran_auto
define tbtran_do      $STARMAN_DIR/starman4_mon tbtran_do
define tbtran_load    $STARMAN_DIR/starman4_mon tbtran_load
define tbtran_make    $STARMAN_DIR/starman4_mon tbtran_make
define tbvalue        $STARMAN_DIR/starman4_mon tbvalue
define tbweed         $STARMAN_DIR/starman4_mon tbweed
define unccd          $STARMAN_DIR/starman4_mon unccd

define stm_addstars       $STARMAN_DIR/starman1_mon addstars
define stm_automag        $STARMAN_DIR/starman1_mon automag
define stm_average        $STARMAN_DIR/starman1_mon average
define stm_chi            $STARMAN_DIR/starman1_mon chi
define stm_diagram        $STARMAN_DIR/starman1_mon diagram
define stm_dustring       $STARMAN_DIR/starman1_mon dustring
define stm_imcalc         $STARMAN_DIR/starman1_mon imcalc
define stm_imcube         $STARMAN_DIR/starman1_mon imcube
define stm_imcut          $STARMAN_DIR/starman1_mon imcut
define stm_imdes          $STARMAN_DIR/starman1_mon imdes
define stm_imfits_dr      $STARMAN_DIR/starman1_mon imfits_dr
define stm_imflash        $STARMAN_DIR/starman1_mon imflash

define stm_imjoin         $STARMAN_DIR/starman2_mon imjoin
define stm_imkey          $STARMAN_DIR/starman2_mon imkey
define stm_import         $STARMAN_DIR/starman2_mon import
define stm_imrotate       $STARMAN_DIR/starman2_mon imrotate
define stm_imsmooth       $STARMAN_DIR/starman2_mon imsmooth
define stm_imstat         $STARMAN_DIR/starman2_mon imstat
define stm_imtype         $STARMAN_DIR/starman2_mon imtype
define stm_imweed         $STARMAN_DIR/starman2_mon imweed
define stm_interact       $STARMAN_DIR/starman2_mon interact
define stm_measure        $STARMAN_DIR/starman2_mon measure
define stm_profile        $STARMAN_DIR/starman2_mon profile
define stm_simplemag      $STARMAN_DIR/starman2_mon simplemag
define stm_sprinkle       $STARMAN_DIR/starman2_mon sprinkle
define stm_starfind       $STARMAN_DIR/starman2_mon starfind

define stm_tbplot         $STARMAN_DIR/starman3_mon tbplot
define stm_tbcalc         $STARMAN_DIR/starman3_mon tbcalc
define stm_tbchart        $STARMAN_DIR/starman3_mon tbchart
define stm_tbcomps        $STARMAN_DIR/starman3_mon tbcomps
define stm_tbcut          $STARMAN_DIR/starman3_mon tbcut
define stm_tbdes          $STARMAN_DIR/starman3_mon tbdes
define stm_tbjoin         $STARMAN_DIR/starman3_mon tbjoin
define stm_tbkey          $STARMAN_DIR/starman3_mon tbkey
define stm_tblist         $STARMAN_DIR/starman3_mon tblist
define stm_tbload         $STARMAN_DIR/starman3_mon tbload
define stm_tbmatch        $STARMAN_DIR/starman3_mon tbmatch
define stm_tbnative       $STARMAN_DIR/starman3_mon tbnative
define stm_tbnmatch       $STARMAN_DIR/starman3_mon tbnmatch
define stm_tbplot         $STARMAN_DIR/starman3_mon tbplot
define stm_tbpmatch       $STARMAN_DIR/starman3_mon tbpmatch
define stm_tbrenum        $STARMAN_DIR/starman3_mon tbrenum

define stm_tbsheet        $STARMAN_DIR/starman4_mon tbsheet
define stm_tbsort         $STARMAN_DIR/starman4_mon tbsort
define stm_tbstat         $STARMAN_DIR/starman4_mon tbstat
define stm_tbtran_auto    $STARMAN_DIR/starman4_mon tbtran_auto
define stm_tbtran_do      $STARMAN_DIR/starman4_mon tbtran_do
define stm_tbtran_load    $STARMAN_DIR/starman4_mon tbtran_load
define stm_tbtran_make    $STARMAN_DIR/starman4_mon tbtran_make
define stm_tbvalue        $STARMAN_DIR/starman4_mon tbvalue
define stm_tbweed         $STARMAN_DIR/starman4_mon tbweed
define stm_unccd          $STARMAN_DIR/starman4_mon unccd

defstring starmandemo        sh $STARMAN_DIR/starmandemo
defstring starmanhelp        sh $STARMAN_DIR/starmanhelp
defstring starmangripe       sh $STARMAN_DIR/starmangripe
defstring starmanhyperhelp   sh $STARMAN_DIR/starmanhyperhelp
defstring starmanhypergripe  sh $STARMAN_DIR/starmanhypergripe


{   Help definitions.

defhelp addstars       $STARMAN_HELP/starman_help_a
defhelp automag        $STARMAN_HELP/starman_help_a
defhelp average        $STARMAN_HELP/starman_help_a
defhelp chi            $STARMAN_HELP/starman_help_a
defhelp diagram        $STARMAN_HELP/starman_help_a
defhelp dustring       $STARMAN_HELP/starman_help_a
defhelp imcalc         $STARMAN_HELP/starman_help_a
defhelp imcube         $STARMAN_HELP/starman_help_a
defhelp imcut          $STARMAN_HELP/starman_help_a
defhelp imdes          $STARMAN_HELP/starman_help_a
defhelp imfits_dr      $STARMAN_HELP/starman_help_a
defhelp imflash        $STARMAN_HELP/starman_help_a
defhelp imjoin         $STARMAN_HELP/starman_help_a
defhelp imkey          $STARMAN_HELP/starman_help_a
defhelp import         $STARMAN_HELP/starman_help_a
defhelp imrotate       $STARMAN_HELP/starman_help_a
defhelp imsmooth       $STARMAN_HELP/starman_help_a
defhelp imstat         $STARMAN_HELP/starman_help_a
defhelp imtype         $STARMAN_HELP/starman_help_a
defhelp imweed         $STARMAN_HELP/starman_help_a
defhelp interact       $STARMAN_HELP/starman_help_a
defhelp measure        $STARMAN_HELP/starman_help_a
defhelp profile        $STARMAN_HELP/starman_help_a
defhelp simplemag      $STARMAN_HELP/starman_help_a
defhelp sprinkle       $STARMAN_HELP/starman_help_a
defhelp starfind       $STARMAN_HELP/starman_help_a
defhelp tbplot         $STARMAN_HELP/starman_help_a
defhelp tbcalc         $STARMAN_HELP/starman_help_a
defhelp tbchart        $STARMAN_HELP/starman_help_a
defhelp tbcomps        $STARMAN_HELP/starman_help_a
defhelp tbcut          $STARMAN_HELP/starman_help_a
defhelp tbdes          $STARMAN_HELP/starman_help_a
defhelp tbjoin         $STARMAN_HELP/starman_help_a
defhelp tbkey          $STARMAN_HELP/starman_help_a
defhelp tblist         $STARMAN_HELP/starman_help_a
defhelp tbload         $STARMAN_HELP/starman_help_a
defhelp tbmatch        $STARMAN_HELP/starman_help_a
defhelp tbnative       $STARMAN_HELP/starman_help_a
defhelp tbnmatch       $STARMAN_HELP/starman_help_a
defhelp tbplot         $STARMAN_HELP/starman_help_a
defhelp tbpmatch       $STARMAN_HELP/starman_help_a
defhelp tbrenum        $STARMAN_HELP/starman_help_a
defhelp tbsheet        $STARMAN_HELP/starman_help_a
defhelp tbsort         $STARMAN_HELP/starman_help_a
defhelp tbstat         $STARMAN_HELP/starman_help_a
defhelp tbtran_auto    $STARMAN_HELP/starman_help_a
defhelp tbtran_do      $STARMAN_HELP/starman_help_a
defhelp tbtran_load    $STARMAN_HELP/starman_help_a
defhelp tbtran_make    $STARMAN_HELP/starman_help_a
defhelp tbvalue        $STARMAN_HELP/starman_help_a
defhelp tbweed         $STARMAN_HELP/starman_help_a
defhelp unccd          $STARMAN_HELP/starman_help_a
defhelp starmanhelp    $STARMAN_HELP/starman_help_a
defhelp starmangripe   $STARMAN_HELP/starman_help_a


{   Announce the STARMAN commands are available.

print ' '
print '   ____________________________________________________________________ '
print '  |  Starman - Stellar Photometry               Version PKG_VERS  Dec 95  |'
print '  |            Image and Table Handling                                |'
print '  |                                                                    |'
print '  |1) Help:                         starmanhyperhelp / starmanhelp     |'
print '  |2) Advice/Bug Reports:           starmanhypergripe / starmangripe   |'
print '  |                                   - or - mail alan.penny@rl.ac.uk  |'
print '  |3) Prompts in programs:          ? or ?? for help : !! to exit      |'
print '  |4) Default data for a prompt:    $STARMAN_DATA/progam_parameter     |'
print '  |                                    e.g. $STARMAN_DATA/measure_in   |'
print '  |5) Demonstration:                starmandemo                        |'
print '  |                                                                    |'
print '  |All rights reserved. Starlink copyright and disclaimer notices apply|'
print '   --------------------------------------------------------------------'
print ' '
