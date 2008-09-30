{+

{   Name:
{      ESP.ICL

{   Purpose:
{      ESP start-up procedure.

{   Language:
{      ADAM ICL

{   Type of module:
{      ICL file

{   Arguments:

{   Invocation:
{      LOAD ESP

{   Description:
{      The ESP commands are defined.

{   Authors:
{      GJP: Grant Privett (STARLINK)
{      BLY: Martin Bly (Starlink, RAL)

{   History:
{      SEP-1994 (GJP)
{         Original version.
{      21-NOV-1994 (BLY):
{         Added: Help, extra command names, banner.
{      24-OCT-1996 (GJP):
{         Added: GAUFIT.


{   Bugs:

{-

{   Define main package help

defhelp esp     $ESP_HELP 0
defstring esphelp help esp

{   Basic command definitions.

define corr         $ESP_DIR/esp_mon corr
define esp_corr     $ESP_DIR/esp_mon corr

define ellfou       $ESP_DIR/esp_mon ellfou
define esp_ellfou   $ESP_DIR/esp_mon ellfou

define ellpro       $ESP_DIR/esp_mon ellpro
define esp_ellpro   $ESP_DIR/esp_mon ellpro

define fastmed      $ESP_DIR/esp_mon fastmed
define esp_fastmed  $ESP_DIR/esp_mon fastmed

define gaufit       $ESP_DIR/esp_mon gaufit
define esp_gaufit   $ESP_DIR/esp_mon gaufit

define graphs       $ESP_DIR/esp_mon graphs
define esp_graphs   $ESP_DIR/esp_mon graphs

define histpeak     $ESP_DIR/esp_mon histpeak
define esp_histpeak $ESP_DIR/esp_mon histpeak

define hsub         $ESP_DIR/esp_mon hsub
define esp_hsub     $ESP_DIR/esp_mon hsub

define loback       $ESP_DIR/esp_mon loback
define esp_loback   $ESP_DIR/esp_mon loback

define mask         $ESP_DIR/esp_mon mask
define esp_mask     $ESP_DIR/esp_mon mask

define mixup        $ESP_DIR/esp_mon mixup
define esp_mixup    $ESP_DIR/esp_mon mixup

define sector       $ESP_DIR/esp_mon sector
define esp_sector   $ESP_DIR/esp_mon sector

define selfc        $ESP_DIR/esp_mon selfc
define esp_selfc    $ESP_DIR/esp_mon selfc

define selfcw       $ESP_DIR/esp_mon selfcw
define esp_selfcw   $ESP_DIR/esp_mon selfcw

define skew         $ESP_DIR/esp_mon skew
define esp_skew     $ESP_DIR/esp_mon skew

define topped       $ESP_DIR/esp_mon topped
define esp_topped   $ESP_DIR/esp_mon topped

{   Help definitions.

defhelp corr     $ESP_HELP
defhelp ellfou   $ESP_HELP
defhelp ellpro   $ESP_HELP
defhelp fastmed  $ESP_HELP
defhelp gaufit   $ESP_HELP
defhelp graphs   $ESP_HELP
defhelp histpeak $ESP_HELP
defhelp hsub     $ESP_HELP
defhelp loback   $ESP_HELP
defhelp mask     $ESP_HELP
defhelp mixup    $ESP_HELP
defhelp sector   $ESP_HELP
defhelp selfc    $ESP_HELP
defhelp selfcw   $ESP_HELP
defhelp skew     $ESP_HELP
defhelp topped   $ESP_HELP

{   Announce the ESP commands are available.

print " "
print "   ESP commands are now available (Version 0.11-4)."
print " "
print "   Type `help esp' or `esphelp' for help on ESP commands."
print " "

