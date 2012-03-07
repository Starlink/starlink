/*
*+
*  Name:
*     palObs

*  Purpose:
*     Parameters of selected ground-based observing stations

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     int palObs( size_t n, const char * c,
*                 char * ident, size_t identlen,
*                 char * name, size_t namelen,
*                 double * w, double * p, double * h );

*  Arguments:
*     n = size_t (Given)
*         Number specifying the observing station. If 0
*         the identifier in "c" is used to determine the
*         observing station to use.
*     c = const char * (Given)
*         Identifier specifying the observing station for
*         which the parameters should be returned. Only used
*         if n is 0. Can be NULL for n>0. Case insensitive.
*     ident = char * (Returned)
*         Identifier of the observing station selected. Will be
*         identical to "c" if n==0. Unchanged if "n" or "c"
*         do not match an observing station. Should be at least
*         11 characters (including the trailing nul).
*     identlen = size_t (Given)
*         Size of the buffer "ident" including trailing nul.
*     name = char * (Returned)
*         Full name of the specified observing station. Contains "?"
*         if "n" or "c" did not correspond to a valid station. Should
*         be at least 41 characters (including the trailing nul).
*     w = double * (Returned)
*         Longitude (radians, West +ve). Unchanged if observing
*         station could not be identified.
*     p = double * (Returned)
*         Geodetic latitude (radians, North +ve). Unchanged if observing
*         station could not be identified.
*     h = double * (Returned)
*         Height above sea level (metres). Unchanged if observing
*         station could not be identified.

*  Returned Value:
*     palObs = int
*         0 if an observing station was returned. -1 if no match was
*         found.

*  Description:
*     Station numbers, identifiers, names and other details are
*     subject to change and should not be hardwired into
*     application programs.
*
*     All characters in "c" up to the first space are
*     checked;  thus an abbreviated ID will return the parameters
*     for the first station in the list which matches the
*     abbreviation supplied, and no station in the list will ever
*     contain embedded spaces. "c" must not have leading spaces.
*
*     IMPORTANT -- BEWARE OF THE LONGITUDE SIGN CONVENTION.  The
*     longitude returned by sla_OBS is west-positive in accordance
*     with astronomical usage.  However, this sign convention is
*     left-handed and is the opposite of the one used by geographers;
*     elsewhere in PAL the preferable east-positive convention is
*     used.  In particular, note that for use in palAop, palAoppa
*     and palOap the sign of the longitude must be reversed.
*
*     Users are urged to inform the author of any improvements
*     they would like to see made.  For example:
*
*         typographical corrections
*         more accurate parameters
*         better station identifiers or names
*         additional stations


*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Differs from the SLA interface in that the output short name
*       is not the same variable as the input short name. This simplifies
*       consting. Additionally the size of the output buffers are now
*       specified in the API and a status integer is returned.

*  History:
*     2012-03-06 (TIMJ):
*        Initial version containing entries from SLA/F as of 15 March 2002
*        with a 2008 tweak to the JCMT GPS position.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "star/util.h"
#include "pal.h"
#include "palmac.h"

#include <string.h>

/* Helper macros to convert degrees to radians in longitude and latitude */
#define WEST(ID,IAM,AS) PAL__DAS2R*((60.0*(60.0*(double)ID+(double)IAM))+(double)AS)
#define NORTH(ID,IAM,AS) WEST(ID,IAM,AS)
#define EAST(ID,IAM,AS) -1.0*WEST(ID,IAM,AS)
#define SOUTH(ID,IAM,AS) -1.0*WEST(ID,IAM,AS)

struct telData {
  double w;
  double p;
  double h;
  char shortname[11];
  char longname[41];
};


int palObs( size_t n, const char * c,
            char * ident, size_t identlen,
            char * name, size_t namelen,
            double * w, double * p, double * h ) {
  const struct telData telData[] = {
    /* AAT (Observer's Guide)                                            AAT */
    {
      EAST(149,3,57.91),
      SOUTH(31,16,37.34),
      1164E0,
      "AAT",
      "Anglo-Australian 3.9m Telescope"
    },
    /* WHT (Gemini, April 1987)                                       LPO4.2 */
    {
      WEST(17,52,53.9),
      NORTH(28,45,38.1),
      2332E0,
      "LPO4.2",
      "William Herschel 4.2m Telescope"
    },
    /* INT (Gemini, April 1987)                                       LPO2.5 */
    {
      WEST(17,52,39.5),
      NORTH(28,45,43.2),
      2336E0,
      "LPO2.5",
      "Isaac Newton 2.5m Telescope"
    },
    /* JKT (Gemini, April 1987)                                         LPO1 */
    {
      WEST(17,52,41.2),
      NORTH(28,45,39.9),
      2364E0,
      "LPO1",
      "Jacobus Kapteyn 1m Telescope"
    },
    /* Lick 120" (S.L.Allen, private communication, 2002)            LICK120 */
    {
      WEST(121,38,13.689),
      NORTH(37,20,34.931),
      1286E0,
      "LICK120",
      "Lick 120 inch"
    },
    /* MMT 6.5m conversion (MMT Observatory website)                     MMT */
    {
      WEST(110,53,4.4),
      NORTH(31,41,19.6),
      2608E0,
      "MMT",
      "MMT 6.5m, Mt Hopkins"
    },
    /* Victoria B.C. 1.85m (1984 Almanac)                              DAO72 */
    {
      WEST(123,25,1.18),
      NORTH(48,31,11.9),
      238E0,
      "DAO72",
      "DAO Victoria BC 1.85 metre"
    },
    /* Las Campanas (1983 Almanac)                                    DUPONT */
    {
      WEST(70,42,9.),
      SOUTH(29,0,11.),
      2280E0,
      "DUPONT",
      "Du Pont 2.5m Telescope, Las Campanas"
    },
    /* Mt Hopkins 1.5m (1983 Almanac)                               MTHOP1.5 */
    {
      WEST(110,52,39.00),
      NORTH(31,40,51.4),
      2344E0,
      "MTHOP1.5",
      "Mt Hopkins 1.5 metre"
    },
    /* Mt Stromlo 74" (1983 Almanac)                               STROMLO74 */
    {
      EAST(149,0,27.59),
      SOUTH(35,19,14.3),
      767E0,
      "STROMLO74",
      "Mount Stromlo 74 inch"
    },
    /* ANU 2.3m, SSO (Gary Hovey)                                     ANU2.3 */
    {
      EAST(149,3,40.3),
      SOUTH(31,16,24.1),
      1149E0,
      "ANU2.3",
      "Siding Spring 2.3 metre"
    },
    /* Greenbank 140' (1983 Almanac)                                 GBVA140 */
    {
      WEST(79,50,9.61),
      NORTH(38,26,15.4),
      881E0,
      "GBVA140",
      "Greenbank 140 foot"
    },
    /* Cerro Tololo 4m (1982 Almanac)                               TOLOLO4M */
    {
      WEST(70,48,53.6),
      SOUTH(30,9,57.8),
      2235E0,
      "TOLOLO4M",
      "Cerro Tololo 4 metre"
    },
    /* Cerro Tololo 1.5m (1982 Almanac)                           TOLOLO1.5M */
    {
      WEST(70,48,54.5),
      SOUTH(30,9,56.3),
      2225E0,
      "TOLOLO1.5M",
      "Cerro Tololo 1.5 metre"
    },
    /* Tidbinbilla 64m (1982 Almanac)                              TIDBINBLA */
    {
      EAST(148,58,48.20),
      SOUTH(35,24,14.3),
      670E0,
      "TIDBINBLA",
      "Tidbinbilla 64 metre"
    },
    /* Bloemfontein 1.52m (1981 Almanac)                              BLOEMF */
    {
      EAST(26,24,18.),
      SOUTH(29,2,18.),
      1387E0,
      "BLOEMF",
      "Bloemfontein 1.52 metre"
    },
    /* Bosque Alegre 1.54m (1981 Almanac)                         BOSQALEGRE */
    {
      WEST(64,32,48.0),
      SOUTH(31,35,53.),
      1250E0,
      "BOSQALEGRE",
      "Bosque Alegre 1.54 metre"
    },
    /* USNO 61" astrographic reflector, Flagstaff (1981 Almanac)   FLAGSTF61 */
    {
      WEST(111,44,23.6),
      NORTH(35,11,2.5),
      2316E0,
      "FLAGSTF61",
      "USNO 61 inch astrograph, Flagstaff"
    },
    /* Lowell 72" (1981 Almanac)                                    LOWELL72 */
    {
      WEST(111,32,9.3),
      NORTH(35,5,48.6),
      2198E0,
      "LOWELL72",
      "Perkins 72 inch, Lowell"
    },
    /* Harvard 1.55m (1981 Almanac)                                  HARVARD */
    {
      WEST(71,33,29.32),
      NORTH(42,30,19.0),
      185E0,
      "HARVARD",
      "Harvard College Observatory 1.55m"
    },
    /* Okayama 1.88m (1981 Almanac)                                  OKAYAMA */
    {
      EAST(133,35,47.29),
      NORTH(34,34,26.1),
      372E0,
      "OKAYAMA",
      "Okayama 1.88 metre"
    },
    /* Kitt Peak Mayall 4m (1981 Almanac)                            KPNO158 */
    {
      WEST(111,35,57.61),
      NORTH(31,57,50.3),
      2120E0,
      "KPNO158",
      "Kitt Peak 158 inch"
    },
    /* Kitt Peak 90 inch (1981 Almanac)                               KPNO90 */
    {
      WEST(111,35,58.24),
      NORTH(31,57,46.9),
      2071E0,
      "KPNO90",
      "Kitt Peak 90 inch"
    },
    /* Kitt Peak 84 inch (1981 Almanac)                               KPNO84 */
    {
      WEST(111,35,51.56),
      NORTH(31,57,29.2),
      2096E0,
      "KPNO84",
      "Kitt Peak 84 inch"
    },
    /* Kitt Peak 36 foot (1981 Almanac)                             KPNO36FT */
    {
      WEST(111,36,51.12),
      NORTH(31,57,12.1),
      1939E0,
      "KPNO36FT",
      "Kitt Peak 36 foot"
    },
    /* Kottamia 74" (1981 Almanac)                                  KOTTAMIA */
    {
      EAST(31,49,30.),
      NORTH(29,55,54.),
      476E0,
      "KOTTAMIA",
      "Kottamia 74 inch"
    },
    /* La Silla 3.6m (1981 Almanac)                                   ESO3.6 */
    {
      WEST(70,43,36.),
      SOUTH(29,15,36.),
      2428E0,
      "ESO3.6",
      "ESO 3.6 metre"
    },
    /* Mauna Kea 88 inch                                            MAUNAK88 */
    /* (IfA website, Richard Wainscoat) */
    {
      WEST(155,28,9.96),
      NORTH(19,49,22.77),
      4213.6E0,
      "MAUNAK88",
      "Mauna Kea 88 inch"
    },
    /* UKIRT (IfA website, Richard Wainscoat)                          UKIRT */
    {
      WEST(155,28,13.18),
      NORTH(19,49,20.75),
      4198.5E0,
      "UKIRT",
      "UK Infra Red Telescope"
    },
    /* Quebec 1.6m (1981 Almanac)                                  QUEBEC1.6 */
    {
      WEST(71,9,9.7),
      NORTH(45,27,20.6),
      1114E0,
      "QUEBEC1.6",
      "Quebec 1.6 metre"
    },
    /* Mt Ekar 1.82m (1981 Almanac)                                   MTEKAR */
    {
      EAST(11,34,15.),
      NORTH(45,50,48.),
      1365E0,
      "MTEKAR",
      "Mt Ekar 1.82 metre"
    },
    /* Mt Lemmon 60" (1981 Almanac)                               MTLEMMON60 */
    {
      WEST(110,42,16.9),
      NORTH(32,26,33.9),
      2790E0,
      "MTLEMMON60",
      "Mt Lemmon 60 inch"
    },
    /* Mt Locke 2.7m (1981 Almanac)                               MCDONLD2.7 */
    {
      WEST(104,1,17.60),
      NORTH(30,40,17.7),
      2075E0,
      "MCDONLD2.7",
      "McDonald 2.7 metre"
    },
    /* Mt Locke 2.1m (1981 Almanac)                               MCDONLD2.1 */
    {
      WEST(104,1,20.10),
      NORTH(30,40,17.7),
      2075E0,
      "MCDONLD2.1",
      "McDonald 2.1 metre"
    },
    /* Palomar 200" (1981 Almanac)                                PALOMAR200 */
    {
      WEST(116,51,50.),
      NORTH(33,21,22.),
      1706E0,
      "PALOMAR200",
      "Palomar 200 inch"
    },
    /* Palomar 60" (1981 Almanac)                                  PALOMAR60 */
    {
      WEST(116,51,31.),
      NORTH(33,20,56.),
      1706E0,
      "PALOMAR60",
      "Palomar 60 inch"
    },
    /* David Dunlap 74" (1981 Almanac)                              DUNLAP74 */
    {
      WEST(79,25,20.),
      NORTH(43,51,46.),
      244E0,
      "DUNLAP74",
      "David Dunlap 74 inch"
    },
    /* Haute Provence 1.93m (1981 Almanac)                         HPROV1.93 */
    {
      EAST(5,42,46.75),
      NORTH(43,55,53.3),
      665E0,
      "HPROV1.93",
      "Haute Provence 1.93 metre"
    },
    /* Haute Provence 1.52m (1981 Almanac)                         HPROV1.52 */
    {
      EAST(5,42,43.82),
      NORTH(43,56,0.2),
      667E0,
      "HPROV1.52",
      "Haute Provence 1.52 metre"
    },
    /* San Pedro Martir 83" (1981 Almanac)                           SANPM83 */
    {
      WEST(115,27,47.),
      NORTH(31,2,38.),
      2830E0,
      "SANPM83",
      "San Pedro Martir 83 inch"
    },
    /* Sutherland 74" (1981 Almanac)                                  SAAO74 */
    {
      EAST(20,48,44.3),
      SOUTH(32,22,43.4),
      1771E0,
      "SAAO74",
      "Sutherland 74 inch"
    },
    /* Tautenburg 2m (1981 Almanac)                                  TAUTNBG */
    {
      EAST(11,42,45.),
      NORTH(50,58,51.),
      331E0,
      "TAUTNBG",
      "Tautenburg 2 metre"
    },
    /* Catalina 61" (1981 Almanac)                                CATALINA61 */
    {
      WEST(110,43,55.1),
      NORTH(32,25,0.7),
      2510E0,
      "CATALINA61",
      "Catalina 61 inch"
    },
    /* Steward 90" (1981 Almanac)                                  STEWARD90 */
    {
      WEST(111,35,58.24),
      NORTH(31,57,46.9),
      2071E0,
      "STEWARD90",
      "Steward 90 inch"
    },
    /* Russian 6m (1981 Almanac)                                       USSR6 */
    {
      EAST(41,26,30.0),
      NORTH(43,39,12.),
      2100E0,
      "USSR6",
      "USSR 6 metre"
    },
    /* Arecibo 1000' (1981 Almanac)                                  ARECIBO */
    {
      WEST(66,45,11.1),
      NORTH(18,20,36.6),
      496E0,
      "ARECIBO",
      "Arecibo 1000 foot"
    },
    /* Cambridge 5km (1981 Almanac)                                  CAMB5KM */
    {
      EAST(0,2,37.23),
      NORTH(52,10,12.2),
      17E0,
      "CAMB5KM",
      "Cambridge 5km"
    },
    /* Cambridge 1 mile (1981 Almanac)                             CAMB1MILE */
    {
      EAST(0,2,21.64),
      NORTH(52,9,47.3),
      17E0,
      "CAMB1MILE",
      "Cambridge 1 mile"
    },
    /* Bonn 100m (1981 Almanac)                                   EFFELSBERG */
    {
      EAST(6,53,1.5),
      NORTH(50,31,28.6),
      366E0,
      "EFFELSBERG",
      "Effelsberg 100 metre"
    },
    /* Greenbank 300' (1981 Almanac)                        GBVA300 (R.I.P.) */
    {
      WEST(79,50,56.36),
      NORTH(38,25,46.3),
      894E0,
      "(R.I.P.)",
      "Greenbank 300 foot"
    },
    /* Jodrell Bank Mk 1 (1981 Almanac)                             JODRELL1 */
    {
      WEST(2,18,25.),
      NORTH(53,14,10.5),
      78E0,
      "JODRELL1",
      "Jodrell Bank 250 foot"
    },
    /* Australia Telescope Parkes Observatory                         PARKES */
    /* (Peter te Lintel Hekkert) */
    {
      EAST(148,15,44.3591),
      SOUTH(32,59,59.8657),
      391.79E0,
      "PARKES",
      "Parkes 64 metre"
    },
    /* VLA (1981 Almanac)                                                VLA */
    {
      WEST(107,37,3.82),
      NORTH(34,4,43.5),
      2124E0,
      "VLA",
      "Very Large Array"
    },
    /* Sugar Grove 150' (1981 Almanac)                            SUGARGROVE */
    {
      WEST(79,16,23.),
      NORTH(38,31,14.),
      705E0,
      "SUGARGROVE",
      "Sugar Grove 150 foot"
    },
    /* Russian 600' (1981 Almanac)                                   USSR600 */
    {
      EAST(41,35,25.5),
      NORTH(43,49,32.),
      973E0,
      "USSR600",
      "USSR 600 foot"
    },
    /* Nobeyama 45 metre mm dish (based on 1981 Almanac entry)      NOBEYAMA */
    {
      EAST(138,29,12.),
      NORTH(35,56,19.),
      1350E0,
      "NOBEYAMA",
      "Nobeyama 45 metre"
    },
    /* James Clerk Maxwell 15 metre mm telescope, Mauna Kea             JCMT */
    /* From GPS measurements on 11Apr2007 for eSMA setup (R. Tilanus) */
    {
      WEST(155,28,37.30),
      NORTH(19,49,22.22),
      4124.75E0,
      "JCMT",
      "JCMT 15 metre"
    },
    /* ESO 3.5 metre NTT, La Silla (K.Wirenstrand)                    ESONTT */
    {
      WEST(70,43,7.),
      SOUTH(29,15,30.),
      2377E0,
      "ESONTT",
      "ESO 3.5 metre NTT"
    },
    /* St Andrews University Observatory (1982 Almanac)           ST.ANDREWS */
    {
      WEST(2,48,52.5),
      NORTH(56,20,12.),
      30E0,
      "ST.ANDREWS",
      "St Andrews"
    },
    /* Apache Point 3.5 metre (R.Owen)                                APO3.5 */
    {
      WEST(105,49,11.56),
      NORTH(32,46,48.96),
      2809E0,
      "APO3.5",
      "Apache Point 3.5m"
    },
    /* W.M.Keck Observatory, Telescope 1                               KECK1 */
    /* (William Lupton) */
    {
      WEST(155,28,28.99),
      NORTH(19,49,33.41),
      4160E0,
      "KECK1",
      "Keck 10m Telescope #1"
    },
    /* Tautenberg Schmidt (1983 Almanac)                            TAUTSCHM */
    {
      EAST(11,42,45.0),
      NORTH(50,58,51.0),
      331E0,
      "TAUTSCHM",
      "Tautenberg 1.34 metre Schmidt"
    },
    /* Palomar Schmidt (1981 Almanac)                              PALOMAR48 */
    {
      WEST(116,51,32.0),
      NORTH(33,21,26.0),
      1706E0,
      "PALOMAR48",
      "Palomar 48-inch Schmidt"
    },
    /* UK Schmidt, Siding Spring (1983 Almanac)                         UKST */
    {
      EAST(149,4,12.8),
      SOUTH(31,16,27.8),
      1145E0,
      "UKST",
      "UK 1.2 metre Schmidt, Siding Spring"
    },
    /* Kiso Schmidt, Japan (1981 Almanac)                               KISO */
    {
      EAST(137,37,42.2),
      NORTH(35,47,38.7),
      1130E0,
      "KISO",
      "Kiso 1.05 metre Schmidt, Japan"
    },
    /* ESO Schmidt, La Silla (1981 Almanac)                          ESOSCHM */
    {
      WEST(70,43,46.5),
      SOUTH(29,15,25.8),
      2347E0,
      "ESOSCHM",
      "ESO 1 metre Schmidt, La Silla"
    },
    /* Australia Telescope Compact Array                                ATCA */
    /* (WGS84 coordinates of Station 35, Mark Calabretta) */
    {
      EAST(149,33,0.500),
      SOUTH(30,18,46.385),
      236.9E0,
      "ATCA",
      "Australia Telescope Compact Array"
    },
    /* Australia Telescope Mopra Observatory                           MOPRA */
    /* (Peter te Lintel Hekkert) */
    {
      EAST(149,5,58.732),
      SOUTH(31,16,4.451),
      850E0,
      "MOPRA",
      "ATNF Mopra Observatory"
    },
    /* Subaru telescope, Mauna Kea                                     SUBARU */
    /* (IfA website, Richard Wainscoat) */
    {
      WEST(155,28,33.67),
      NORTH(19,49,31.81),
      4163E0,
      "SUBARU",
      "Subaru 8m telescope"
    },
    /* Canada-France-Hawaii Telescope, Mauna Kea                         CFHT */
    /* (IfA website, Richard Wainscoat) */
    {
      WEST(155,28,7.95),
      NORTH(19,49,30.91),
      4204.1E0,
      "CFHT",
      "Canada-France-Hawaii 3.6m Telescope"
    },
    /* W.M.Keck Observatory, Telescope 2                                KECK2 */
    /* (William Lupton) */
    {
      WEST(155,28,27.24),
      NORTH(19,49,35.62),
      4159.6E0,
      "KECK2",
      "Keck 10m Telescope #2"
    },
    /* Gemini North, Mauna Kea                                        GEMININ */
    /* (IfA website, Richard Wainscoat) */
    {
      WEST(155,28,8.57),
      NORTH(19,49,25.69),
      4213.4E0,
      "GEMININ",
      "Gemini North 8-m telescope"
    },
    /* Five College Radio Astronomy Observatory                        FCRAO */
    /* (Tim Jenness) */
    {
      WEST(72,20,42.0),
      NORTH(42,23,30.0),
      314E0,
      "FCRAO",
      "Five College Radio Astronomy Obs"
    },
    /* NASA Infra Red Telescope Facility                                IRTF */
    /* (IfA website, Richard Wainscoat) */
    {
      WEST(155,28,19.20),
      NORTH(19,49,34.39),
      4168.1E0,
      "IRTF",
      "NASA IR Telescope Facility, Mauna Kea"
    },
    /* Caltech Submillimeter Observatory                                 CSO */
    /* (IfA website, Richard Wainscoat; height estimated) */
    {
      WEST(155,28,31.79),
      NORTH(19,49,20.78),
      4080E0,
      "CSO",
      "Caltech Sub-mm Observatory, Mauna Kea"
    },
    /* ESO VLT, UT1                                                       VLT1 */
    /* (ESO website, VLT Whitebook Chapter 2) */
    {
      WEST(70,24,11.642),
      SOUTH(24,37,33.117),
      2635.43,
      "VLT1",
      "ESO VLT, Paranal, Chile: UT1"
    },
    /* ESO VLT, UT2                                                       VLT2 */
    /* (ESO website, VLT Whitebook Chapter 2) */
    {
      WEST(70,24,10.855),
      SOUTH(24,37,31.465),
      2635.43,
      "VLT2",
      "ESO VLT, Paranal, Chile: UT2"
    },
    /* ESO VLT, UT3                                                       VLT3 */
    /* (ESO website, VLT Whitebook Chapter 2) */
    {
      WEST(70,24,9.896),
      SOUTH(24,37,30.300),
      2635.43,
      "VLT3",
      "ESO VLT, Paranal, Chile: UT3"
    },
    /* ESO VLT, UT4                                                       VLT4 */
    /* (ESO website, VLT Whitebook Chapter 2) */
    {
      WEST(70,24,8.000),
      SOUTH(24,37,31.000),
      2635.43,
      "VLT4",
      "ESO VLT, Paranal, Chile: UT4"
    },
    /* Gemini South, Cerro Pachon                                     GEMINIS */
    /* (GPS readings by Patrick Wallace) */
    {
      WEST(70,44,11.5),
      SOUTH(30,14,26.7),
      2738E0,
      "GEMINIS",
      "Gemini South 8-m telescope"
    },
    /* Cologne Observatory for Submillimeter Astronomy (KOSMA)        KOSMA3M */
    /* (Holger Jakob) */
    {
      EAST(7,47,3.48),
      NORTH(45,58,59.772),
      3141E0,
      "KOSMA3M",
      "KOSMA 3m telescope, Gornergrat"
    },
    /* Magellan 1, 6.5m telescope at Las Campanas, Chile            MAGELLAN1 */
    /* (Skip Schaller) */
    {
      WEST(70,41,31.9),
      SOUTH(29,0,51.7),
      2408E0,
      "MAGELLAN1",
      "Magellan 1, 6.5m, Las Campanas"
    },
    /* Magellan 2, 6.5m telescope at Las Campanas, Chile            MAGELLAN2 */
    /* (Skip Schaller) */
    {
      WEST(70,41,33.5),
      SOUTH(29,0,50.3),
      2408E0,
      "MAGELLAN2",
      "Magellan 2, 6.5m, Las Campanas"
    }
  };

  int retval = -1;    /* Return status. 0 if found. -1 if no match */

  /* Work out the number of telescopes */
  const size_t NTEL = sizeof(telData) / sizeof(struct telData);

  if (n > 0) {
    if (n > NTEL) {
      /* Out of bounds */
      star_strellcpy( name, "?", namelen );
      retval = -1; /* just to be sure */
    } else {
      /* Index into telData with correction for zero-based indexing */
      struct telData thistel;
      thistel = telData[n-1];
      *w = thistel.w;
      *p = thistel.p;
      *h = thistel.h;
      star_strellcpy( ident, thistel.shortname, identlen );
      star_strellcpy( name, thistel.longname, namelen );
      retval = 0;
    }

  } else {
    /* Searching */
    int i;
    for (i=0; i<NTEL; i++) {
      struct telData thistel = telData[i];
      if (strcmp( c, thistel.shortname) == 0) {
        /* a match */
        *w = thistel.w;
        *p = thistel.p;
        *h = thistel.h;
        star_strellcpy( ident, thistel.shortname, identlen );
        star_strellcpy( name, thistel.longname, namelen );
        retval = 0;
        break;
      }
    }

  }

  return retval;

}
