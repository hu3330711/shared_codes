/* SDTSpacecraft.h */
/* This file contains declarations for SDT SpaceCraft: */

#ifndef SDTSPACECRAFT_H
#define SDTSPACECRAFT_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* SCCS ID string: */
#define SccsId_SDTSpacecraft_h "@(#)SDTSpacecraft.h	1.17, 09/15/06"

#include <SDTType.h>




/*
 *     These are the spacecraft Ids.
 *
 *     I M P O R T A N T ! ! !
 *     -----------------
 *
 *     Whenever this list is changed, the following must be changed
 *     accordingly:
 *
 *      1) The number of spacecraft Ids below.
 *
 *      2) The array of special information for the spacecraft Ids
 *         below.
 *
 *      3) The list of spacecraft Id strings in the file
 *             SDTSpacecraft.c
 *         and the appropriate functions in that file.
 *
 *      4) The list of split SCM.xxxx.dat files in the Makefile at
 *             ws/src/sdt/FastSnapOns.
 */

#define  ALL_SPACECRAFT                    0
#define  CRRES_SPACECRAFT                  1
#define  ISEE_SPACECRAFT                   2
#define  ISEE2_SPACECRAFT                  3
#define  GEOTAIL_SPACECRAFT                24
#define  WIND_SPACECRAFT                   25
#define  POLAR_SPACECRAFT                  26
#define  CLUSTER_SPACECRAFT                30
#define  THEMIS_SPACECRAFT                 40
#define  GEOTAIL_SURVEY_SPACECRAFT         241
#define  CRRES_SURVEY_SPACECRAFT           1001
#define  FAST_REALTIME_SPACECRAFT          2001




/*
 *     These are the defaults for spacecraft Id.
 *
 *     The set labeled "DEF" has default vales that could correspond to
 *     actual data.
 *
 *     The set labeled "NONE" has default vales that could not
 *     correspond to actual data.
 */

#define  DEFAULT_S_C_ID_DEF                ALL_SPACECRAFT
#define  DEFAULT_S_C_SUB_ID_DEF            1

#define  DEFAULT_S_C_ID_NONE               -1
#define  DEFAULT_S_C_SUB_ID_NONE           -1




/*
 *     These are the defaults for source Id.
 *
 *     The set labeled "DEF" has default vales that could correspond to
 *     actual data.
 *
 *     The set labeled "NONE" has default vales that could not
 *     correspond to actual data.
 */

#define  DEFAULT_SOURCE_ID_DEF             ALL_SPACECRAFT

#define  DEFAULT_SOURCE_ID_NONE            -1




/*
 *     Number of spacecraft Ids.
 *
 *     Must agree with the spacecraft Ids defined above.
 */

#define  N_IDS                 (12)




/*
 *     The structure that follows contains special information for one
 *     spacecraft Id.
 */


   struct SDT_SC_Id_Info_struct


     {


/*
 *     Spacecraft Id.
 */

       int SC_Id ;


/*
 *     Number of sub-spacecrafts for the spacecraft ID (or "spacecraft",
 *     or "mission", or "project").
 */

       int n_sub_SC ;


/*
 *     Name of a "configuration sub-directory".
 *
 *     This is a sub-directory of $FASTCONFIG or of $FASTLIB (either
 *     directly or indirectly), where configuration files or other files
 *     for the spacecraft may be stored.
 *
 *     In practice, for some of the spacecraft Id this subdirectory may
 *     not exist.
 */

       const char *SC_cfg_sub_dir ;


/*
 *     Character string that corresponds to how the spacecraft is
 *     identified within SCM.Data.dat files.
 */

       const char *SCM_Data_name ;


/*
 *     Character string that is the suffix for split SCM.xxxx.dat files.
 */

       const char *SCM_file_suffix ;


     } ;


   typedef
       struct SDT_SC_Id_Info_struct
           SDT_SC_Id_Info ;




/*
 *     The array that follows contains special information for the
 *     spacecraft Ids.
 *
 *     There should be N_IDS array elements.
 */

   static SDT_SC_Id_Info
       SDT_SC_Id_Info_val[] =

     {

       {
         ((int) ALL_SPACECRAFT),
         0,
         ((const char *) "general"),
         ((const char *) "ALL_SPACECRAFT"),
         ((const char *) "GENERAL"),
       } ,

       {
         ((int) CRRES_SPACECRAFT),
         1,
         ((const char *) "crres"),
         ((const char *) "CRRES_SPACECRAFT"),
         ((const char *) "CRRES"),
       } ,

       {
         ((int) ISEE_SPACECRAFT),
         1,
         ((const char *) "isee"),
         ((const char *) "ISEE_SPACECRAFT"),
         ((const char *) "ISEE"),
       } ,

       {
         ((int) ISEE2_SPACECRAFT),
         1,
         ((const char *) "isee2"),
         ((const char *) "ISEE2_SPACECRAFT"),
         ((const char *) "ISEE2"),
       } ,

       {
         ((int) GEOTAIL_SPACECRAFT),
         1,
         ((const char *) "geotail"),
         ((const char *) "GEOTAIL_SPACECRAFT"),
         ((const char *) "GEOTAIL"),
       } ,

       {
         ((int) WIND_SPACECRAFT),
         1,
         ((const char *) "wind"),
         ((const char *) "WIND_SPACECRAFT"),
         ((const char *) "WIND"),
       } ,

       {
         ((int) POLAR_SPACECRAFT),
         1,
         ((const char *) "polar"),
         ((const char *) "POLAR_SPACECRAFT"),
         ((const char *) "POLAR"),
       } ,

       {
         ((int) CLUSTER_SPACECRAFT),
         4,
         ((const char *) "cluster"),
         ((const char *) "CLUSTER_SPACECRAFT"),
         ((const char *) "CLUSTER"),
       } ,

       {
         ((int) THEMIS_SPACECRAFT),
         5,
         ((const char *) "themis"),
         ((const char *) "THEMIS_SPACECRAFT"),
         ((const char *) "THEMIS"),
       } ,

       {
         ((int) GEOTAIL_SURVEY_SPACECRAFT),
         1,
         ((const char *) "geo_srvmin"),
         ((const char *) "GEOTAIL_SURVEY_SPACECRAFT"),
         ((const char *) "GEOTAIL_SURVEY"),
       } ,

       {
         ((int) CRRES_SURVEY_SPACECRAFT),
         1,
         ((const char *) "crres_survey"),
         ((const char *) "CRRES_SURVEY_SPACECRAFT"),
         ((const char *) "CRRES_SURVEY"),
       } ,

       {
         ((int) FAST_REALTIME_SPACECRAFT),
         1,
         ((const char *) "fast"),
         ((const char *) "FAST_SPACECRAFT"),
         ((const char *) "FAST"),
       } ,

     } ;




/* These are the CRRES instrument ids */
#define   CRR_SPHERE_ID_1           1
#define   CRR_SPHERE_ID_2           2
#define   CRR_CYLINDER_ID_3         3
#define   CRR_CYLINDER_ID_4         4
#define   CRR_POSITIVE_SPIN_AXIS    5
#define   CRR_SPACECRAFT_X          10
#define   CRR_SPACECRAFT_Y          11
#define   CRR_SPACECRAFT_Z          12

/* These are the ISEE instrument ids */
/* These are the GEOTAIL instrument ids */
/* These are the POLAR instrument ids */
/* These are the CLUSTER instrument ids */
/* These are the THEMIS instrument ids */
/* These are the FAST instrument ids */

/* These are the SpaceCraft Routines */
char *SDTReturnSpaceCraftName( int16 SpaceCraftId) ;
char *SDTReturnSpaceCraftNamePtr( int16 SpaceCraftId) ;
int16 SDTReturnSpaceCraftId( char *SpaceCraftName) ;
int SDTReturnSpaceCraftIndex( int16 SpaceCraftId) ;


/**********************************************************************/

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTSPACECRAFT_H */

