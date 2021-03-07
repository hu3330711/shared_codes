/*
 ***********************************************************************
 *
 *     wt_ALL_S_C_001.h
 *
 *     Author: Winston Teitler.
 *
 ***********************************************************************
 *
 *     C header for the ALL SPACECRAFT library.
 *
 ***********************************************************************
 *
 *     @(#)wt_ALL_S_C_001.h	1.6    04/23/07    UCB SSL
 *
 ***********************************************************************
 */





/*
 *======================================================================
 *======================================================================
 */





#ifndef  WT_ALL_S_C_001_H
#define  WT_ALL_S_C_001_H





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Headers.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     General purpose C header.
 *
 *----------------------------------------------------------------------
 */



#include <wtgen001.h>



/*
 *----------------------------------------------------------------------
 *
 *     Spacecraft.
 *
 *----------------------------------------------------------------------
 */



#include <wt_CLUSTER_001.h>

#include <wt_FAST_001.h>

#include <wt_POLAR_001.h>

#include <wt_THEMIS_001.h>





/*
 *======================================================================
 *======================================================================
 */





#ifdef  __cplusplus
   extern  "C"
   {
#endif





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Constants.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     Spacecraft identification.
 *
 *----------------------------------------------------------------------
 *
 *     Each mission is identified by a unique "Mission Id".
 *
 *     The actual values for "Mission Id" are chosen to agree with the
 *     values of the "Spacecraft Ids" defined in the SDT library header
 *     file SDTSpacecraft.h.
 *
 *     Within each mission, each spacecraft is identified by a unique
 *     "Mission Sub-Id" (unique within the mission).
 *
 *
 *
 *     Alternatively, each spacecraft is identified by a unique
 *     "Spacecraft Id".
 *
 *----------------------------------------------------------------------
 */



/* Mission Id is CLUSTER */
#define  WT_MISSION_ID_CLUSTER (30)

/* Mission Id is FAST */
#define  WT_MISSION_ID_FAST    (2001)

/* Mission Id is POLAR */
#define  WT_MISSION_ID_POLAR   (26)

/* Mission Id is THEMIS */
#define  WT_MISSION_ID_THEMIS  (40)



/* Mission Sub-Id is CLUSTER_SC1 */
#define  WT_MISSION_SUB_ID_CLUSTER_SC1 (1)

/* Mission Sub-Id is CLUSTER_SC2 */
#define  WT_MISSION_SUB_ID_CLUSTER_SC2                                 \
                               (WT_MISSION_SUB_ID_CLUSTER_SC1 + 1)

/* Mission Sub-Id is CLUSTER_SC3 */
#define  WT_MISSION_SUB_ID_CLUSTER_SC3                                 \
                               (WT_MISSION_SUB_ID_CLUSTER_SC1 + 2)

/* Mission Sub-Id is CLUSTER_SC4 */
#define  WT_MISSION_SUB_ID_CLUSTER_SC4                                 \
                               (WT_MISSION_SUB_ID_CLUSTER_SC1 + 3)


/* Mission Sub-Id is FAST */
#define  WT_MISSION_SUB_ID_FAST        (0)


/* Mission Sub-Id is POLAR */
#define  WT_MISSION_SUB_ID_POLAR       (0)



/* Mission Sub-Id is THEMIS_SC1 */
#define  WT_MISSION_SUB_ID_THEMIS_SC1  (1)

/* Mission Sub-Id is THEMIS_SC2 */
#define  WT_MISSION_SUB_ID_THEMIS_SC2                                  \
                               (WT_MISSION_SUB_ID_THEMIS_SC1 + 1)

/* Mission Sub-Id is THEMIS_SC3 */
#define  WT_MISSION_SUB_ID_THEMIS_SC3                                  \
                               (WT_MISSION_SUB_ID_THEMIS_SC1 + 2)

/* Mission Sub-Id is THEMIS_SC4 */
#define  WT_MISSION_SUB_ID_THEMIS_SC4                                  \
                               (WT_MISSION_SUB_ID_THEMIS_SC1 + 3)

/* Mission Sub-Id is THEMIS_SC5 */
#define  WT_MISSION_SUB_ID_THEMIS_SC5                                  \
                               (WT_MISSION_SUB_ID_THEMIS_SC1 + 4)



/* Spacecraft Id is CLUSTER_SC1 */
#define  WT_SPACECRAFT_ID_CLUSTER_SC1                                  \
                               (WT_MISSION_ID_CLUSTER +                \
                                WT_MISSION_SUB_ID_CLUSTER_SC1)

/* Spacecraft Id is CLUSTER_SC2 */
#define  WT_SPACECRAFT_ID_CLUSTER_SC2                                  \
                               (WT_MISSION_ID_CLUSTER +                \
                                WT_MISSION_SUB_ID_CLUSTER_SC2)

/* Spacecraft Id is CLUSTER_SC3 */
#define  WT_SPACECRAFT_ID_CLUSTER_SC3                                  \
                               (WT_MISSION_ID_CLUSTER +                \
                                WT_MISSION_SUB_ID_CLUSTER_SC3)

/* Spacecraft Id is CLUSTER_SC4 */
#define  WT_SPACECRAFT_ID_CLUSTER_SC4                                  \
                               (WT_MISSION_ID_CLUSTER +                \
                                WT_MISSION_SUB_ID_CLUSTER_SC4)


/* Spacecraft Id is FAST */
#define  WT_SPACECRAFT_ID_FAST                                         \
                               (WT_MISSION_ID_FAST +                   \
                                WT_MISSION_SUB_ID_FAST)


/* Spacecraft Id is POLAR */
#define  WT_SPACECRAFT_ID_POLAR                                        \
                               (WT_MISSION_ID_POLAR +                  \
                                WT_MISSION_SUB_ID_POLAR)



/* Spacecraft Id is THEMIS_SC1 */
#define  WT_SPACECRAFT_ID_THEMIS_SC1                                   \
                               (WT_MISSION_ID_THEMIS +                 \
                                WT_MISSION_SUB_ID_THEMIS_SC1)

/* Spacecraft Id is THEMIS_SC2 */
#define  WT_SPACECRAFT_ID_THEMIS_SC2                                   \
                               (WT_MISSION_ID_THEMIS +                 \
                                WT_MISSION_SUB_ID_THEMIS_SC2)

/* Spacecraft Id is THEMIS_SC3 */
#define  WT_SPACECRAFT_ID_THEMIS_SC3                                   \
                               (WT_MISSION_ID_THEMIS +                 \
                                WT_MISSION_SUB_ID_THEMIS_SC3)

/* Spacecraft Id is THEMIS_SC4 */
#define  WT_SPACECRAFT_ID_THEMIS_SC4                                   \
                               (WT_MISSION_ID_THEMIS +                 \
                                WT_MISSION_SUB_ID_THEMIS_SC4)

/* Spacecraft Id is THEMIS_SC5 */
#define  WT_SPACECRAFT_ID_THEMIS_SC5                                   \
                               (WT_MISSION_ID_THEMIS +                 \
                                WT_MISSION_SUB_ID_THEMIS_SC5)





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Type definitions.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     Spacecraft identification.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Full spacecraft identification.
 */


   struct WT_T_Full_S_C_Id_struct


     {


       int mission_id ;        /* Mission Id */

       int mission_sub_id ;    /* Mission Sub-Id */

       int s_c_id ;            /* S/C Id */


     } ;


   typedef
       struct WT_T_Full_S_C_Id_struct
           WT_T_Full_S_C_Id ;





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Arrays.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     Spacecraft identification.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Full identification for all S/C.
 */


   static WT_T_Full_S_C_Id WT_A_full_s_c_id[] =


     {


       { WT_MISSION_ID_CLUSTER,
         WT_MISSION_SUB_ID_CLUSTER_SC1,
         WT_SPACECRAFT_ID_CLUSTER_SC1 },

       { WT_MISSION_ID_CLUSTER,
         WT_MISSION_SUB_ID_CLUSTER_SC2,
         WT_SPACECRAFT_ID_CLUSTER_SC2 },

       { WT_MISSION_ID_CLUSTER,
         WT_MISSION_SUB_ID_CLUSTER_SC3,
         WT_SPACECRAFT_ID_CLUSTER_SC3 },

       { WT_MISSION_ID_CLUSTER,
         WT_MISSION_SUB_ID_CLUSTER_SC4,
         WT_SPACECRAFT_ID_CLUSTER_SC4 },


       { WT_MISSION_ID_FAST,
         WT_MISSION_SUB_ID_FAST,
         WT_SPACECRAFT_ID_FAST },


       { WT_MISSION_ID_POLAR,
         WT_MISSION_SUB_ID_POLAR,
         WT_SPACECRAFT_ID_POLAR },


       { WT_MISSION_ID_THEMIS,
         WT_MISSION_SUB_ID_THEMIS_SC1,
         WT_SPACECRAFT_ID_THEMIS_SC1 },

       { WT_MISSION_ID_THEMIS,
         WT_MISSION_SUB_ID_THEMIS_SC2,
         WT_SPACECRAFT_ID_THEMIS_SC2 },

       { WT_MISSION_ID_THEMIS,
         WT_MISSION_SUB_ID_THEMIS_SC3,
         WT_SPACECRAFT_ID_THEMIS_SC3 },

       { WT_MISSION_ID_THEMIS,
         WT_MISSION_SUB_ID_THEMIS_SC4,
         WT_SPACECRAFT_ID_THEMIS_SC4 },

       { WT_MISSION_ID_THEMIS,
         WT_MISSION_SUB_ID_THEMIS_SC5,
         WT_SPACECRAFT_ID_THEMIS_SC5 },


     } ;



/* Number of S/C */
#define  WT_N_S_C              ((int)                                  \
                                ((sizeof WT_A_full_s_c_id) /           \
                                 (sizeof WT_A_full_s_c_id[0])))





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Function prototypes.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     "Computed" files.
 *
 *----------------------------------------------------------------------
 */



   int
       ALL_S_C_Get_Comp_File_Name_1(
           const WT_T_Full_S_C_Id *id_ptr, int type,
           char **name1_ptr, int *n_double_ptr,
           int *n_float_ptr) ;


   int
       Computed_Files_Defined(const WT_T_Full_S_C_Id *id_ptr,
           int *o_flag_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Spacecraft identification.
 *
 *----------------------------------------------------------------------
 */



   int
       Full_S_C_Id_From_Mission(int mission_id,
           int mission_sub_id,
           WT_T_Full_S_C_Id *p) ;


   int
       Full_S_C_Id_From_S_C_Id(int s_c_id,
           WT_T_Full_S_C_Id *p) ;



/*
 *----------------------------------------------------------------------
 *
 *     Spacecraft rotation.
 *
 *----------------------------------------------------------------------
 */



   int
       ALL_S_C_Spin_Pulse_Parm(const WT_T_Full_S_C_Id *id_ptr,
           long int jdn, double time,
           WT_T_Spin_Pulse_Parm **parm_ptr_ptr) ;


   int
       Spin_Pulse_Parm_Defined(const WT_T_Full_S_C_Id *id_ptr,
           int *o_flag_ptr) ;





/*
 *======================================================================
 *======================================================================
 */





#ifdef  __cplusplus
   }
#endif





/*
 *======================================================================
 *======================================================================
 */





#endif   /* WT_ALL_S_C_001_H */
