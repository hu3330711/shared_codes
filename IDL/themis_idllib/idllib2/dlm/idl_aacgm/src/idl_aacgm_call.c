/* idl_aacgm_call.c
   =================
   Author: H. Korth
*/


/* 
 Copyright © 2004 The Johns Hopkins University/Applied Physics Laboratory.
 All rights reserved.
 
 This material may be used, modified, or reproduced by or for the U.S.
 Government pursuant to the license rights granted under the clauses at DFARS
 252.227-7013/7014.
 
 For any other permissions, please contact the Space Department
 Program Office at JHU/APL.
 
 This Distribution and Disclaimer Statement must be included in all copies of
 idl_aacgm (hereinafter "the Program").
 
 The Program was developed at The Johns Hopkins University/Applied Physics
 Laboratory (JHU/APL) which is the author thereof under the "work made for
 hire" provisions of the copyright law.  
 
 JHU/APL assumes no obligation to provide support of any kind with regard to
 the Program.  This includes no obligation to provide assistance in using the
 Program or to provide updated versions of the Program.
 
 THE PROGRAM AND ITS DOCUMENTATION ARE PROVIDED AS IS AND WITHOUT ANY EXPRESS
 OR IMPLIED WARRANTIES WHATSOEVER.  ALL WARRANTIES INCLUDING, BUT NOT LIMITED
 TO, PERFORMANCE, MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE ARE
 HEREBY DISCLAIMED.  YOU ASSUME THE ENTIRE RISK AND LIABILITY OF USING THE
 PROGRAM TO INCLUDE USE IN COMPLIANCE WITH ANY THIRD PARTY RIGHTS.  YOU ARE
 ADVISED TO TEST THE PROGRAM THOROUGHLY BEFORE RELYING ON IT.  IN NO EVENT
 SHALL JHU/APL BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING, WITHOUT
 LIMITATION, ANY LOST PROFITS, LOST SAVINGS OR OTHER INCIDENTAL OR
 CONSEQUENTIAL DAMAGES, ARISING OUT OF THE USE OR INABILITY TO USE THE
 PROGRAM."
 
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <idl_export.h>
#include "idl_aacgm.h"
#include "mlt.h"
#include "convert_geo_coord.h"
#include "aacgm_lib.h"

/*globals here*/
static char statusBuffer[256] ;

/* function protos */
extern IDL_VPTR IDL_CDECL aacgm_mlt(int argc, IDL_VPTR argv[], char *argk);
extern IDL_VPTR IDL_CDECL aacgm_mlong(int argc, IDL_VPTR argv[], char *argk);
extern void IDL_CDECL aacgm_conv_coord(int argc, IDL_VPTR argv[], char *argk);
extern void IDL_CDECL aacgm_load_coef(int argc, IDL_VPTR argv[], char *argk);

static IDL_SYSFUN_DEF2 idl_aacgm_functions[] = {
	{aacgm_mlt, "AACGM_MLT", 3, 3, IDL_SYSFUN_DEF_F_KEYWORDS, 0},
	{aacgm_mlong, "AACGM_MLONG", 3, 3, 0, 0},
};

static IDL_SYSFUN_DEF2 idl_aacgm_procedures[] = {
	{(IDL_FUN_RET) aacgm_conv_coord, "AACGM_CONV_COORD", 6, 6, IDL_SYSFUN_DEF_F_KEYWORDS, 0},
	{(IDL_FUN_RET) aacgm_load_coef, "AACGM_LOAD_COEF", 1, 1, 0, 0},
};

/* Check which system we are one */
#if defined(WIN32) 

#include <windows.h>

#endif

/* startup call when DLM is loaded */
int idl_aacgm_startup(void)
{
    if (!IDL_SysRtnAdd(idl_aacgm_functions, TRUE, ARRLEN(idl_aacgm_functions))) {
                return IDL_FALSE;
        }
	
	if (!IDL_SysRtnAdd(idl_aacgm_procedures, FALSE, ARRLEN(idl_aacgm_procedures))) {
                return IDL_FALSE;
        }

        return(IDL_TRUE);
}

/*{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|*/

IDL_VPTR IDL_CDECL aacgm_mlt(int argc, IDL_VPTR argv[], char *argk)
{
	typedef struct {
		IDL_KW_RESULT_FIRST_FIELD;
		int mslong_set;
		IDL_VPTR mslong_out;
	} KW_RESULT;

	static IDL_KW_PAR kw_pars[] = { IDL_KW_FAST_SCAN,
		{"MSLONG",IDL_TYP_UNDEF,1,IDL_KW_OUT|IDL_KW_ZERO,IDL_KW_OFFSETOF(mslong_set),IDL_KW_OFFSETOF(mslong_out)},
		{NULL}
        };

	KW_RESULT kw;
	int tyr, tt0, i;
	double tmlong, tmlt_out, tmslong;
	int *pyr, *pt0;
	double *pmlong, *pmlt_out, *pmslong;
	IDL_VPTR yr, t0, mlong;
	IDL_VPTR mlt_out, mslong;
	IDL_MEMINT n_yr, n_t0, n_mlong;

	IDL_KWProcessByOffset(argc,argv,argk,kw_pars,0,1,&kw);

	yr=IDL_BasicTypeConversion(1,&argv[0],IDL_TYP_LONG);
	IDL_VarGetData(yr, &n_yr,(char **) &pyr, FALSE);
	t0=IDL_BasicTypeConversion(1,&argv[1],IDL_TYP_LONG);
	IDL_VarGetData(t0, &n_t0,(char **) &pt0, FALSE);
	mlong=IDL_BasicTypeConversion(1,&argv[2],IDL_TYP_DOUBLE);
	IDL_VarGetData(mlong, &n_mlong,(char **) &pmlong, FALSE);

	if ((n_yr != n_t0) || (n_yr != n_mlong))
		IDL_Message(IDL_M_NAMED_GENERIC,IDL_MSG_LONGJMP,
		"Array dimensions differ.");

    pmlt_out=(double *) IDL_MakeTempVector(IDL_TYP_DOUBLE,
		n_yr, IDL_ARR_INI_ZERO, &mlt_out);	
    pmslong=(double *) IDL_MakeTempVector(IDL_TYP_DOUBLE,
		n_yr, IDL_ARR_INI_ZERO, &mslong);	

	for (i=0;i<n_yr;i++) {
		tyr=pyr[i];
		tt0=pt0[i];
		tmlong=pmlong[i];

		if ((tmlong < -180.0) || (tmlong > 360.0))
		IDL_Message(IDL_M_NAMED_GENERIC,IDL_MSG_LONGJMP,
		"Valid longitude range is -180 < MLONG < 360.");

  		tmlt_out=mlt(tyr,tt0,tmlong,&tmslong);

		pmlt_out[i]=tmlt_out;
		pmslong[i]=tmslong;
	}

	if (kw.mslong_set) IDL_VarCopy(mslong,kw.mslong_out);

	if (yr != argv[0]) IDL_Deltmp(yr);
	if (t0 != argv[1]) IDL_Deltmp(t0);
	if (mlong != argv[2]) IDL_Deltmp(mlong);
	IDL_Deltmp(mslong);

	IDL_KW_FREE;
	return(mlt_out);
}

/*{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|*/

IDL_VPTR IDL_CDECL aacgm_mlong(int argc, IDL_VPTR argv[], char *argk)
{
	int tyr, tt0, i;
	double tmlt, tmlt0, tmslong, tmlong_out;
	int *pyr, *pt0;
	double *pmlt, *pmlong_out;
	IDL_VPTR yr, t0, mlta;
	IDL_VPTR mlong_out;
	IDL_MEMINT n_yr, n_t0, n_mlt;

	yr=IDL_BasicTypeConversion(1,&argv[0],IDL_TYP_LONG);
	IDL_VarGetData(yr, &n_yr,(char **) &pyr, FALSE);
	t0=IDL_BasicTypeConversion(1,&argv[1],IDL_TYP_LONG);
	IDL_VarGetData(t0, &n_t0,(char **) &pt0, FALSE);
	mlta=IDL_BasicTypeConversion(1,&argv[2],IDL_TYP_DOUBLE);
	IDL_VarGetData(mlta, &n_mlt,(char **) &pmlt, FALSE);

	if ((n_yr != n_t0) || (n_yr != n_mlt))
		IDL_Message(IDL_M_NAMED_GENERIC,IDL_MSG_LONGJMP,
		"Array dimensions differ.");

    pmlong_out=(double *) IDL_MakeTempVector(IDL_TYP_DOUBLE,
		n_yr, IDL_ARR_INI_ZERO, &mlong_out);	

	for (i=0;i<n_yr;i++) {
		tyr=pyr[i];
		tt0=pt0[i];
		tmlt=pmlt[i];

		if ((tmlt < 0.0) || (tmlt > 24.0))
		IDL_Message(IDL_M_NAMED_GENERIC,IDL_MSG_LONGJMP,
		"Valid MLT range is 0 < MLT < 24.");

        tmlt0=mlt(tyr,tt0,0.0,&tmslong);
  		tmlong_out=(tmlt-tmlt0)*360.0/24.0;
		if (tmlong_out < 0) tmlong_out=tmlong_out+360.0;

		pmlong_out[i]=tmlong_out;
	}

	if (yr != argv[0]) IDL_Deltmp(yr);
	if (t0 != argv[1]) IDL_Deltmp(t0);
	if (mlta != argv[2]) IDL_Deltmp(mlta);

	return(mlong_out);
}

/*{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|*/

void IDL_CDECL aacgm_conv_coord(int argc, IDL_VPTR argv[], char *argk)
{
	typedef struct {
		IDL_KW_RESULT_FIRST_FIELD;
		int order;
		int to_aacgm;
		int to_geo;
	} KW_RESULT;

	static IDL_KW_PAR kw_pars[] = { IDL_KW_FAST_SCAN,
		{"ORDER",IDL_TYP_LONG,1,IDL_KW_ZERO,0,IDL_KW_OFFSETOF(order)},
		{"TO_AACGM",IDL_TYP_LONG,1,IDL_KW_ZERO,0,IDL_KW_OFFSETOF(to_aacgm)},
		{"TO_GEO",IDL_TYP_LONG,1,IDL_KW_ZERO,0,IDL_KW_OFFSETOF(to_geo)},
		{NULL}
        };

	KW_RESULT kw;
	int flag,i,terr,order;
	int *perr;
	double tlat_in,tlon_in,theight_in;
	double tlat_out,tlon_out;
	double *plat_in,*plon_in,*pheight_in;
	double *plat_out,*plon_out;
	IDL_VPTR lat_in,lon_in,height_in;
	IDL_VPTR lat_out,lon_out,err;
	IDL_MEMINT n_lat_in,n_lon_in,n_height_in;

	IDL_KWProcessByOffset(argc,argv,argk,kw_pars,0,1,&kw);

	lat_in=IDL_BasicTypeConversion(1,&argv[0],IDL_TYP_DOUBLE);
	IDL_VarGetData(lat_in, &n_lat_in,(char **) &plat_in, FALSE);
	lon_in=IDL_BasicTypeConversion(1,&argv[1],IDL_TYP_DOUBLE);
	IDL_VarGetData(lon_in, &n_lon_in,(char **) &plon_in, FALSE);
	height_in=IDL_BasicTypeConversion(1,&argv[2],IDL_TYP_DOUBLE);
	IDL_VarGetData(height_in, &n_height_in,(char **) &pheight_in, FALSE);

    IDL_StoreScalarZero(argv[3], IDL_TYP_DOUBLE);
    IDL_StoreScalarZero(argv[4], IDL_TYP_DOUBLE);
    IDL_StoreScalarZero(argv[5], IDL_TYP_LONG);

	if ((n_lat_in != n_lon_in) || (n_lat_in != n_height_in))
		IDL_Message(IDL_M_NAMED_GENERIC,IDL_MSG_LONGJMP,
		"Array dimensions differ.");

    plat_out=(double *) IDL_MakeTempVector(IDL_TYP_DOUBLE,
		n_lat_in, IDL_ARR_INI_ZERO, &lat_out);	
    plon_out=(double *) IDL_MakeTempVector(IDL_TYP_DOUBLE,
		n_lat_in, IDL_ARR_INI_ZERO, &lon_out);	
    perr=(int *) IDL_MakeTempVector(IDL_TYP_LONG,
		n_lat_in, IDL_ARR_INI_ZERO, &err);	

	if ((kw.to_aacgm == 0) && (kw.to_geo == 0))
		IDL_Message(IDL_M_NAMED_GENERIC,IDL_MSG_LONGJMP,
		"Conversion direction not specified.");
	if ((kw.to_aacgm == 1) && (kw.to_geo == 1))
		IDL_Message(IDL_M_NAMED_GENERIC,IDL_MSG_LONGJMP,
		"Only one conversion direction may be specified.");
	if (kw.to_aacgm) flag=0;
	if (kw.to_geo) flag=1;

    if (kw.order) order=kw.order;
		else order=10;

	for (i=0;i<n_lat_in;i++) {
		tlat_in=plat_in[i];
		tlon_in=plon_in[i];
		theight_in=pheight_in[i];

		if ((tlat_in < -90.0) || (tlat_in > 90.0) || 
			(tlon_in < -180.0) || (tlon_in > 360.0) ||
			(theight_in < 0.0))
		IDL_Message(IDL_M_NAMED_GENERIC,IDL_MSG_LONGJMP,
		"Valid ranges are -90 < LAT < +90, -180 < LON < 360, Height > 0.");

		terr=convert_geo_coord(tlat_in,tlon_in,theight_in,&tlat_out,&tlon_out,flag,order);

		if (terr ==0) {
			plat_out[i]=tlat_out;
			plon_out[i]=tlon_out;
		} else{
			plat_out[i]=0;
			plon_out[i]=0;
		}
		perr[i]=terr;
	}

	IDL_VarCopy(lat_out,argv[3]);
	IDL_VarCopy(lon_out,argv[4]);
	IDL_VarCopy(err,argv[5]);

	if (lat_in != argv[0]) IDL_Deltmp(lat_in);
	if (lon_in != argv[1]) IDL_Deltmp(lon_in);
	if (height_in != argv[2]) IDL_Deltmp(height_in);

	IDL_KW_FREE;
}

/*{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|*/

void IDL_CDECL aacgm_load_coef(int argc, IDL_VPTR argv[], char *argk)
{
	char fname[256];
	char yearstr[32];
	char status_msg[256];
	int year;
	int status;

    year=IDL_LongScalar(argv[0]);

    sprintf(yearstr,"%4.4d",year);  
    strcpy(fname,getenv("AACGM_DAT_PREFIX"));  
	if (strlen(fname) > 0) {
		strcat(fname,yearstr);
		strcat(fname,".asc");
		status=load_coef_aacgm(fname);
	} else status=-1;

	if (status == 0) {
		strcpy(status_msg,"Coefficients loaded for year ");
		strcat(status_msg,yearstr);
		strcat(status_msg,".");
		IDL_Message(IDL_M_NAMED_GENERIC,IDL_MSG_INFO,
		status_msg);
	} else
		IDL_Message(IDL_M_NAMED_GENERIC,IDL_MSG_LONGJMP,
		"Coefficient file not found.");
}
/*{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|{{:|*/
