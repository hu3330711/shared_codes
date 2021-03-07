/* idl_aacgm.c
   ============
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

/*
 * Define message codes and their corresponding printf(3) format
 * strings. Note that message codes start at zero and each one is
 * one less that the previous one. Codes must be monotonic and
 * contiguous. Must match the corresponding entries in IDLtoC.h
 */
static IDL_MSG_DEF msg_arr[] =
{
  {  "idl_aacgm_ERROR",		"%NError: %s." },
  {  "idl_aacgm_NOSTRINGARRAY",    "%NString arrays not allowed %s"},
 };

/*
 * The load function fills in this message block handle with the
 * opaque handle to the message block used for this module. The other
 * routines can then use it to throw errors from this block.
 */
IDL_MSG_BLOCK msg_block;

int IDL_Load(void)
{
/*define the message block*/
  if (!(msg_block = IDL_MessageDefineBlock("AACGM", ARRLEN(msg_arr),
	   msg_arr))) {
	return IDL_FALSE;
  }
/*Call the startup function to add the routines to IDL.*/
  if (!idl_aacgm_startup()){
    IDL_MessageFromBlock(msg_block, idl_aacgm_ERROR, 
		IDL_MSG_RET,"Unable to initialize AACGM");
  }
  
  return IDL_TRUE;
}

