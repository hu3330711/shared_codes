/*
 +--------------------------------------------------------------------+ 
 | Copyright(c) 1991 Regents of the University of California          |
 | All rights reserved.                                               |
 |                                                                    |
 | Redistribution and use in source and binary forms are permitted    |
 | provided that the above copyright notice and this paragraph are    |
 | duplicated in all such forms and that any documentation,           |
 | advertising materials, and other materials related to such         |
 | distribution and use acknowledge that the software was developed   |
 | by the University of California, Los Angeles.  The name of the     |
 | University may not be used to endorse or promote products derived  |
 | from this software without specific prior written permission.      |
 | THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR     |
 | IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED     |
 | WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.|
 +--------------------------------------------------------------------+ 

 @(#)time_igpp.h	1.2, 06/02/04
*/

#ifndef TIME_IGPP

#ifdef ANSI_STD_C

double c_con_t( char *at) ;
double i_con_t( int it[8]) ;
void t_con_c( double t, char *at) ;
void i_con_c( int it[8], char *at) ;
void t_con_i( double t, int it[8]) ;
void c_con_i( char *at, int it[8]) ;
double gregorian_day( int dy, int month, int year) ;
void gregorian_date( float d, int *dy, int *month, int *year) ;
double t_now() ;
int cc_num_str( char **a, int w, int *k) ;
int cc_mon_str( char **a, int *k) ;

#else /* ANSI_STD_C */

double c_con_t(),i_con_t(),gregorian_day(),t_now();
void t_con_c(),c_con_i(),i_con_c(),t_con_i(),gregorian_date();

#endif /* ANSI_STD_C */

#define TIME_IGPP
#endif

