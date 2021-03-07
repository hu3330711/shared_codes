#ifndef SDTTypeToCDFType_h
#define SDTTypeToCDFType_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#define SccsId_SDTTypeToCDFType_h "@(#)SDTTypeToCDFType.h	1.3 04/16/04 UCB SSL"

/*
 * SDTTypeToCDFType maps the integer values of the TypeOfStorage field of a
 * ComponentDescription structure, as defined in SDTDescription.h, into the
 * long values used by the CDF software to define storage types, as defined
 * in cdf.h.
 */
long SDTTypeToCDFType(int);

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif  /* SDTTypeToCDFType_h */
