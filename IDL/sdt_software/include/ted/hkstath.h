
#define  SccsId_hkstath_h  "@(#)hkstath.h	1.1, 05/23/00"

#ifndef _hkstatus_h
#define _hkstatus_h
typedef enum {
	HkextractStatusOk,		/* Result Correct */
	HkextractStatusParseAliasesCreate,		/* parse alias create symbol */
	HkextractStatusParseAliasesInit,		/* parse alias initialisation */
	HkextractStatusParseAliasesSyntax,		/* processing aliases */
	HkextractStatusParseAliasesArgs,		/* parse alias input string */
	HkextractStatusCalibrateArgs,		/* arguments to procedure */
	HkextractStatusCalibrateUndefined,		/* calibration limits */
	HkextractStatusCheckAdid,		/* hpd/hkd packet header conatains invalid data source */
	HkextractStatusCheckHpdAdid,		/* hpd data source violation */
	HkextractStatusCheckHkdAdid,		/* hkd data source violation */
	HkextractStatusCheckHkdStream,		/* hkd data stream violation */
	HkextractStatusCheckHpdStream,		/* hpd data stream violation */
	HkextractStatusCheckHkdGsid,		/* hkd data gsid violation */
	HkextractStatusCheckHpdGsid,		/* hpd data gsid violation */
	HkextractStatusCheckScetDay,		/* ddsp scet day test */
	HkextractStatusCheckScetMs,		/* ddsp ms test */
	HkextractStatusCheckScetUs,		/* ddsp us test */
	HkextractStatusCheckScid,		/* dds packet header */
	HkextractStatusCheckScidAdidMatch1,		/* scid = 1/adid match */
	HkextractStatusCheckScidAdidMatch2,		/* scid = 2/adid match */
	HkextractStatusCheckScidAdidMatch3,		/* scid = 3/adid match */
	HkextractStatusCheckScidAdidMatch4,		/* scid = 4/adid match */
	HkextractStatusCheckScidAdidMatchX,		/* scid != {1 */
	HkextractStatusCheckStream,		/* hkd/hpd packet header contains invalid data stream */
	HkextractStatusCheckStreamAndAdidVc0,		/* stream = VC 0/adid match */
	HkextractStatusCheckStreamAndAdidVc2,		/* stream = VC 2/adid match */
	HkextractStatusCheckStreamAndAdidVc3,		/* stream = VC 3/adid match */
	HkextractStatusCheckStreamAndAdidNa,		/* stream = Na/adid match */
	HkextractStatusCheckStreamAndAdidX,		/* stream = X/adid match */
	HkextractStatusCheckTasiHkd,		/* hkd tasi */
	HkextractStatusCheckTasiHpd,		/* hpd tasi */
	HkextractStatusCheckTcal,		/* time quality is invalid */
	HkextractStatusCheckTcalHpd,		/* hpd time quality != actual */
	HkextractStatusDecodeValue,		/* Unpack Symbol Text symbol type not text */
	HkextractStatusGetAttribArgs,		/* argument to procedure */
	HkextractStatusGetAttribRes,		/* argument to result */
	HkextractStatusGetAttribSymbol,		/* get/unpack attribute */
	HkextractStatusGetAttribSymbolArgs,		/* argument symbol */
	HkextractStatusGrowTable,		/* next table size */
	HkextractStatusGrowTableArgs,		/* arguments */
	HkextractStatusGrowTableDup,		/* table transfer */
	HkextractStatusGrowTableLimit,		/* maximum table size */
	HkextractStatusGrowTableSize,		/* shrinking table size */
	HkextractStatusHpdHkdAdidMatch,		/* ddsp hpd/hkd adid */
	HkextractStatusInsertSymbol,		/* Insert symbol failed to verify the symbol or destination table */
	HkextractStatusInsertSymbolArgs,		/* argument to procedure */
	HkextractStatusInsertSymbolName,		/* search string */
	HkextractStatusInsertSymbolNotATable,		/* Insert symbol destination symbol not a table */
	HkextractStatusInsertSymbolTable,		/* Insert Symbol failed to place source symbol in destination table */
	HkextractStatusInsertSymbolTableInvalid,		/* Insert Symbol found no table in the destination symbol */
	HkextractStatusLookupFailed,		/* Search Symbol failed to locate a symbol */
	HkextractStatusPacketLength,		/* ddsp data > 2^24 -1 */
	HkextractStatusPdeFieldReadArgs,		/* arguments to procedure */
	HkextractStatusPdeFieldReadInput,		/* inut string */
	HkextractStatusReadAliasesArgs,		/* argument to procedure */
	HkextractStatusReadAliasesInit,		/* configuring for Parse */
	HkextractStatusReadAliasesParse,		/* parsing alias string */
	HkextractStatusReadAliasesTable,		/* acess to alias table */
	HkextractStatusSearchSymbolNotATable,		/* type */
	HkextractStatusTableSearchNotFound,		/* search failed */
	HkextractStatusTableSearchArgs,		/* arguments */
	HkextractStatusTableInsertArgs,		/* arguments */
	HkextractStatusTableInsertGrow,		/* extending table */
	HkextractStatusTableInsertSize,		/* hash index too high */
	HkextractStatusTableInsertAlready,		/* collision */
	HkextractStatusTableInsertHash,		/* hashing */
	HkextractStatusUnpackSymbolTextType,		/* not text */
	HkextractStatusUnpackSymbolTextValue,		/* extracting value */
	HkextractStatusValidateHkHeaderAdid,		/* validate hk header adid */
	HkextractStatusValidateHkHeaderGsid,		/* validate hk header gsid */
	HkextractStatusValidateHkHeaderLength,		/* validate hk ddsp header length */
	HkextractStatusValidateHkHeaderMatchScidAndSource,		/* validate hk ddsp header scid and source match */
	HkextractStatusValidateHkHeaderScid,		/* validate hk header scid */
	HkextractStatusValidateHkHeaderStream,		/* validate hk ddsp header stream */
	HkextractStatusValidateHkHeaderStreamAndAdid,		/* validate hk ddsp header adid and stream */
	HkextractStatusValidateHkHeaderTasi,		/* validate hk ddsp tasi */
	HkextractStatusValidateHkHeaderTcal,		/* validate hk ddsp tcal */
	HkextractStatusValidateHkHeaderArgs,		/* hk ddsp validation arg */
	HkextractStatusValidatePacketAndTableArgs,		/* packet/table validation pre-conditions */
	HkextractStatusValidatePacketAndTableAdid,		/* packet/table validation hkd adid */
	HkextractStatusValidatePacketAndTableTime,		/* packet/table validation hkd time */
	HkextractStatusVerifyTable,		/* validate packet with table table invalid */
	HkextractStatusParsePdeDataArgs,		/* pre-conditions on arguments */
	HkextractStatusParsePdeDataLexError,		/* syntax error in a PDE */
	HkextractStatusParsePdeDataSymbol,		/* inserting PDE to table */
	HkextractStatusMemory		/* memory resources failiure */
} HkextractStatus;
#endif
