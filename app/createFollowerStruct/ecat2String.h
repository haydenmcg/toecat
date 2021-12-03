/**
 *  \file   ecat2String.h
 *
 */

#ifndef _ECAT_2_STRING_ 
#define _ECAT_2_STRING_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include "ethercat.h"


//-----------------------------------------------------------------------------
//
//
char* dtype2string(uint16 dtype);

//-----------------------------------------------------------------------------
//
//
char* SDO2string(uint16 follower, uint16 index, uint8 subidx, uint16 dtype);






#ifdef __cplusplus
}
#endif

#endif /* _ECAT_2_STRING_*/
