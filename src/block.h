/* SPDX-License-Identifier: BSD-3-Clause */
/*
 * Authors: Simon Kuenzer <simon.kuenzer@neclab.eu>
 *          Fabrice Buoro <fabrice@tarides.com>
 *          Samuel Hym <samuel@tarides.com>
 *
 * Copyright (c) 2013-2017, NEC Europe Ltd., NEC Corporation.
 *               2024-2025, Tarides.
 *               All rights reserved.
*/

#ifndef BLOCK_H
#define BLOCK_H

#include <uk/blkdev.h>
#include <uk/assert.h>
#include <uk/print.h>
#include <uk/alloc.h>

#include "yield.h"

#define MBU_DEBUG 0 /* FIXME */

#ifdef MBU_DEBUG
#define UK_DEBUG 1
#endif /* !MBU_DEBUG */

#define QUEUE_COUNT     1

struct token_s;
typedef struct token_s token_t;
typedef unsigned int token_id_t;

struct token_s {
  struct uk_blkreq  req;
  struct block_s    *block;
};

typedef struct block_s {
  struct uk_blkdev  *dev;
  struct uk_alloc   *alloc;
  token_t           *tokens;
  unsigned long     infly;
  unsigned int      id;
} block_t;


#endif /* !BLOCK_H */
