/*
 * Enrico Franchi (c) 2010
 * This library is released under MIT licese.
 */

#ifndef RK_LINKED_LIST_H
#define RK_LINKED_LIST_H

#include <stdlib.h>

struct rk_dllist_node;

typedef struct rk_dllist_node* rk_dllist;

struct rk_dllist_node* rk_dllist_cons(void* value, size_t size,
                                      rk_dllist tail);
struct rk_dllist_node* rk_dllist_rcons(void* value, size_t size,
                                       rk_dllist tail);


void rk_dllist_node_init(struct rk_dllist_node* node,
                        void* value, size_t size,
                        struct rk_dllist_node* prev,
                        struct rk_dllist_node* next);

struct rk_dllist_node* rk_dllist_node_alloc(
                struct rk_dllist_node** node, size_t sz);

#endif
