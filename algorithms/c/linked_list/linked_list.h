/*
 * Enrico Franchi (c) 2010
 * This library is released under MIT licese.
 */

#ifndef RK_LINKED_LIST_H
#define RK_LINKED_LIST_H

#if __STDC_VERSION__ >= 199901L
  /* "inline" is a keyword */
#else
# define bool int
#endif

#include <stdlib.h>

struct rk_dllist_node {
    struct rk_dllist_node* prev;
    struct rk_dllist_node* next;
    size_t size;
    char value[];
};

struct rk_dllist_handle {
    struct rk_dllist_node* first;
    struct rk_dllist_node* last;
};

typedef struct rk_dllist_handle rk_dllist[1];
typedef struct rk_dllist_node rk_dllist_iterator[1];

void rk_dllist_init(rk_dllist lst);
bool rk_dllist_empty(rk_dllist lst);
void rk_dllist_destroy(rk_dllist lst);


struct rk_dllist_node* rk_dllist_push_front(rk_dllist lst,
                                        void* value, size_t size);
struct rk_dllist_node* rk_dllist_push_back(rk_dllist lst,
                                        void* value, size_t size);

void* rk_dllist_pop_front(rk_dllist lst, size_t* size);
void* rk_dllist_pop_back(rk_dllist lst, size_t* size);
void* rk_dllist_pop_front_ns(rk_dllist lst);
void* rk_dllist_pop_back_ns(rk_dllist lst);

void* rk_dllist_peek_front(rk_dllist lst, size_t* size);
void* rk_dllist_peek_back(rk_dllist lst, size_t* size);
void* rk_dllist_peek_front_ns(rk_dllist lst);
void* rk_dllist_peek_back_ns(rk_dllist lst);

void rk_dllist_iterator_init(rk_dllist_iterator it, rk_dllist lst);
bool rk_dllist_iterator_has_next(rk_dllist_iterator it);
bool rk_dllist_iterator_has_prev(rk_dllist_iterator it);
bool rk_dllist_iterator_next(rk_dllist_iterator it);
bool rk_dllist_iterator_prev(rk_dllist_iterator it);

void* rk_dllist_iterator_deref(rk_dllist_iterator it, size_t* size);
void* rk_dllist_iterator_deref_ns(rk_dllist_iterator it);

struct rk_dllist_node* rk_dllist_insert_before(rk_dllist_iterator it,
                                            void* value, size_t size);
struct rk_dllist_node* rk_dllist_insert_after(rk_dllist_iterator it,
                                            void* value, size_t size);


#endif
