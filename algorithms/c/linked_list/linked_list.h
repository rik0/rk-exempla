/*
 * Enrico Franchi (c) 2010
 * This library is released under MIT licese.
 */

#ifndef RK_LINKED_LIST_H
#define RK_LINKED_LIST_H

#include <stdlib.h>

struct rk_dllist_node {
    struct rk_dllist_node* prev;
    struct rk_dllist_node* next;
    size_t size;
    unsigned char value[];
};

struct rk_dllist_handle {
    struct rk_dllist_node* first;
    struct rk_dllist_node* last;
};

typedef struct rk_dllist_handle rk_dllist[1];
typedef struct rk_dllist_node rk_dllist_iterator[1];

void rk_dllist_init(rk_dllist lst);
bool rk_dllist_is_empty(rk_dllist lst);
void rk_dllist_destroy(rk_dllist lst);


struct rk_dllist_node* rk_dllist_push_front(rk_dllist lst,
                                        void* value, size_t size);
struct rk_dllist_node* rk_dllist_push_back(rk_dllist lst,
                                        void* value, size_t size);


typedef enum {
    RK_OK = 0, RK_EEMPTY, RK_EBUFFER
} peek_status;

peek_status rk_dllist_pop_front(rk_dllist lst);
peek_status rk_dllist_pop_back(rk_dllist lst);

peek_status rk_dllist_peek_front_size(rk_dllist lst, size_t* size);
peek_status rk_dllist_peek_back_size(rk_dllist lst,  size_t* size);

peek_status rk_dllist_peek_front(rk_dllist lst, void* buff, size_t max_size);
peek_status rk_dllist_peek_back(rk_dllist lst, void* buff, size_t max_size);

peek_status rk_dllist_peek_front_notsafe(rk_dllist lst, void* buff);
peek_status rk_dllist_peek_back_notsafe(rk_dllist lst, void* buff);

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

bool rk_dllist_remove_before(rk_dllist_iterator it);
bool rk_dllist_remove_after(rk_dllist_iterator it);

#endif
