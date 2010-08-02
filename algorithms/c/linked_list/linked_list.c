/*
 * Enrico Franchi (c) 2010
 * This library is released under MIT licese.
 */

#include "linked_list.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

struct rk_dllist_node {
    struct rk_dllist_node* prev;
    struct rk_dllist_node* next;
    char value[];
};

struct rk_dllist_node*
rk_dllist_node_alloc(struct rk_dllist_node** node, size_t sz) {
    void* tmp = malloc(sizeof(struct rk_dllist_node) + sz);
    if(tmp != NULL) {
        return (*node = tmp);
    } else {
        return NULL;
    }
}

struct rk_dllist_node*
rk_dllist_push(void* value, size_t size, rk_dllist tail) {
    struct rk_dllist_node* new_node;
    assert(tail != NULL);
    assert(tail->next == NULL);
    if(rk_dllist_node_alloc(&new_node, size)) {
        rk_dllist_node_init(new_node, value, size, tail, NULL);
        return (tail->next = new_node);
    }
    return 0;
}

void
rk_dllist_node_init(struct rk_dllist_node* node,
                        void* value, size_t sz,
                        struct rk_dllist_node* prev,
                        struct rk_dllist_node* next) {
    memcpy(node->value, value, sz);
    node->prev = prev;
    node->next = next;
}
