project(rkalglib)

SET(HEADERS
       util.h
       linked_list.h)
SET(SOURCES
        linked_list.c
    )

add_library(rkalglib ${HEADERS} ${SOURCES})
