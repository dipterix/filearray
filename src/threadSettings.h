#ifndef FARR_OPENMP_H
#define FARR_OPENMP_H

#ifdef _OPENMP
#include <omp.h>
#include <pthread.h>
#define FARR_HAS_OPENMP true
#else
#define omp_get_thread_num() 0
#define omp_get_max_threads() 1
#define FARR_HAS_OPENMP false
#endif

#include "common.h"
#include <algorithm>


int getThreads(const bool& max = false);


#endif  // FARR_OPENMP_H
