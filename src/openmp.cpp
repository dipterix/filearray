#include "openmp.h"

static int ompThreads = 0;

// stores n threads when fork occurs
static bool detectFork = false;
static int resetForked = true;

int getThreads(bool max){
#ifdef _OPENMP
    if(detectFork){
        return 1;
    }
    if( max ){
        return omp_get_max_threads();
    }
    int t = ompThreads < 0 ? omp_get_max_threads() : std::min(ompThreads, omp_get_max_threads());
    return std::max(t, 1);
#else
    return 1;
#endif
}

int setThreads(int n, int reset_after_fork){
#ifdef _OPENMP
    if(!detectFork){
        ompThreads = n;
    }
    if( reset_after_fork == 1 ){
        resetForked = true;
    } else if( reset_after_fork == 0 ){
        resetForked = false;
    }
    
    return n;
#else
    return 1;
#endif
}


bool hasOpenMP(){
    return FARR_HAS_OPENMP;
}

void onForked(){
    detectFork = true;
}
void onLeaveFork(){
    if(!resetForked){
        ompThreads = 1;
    }
    detectFork = false;
}

int detectForked(DllInfo *dll){
    // To disable openmp if fork is detected
#ifdef _OPENMP
    return pthread_atfork(&onForked, &onLeaveFork, NULL);
#endif
    
    return 0;
}


