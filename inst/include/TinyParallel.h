#ifndef __FILEARRAY_PARALLEL__
#define __FILEARRAY_PARALLEL__

// TinyThread implementation
#include "TinyParallel/TinyThread.h"

namespace TinyParallel {

inline void parallelFor(std::size_t begin,
                        std::size_t end,
                        Worker& worker,
                        std::size_t grainSize = 1,
                        int numThreads = -1)
{
   grainSize = resolveValue("FILEARRAY_GRAIN_SIZE", grainSize, 1u);
   numThreads = resolveValue("FILEARRAY_NUM_THREADS", numThreads, -1);

   ttParallelFor(begin, end, worker, grainSize);
}

template <typename Reducer>
inline void parallelReduce(std::size_t begin,
                           std::size_t end,
                           Reducer& reducer,
                           std::size_t grainSize = 1,
                           int numThreads = -1)
{
   grainSize = resolveValue("FILEARRAY_GRAIN_SIZE", grainSize, 1);
   numThreads = resolveValue("FILEARRAY_NUM_THREADS", numThreads, -1);

   ttParallelReduce(begin, end, reducer, grainSize);
}

} // end namespace TinyParallel

#endif // __FILEARRAY_PARALLEL__
