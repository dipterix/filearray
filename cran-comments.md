## Dev environment 
* osx (ARM), R 4.2.1

## Test environments
* osx (x64, github-action), R-release
* ubuntu 18.04 (github-action), R-release, R-devel
* win-builder (https://win-builder.r-project.org/), R-release, R-devel
* Solaris (rhub)

## R CMD check results

On `oldrelease`, `release` and `devel`
0 errors | 0 warnings | 0 notes

## Additional tests

* `clang-UBSAN`: using `valgrind` to test all examples and tests (single threaded). The result showed insignificance.

```
==40499== 
==40499== HEAP SUMMARY:
==40499==     in use at exit: 178,803,840 bytes in 20,419 blocks
==40499==   total heap usage: 1,294,546 allocs, 1,274,127 frees, 1,198,582,624 bytes allocated
==40499== 
==40499== LEAK SUMMARY:
==40499==    definitely lost: 0 bytes in 0 blocks
==40499==    indirectly lost: 0 bytes in 0 blocks
==40499==      possibly lost: 0 bytes in 0 blocks
==40499==    still reachable: 178,803,840 bytes in 20,419 blocks
==40499==                       of which reachable via heuristic:
==40499==                         newarray           : 4,264 bytes in 1 blocks
==40499==         suppressed: 0 bytes in 0 blocks
==40499== Reachable blocks (those to which a pointer was found) are not shown.
==40499== To see them, rerun with: --leak-check=full --show-leak-kinds=all
==40499== 
==40499== For lists of detected and suppressed errors, rerun with: -s
==40499== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
```


