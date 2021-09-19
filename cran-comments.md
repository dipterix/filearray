## Dev environment 
* osx (ARM), R 4.1.0 Patched (2021-06-14 r80505)

## Test environments
* osx (x64, github-action), R-release
* ubuntu 18.04 (github-action), R-release, R-devel
* win-builder (https://win-builder.r-project.org/), R-release, R-devel

## R CMD check results

On R-4.X
0 errors | 0 warnings | 0 notes

On R-3.6
0 errors | 1 warning | 0 notes

```
Codoc mismatches from documentation object 'apply':
apply
  Code: function(X, MARGIN, FUN, ...)
  Docs: function(X, MARGIN, FUN, ..., simplify = TRUE)
  Argument names in docs not in code:
    simplify
```

This is because `simplify` was added to `apply` function since R-4.0.
