---
name: mode
filename: R/sample.R
layout: page
roxygen:
  title: Mode
  description: Find the mode of a vector.
  parameters:
  - name: x
    description: A vector of values.
  - name: na.rm
    description: Should `NA`s be removed, defaults to `TRUE`.
  sections:
  - title: When to use
    body: This function can always be used.
  - title: How to use
    body: This function will not work.
  return: The most common element of `x`.
  family: utils
  examples:
  - title: '##  c("Basic usage", "mode(c(1, 2, 3, 4))")'
    source: ''
    output: ~
  - title: '##  c("Remove NAs", "mode(c(1, NA, 2, 2, 5), na.rm = TRUE)")'
    source: ''
    output: ~
---
