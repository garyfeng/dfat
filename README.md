# dfat
An operator `@` to extract a named member of a list that is in a list and returns a vector.

**note**: this library masks the ```@``` or "slotOp" in the base module that is reserved for S4 objects only. 

## Installation ##

```
require(devtools)
devtools::install_github('garyfeng/dfat')
```

## Examples ##

```
require(dfat)

testvec <- list(
  list(from=1, to="here", date="1990-12-12"), 
  list(from=2, to="there", via=list("train", "airplane"), date=123.45)
)

key <- "from"

# different ways to invoke `@`
testvec%@%"from"
testvec%@%"to"
testvec@key
testvec@"to"
`@`(testvec, 2)

# the permissive %@@% returns a list instead of coercing into a vector
testvec%@@%"from"
testvec%@@%"to"
`%@@%`(testvec, 2)

```

See also ```tests\testthat.R```.
