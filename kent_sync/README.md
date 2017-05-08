# Kent1

Synchronous code sample.

## Instructions

Activate the erlang kerl installation:

```
$ source ~/kerl/1.9.3/activate
```

Run the interactive shell:
```
$ erl

# ...

> c(expr).
{ok,expr}
> expr:exp2().
{add,{num,1},{mul,{num,3},{num,0}}}
> expr:simplify(kent1:exp1()).
{num,1}
> q().
```
