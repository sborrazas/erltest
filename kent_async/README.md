# Kent2

Asynchronous code sample.

## Instructions

Activate the erlang kerl installation:

```
$ source ~/kerl/1.9.3/activate
```

Run the interactive shell:
```
$ erl
```

### Example 1: Simple async abstraction using spawn

```
> c(area_consumer, [export_all]).
{ok,area_consumer}
> area_consumer:register_calculator().
{ok,the_calculator}
> area_consumer:consume_registered().
100
> area_consumer:consume().
100
> q().
```

### Example 2: RPC abstraction

```
> c(rpc_template).
{ok,rpc_template}
> Pid = spawn(fun() ->
      receive
        {SenderPid, Tag, N} ->
          SenderPid ! {Tag, N + 1}
      end
    end).
<0.66.0>
> MyTag = rpc_template:rpc(Pid, 2).
#Ref<0.0.6:200>
> rpc_template:wait_receive(MyTag).
{ok, 3}
```

### Example 3: First server abstraction (with loop)

```
> c(counter0)
{ok,counter0}
> counter0:start().
true
> counter0:tick(5).
ack
> counter0:read().
5
```

### Example 4: Generic server (using gen_server_lite)

```
> c(counter1).
{ok,counter1}
> counter1:start().
true
> counter1:tick(4).
ack
> counter1:read().
4
```

### Example 5: Simple cli-serv that responds to expressions

```
> c(calc).
{ok,calc}
> calc:start().
true
> calc:eval({mul, {num, 2}, {num, 8}}).
16
```
