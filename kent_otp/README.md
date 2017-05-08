# Kent3

OTP code sample.

## Instructions

Activate the erlang kerl installation:

```
$ source ~/kerl/1.9.3/activate
```

Run the interactive shell:
```
$ erl
```

### Example 1: Cli-serv that responds to expressions (server/)

Generic | Specific
------- | --------
Spawning the server | Initializing the server state
Storing the loop data | The loop data
Sending requests to the server | The client requests
Sending replies to the client | Handling client requests
Receiving server replies | Contents of server reply
Stopping the server | Cleaning up

Splits calc into the generic/specific behavior (calc is specific, server is
generic).

```
> c(calc).
{ok,calc}
> calc:start([{a, 3}]).
true
> calc:eval({mul, {var, a}, {num, 8}}).
16
```

### Example 2: gen_server behavior and sync/async requests (gen_server/)

Addresses the following issues:

1. Receiving replies that weren't sent by the server I did the request to.
2. Detecting if the server dies so I don't have to continue waiting.
3. Upon having sent the request to the server, the server responds with the
   reply, but before receiving the reply the server terminates. The response
   should be received before the `DOWN` server event, otherwise, the response is
   never processed.
4. Many other raise conditions.

This is done through the `gen_server` behavior.

```
> c(calc).
{ok,calc}
> calc:start([{a, 3}]).
{ok,<0.66.0>}
> calc:eval({var, a}).
3
> calc:print({var, a}).
a
```

### Example 3: supervisor behavior

Supervisors will:
* Start child processes: workers (leaves) or other supervisors.
* Monitor their children.
* Restart their children when they terminate.

Generic | Specific
------- | --------
spawning the supervisor | Supervisor name
Starting the children | What children to start
Monitor the children | Specific child handling (Start, Restart)
Stopping the supervisor | Child dependencies
Cleaning up | Supervisor behaviors

* Supervisor specification: `{RestartTuple, [ChildSpecification]}`
* RestartTuple specification: `{RestartType, MaxRestart, MaxTime}`
* ChildSpecification:
  `{Name, StartFunction, RestartType, ShutDownTime, ProcessType, Modules}`

The `RestartType` can be one of:
* `one_for_one` – Only the crashes process is restarted.
* `one_for_all` – All processes are terminated and restarted. Used when all (or
  most) of the processes depend on each other.
* `rest_for_one` – All processes are started after the crashed one are
  terminated and restarted. Used if you start your processes in order of
  dependency.

`MaxRestart` determines the maximum amount of restart all child processes are
allowed to do in `MaxTime` seconds. If this threshold is reached, the supervisor
terminates itself.

`Name` is a name that has to be unique for a particular supervisor.

`StartFunction` is the start function (in the form `{Module, Function, Args}`).
This function must call an OTP-compliant start_link function.

`RestartType` can be one of:
* `permanent` – The process is always restarted.
* `transient` – The process is only restarted after a non-normal exit.
* `temporary` – The process is never restarted.

`ShutdownTime` is the time the process is allowed to spend in terminate. This
can be an integer (greater than 0) or `infinity`.

`ProcessType` is used for software upgrades. Should be `supervisor` for
supervisors or `worker` for any other behavior.

`Modules` are the list of modules implementing the child.

```
> c(calc).
{ok,calc}
> c(calc_sup).
{ok,calc_sup}
> Env = [{a, 3}].
[{a, 3}]
> calc_sup:start_link(Env).
{ok,<0.69.0>}
> calc:print({var, b}).
b
```

### Example 3: Building applications

Applications are started with `application:start(Name)` in the `application.erl`
file. The start function in the callback module starts the supervisor.

If not loaded, `application:load(Name)` is called prior to the start.

`StartType` can be `normal`, `{takeover, Node}`, `{failover, Node}`.

Projects also include a resource file (e.g. `calc.app`). Sample resource file:
```erlang
{application, calc,
  [{description, "Calc application!"},
   {vsn, "1.0"},
   {registered, [calc_sup, calc]},
   {applications, [kernel,stdlib]},
   {env, [{env, [{a, 23}]}]},
   {mod, {calc_app, []}}]}.
```

We now place the application resource file and the binaries in `ebin/` and start
it as follows:
```
> application:start(calc).
ok
> calc:eval({add, {num, 3}, {num, 5}}).
8
> whereis(calc).
<0.63.0>
> application:which_applications().
[{calc,"Calc application!","1.0"},
 {stdlib,"ERTS  CXC 138 10","3.3"},
 {kernel,"ERTS  CXC 138 10","5.2"}]
```

We can now bundle all of these files through a release file:
```
{release,
  {"OTP APN 181 01", "R14B02"},
  {erts, "5.8.3"},
  [{kernel, "2.14.3"},
   {stdlib, "1.17.3"},
   {calc, "1.0"}]}
```

This creates a boot file, which creates a start script that boots the node and
initialises the shell environment. `rebar3` automates this process.
