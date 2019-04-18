关于erlang的调试
===

Use Erlang debugger
---

``` erlang
1> c(test_module, [debug_info]).
{ok, test_module}
2> debugger:start().
```

Debugging a badmatch error here is aided by using the command line debugger:

``` erlang
1> dbg:tracer().                            % Start the CLI debugger
{ok,<0.55.0>}
2> dbg:p(all, c).                           % Trace all processes, only calls
{ok,[{matched,nonode@nohost,29}]}
3> dbg:tpl(my_module, something, x).        % tpl = trace local functions as well
{ok,[{matched,nonode@nohost,1},{saved,x}]}
4> dbg:tp(other, do, x).                    % tp = trace exported functions  
{ok,[{matched,nonode@nohost,1},{saved,x}]}
5> dbg:tp(my_module, run, x).               % x means print exceptions
{ok,[{matched,nonode@nohost,1},{saved,x}]}  % (and normal return values)
```

Look for {matched,_,1} in the return value... if this would have been 0 instead of 1 (or more) that would have meant that no functions matched the pattern.