# annex

annex is web proxy server.


## Goals

annex provide HTTP and https protocol implement proxy and reverse proxy.
this is small code base.
manage TCP connection, session level and connection.
websocket connection manage is not support.
websocket connection recommend back server to client direct connect.


## Example

```erlang
Hosts = [
  #{host => {127, 0, 0, 1}, port => 8010},
  #{host => {127, 0, 0, 1}, port => 8020}
],
Pid = make_ref(),
ChildSpec = annex_proxy_sup:child_spec(Pid, annex_proxy, 5555,
  [
    #{
      hosts => Hosts
    },
    #{
      url => <<"/hello">>,
      hosts => [
        #{host => {127, 0, 0, 1}, port => 8020}
      ]
    }
  ]),
annex_proxy_sup:start_child(ChildSpec).
```
