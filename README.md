# sqrl-auth-daemon
System wide SQRL client delegation for CPS on listening on port 25519. This is only for POSIX systems.

Install by using:
```bash
$ git clone "https://github.com/TimLuq/sqrl-auth-daemon.git"
$ cd sqrl-auth-daemon
$ cabal install
```
(This requires you to have `cabal` and `ghc` installed, perhaps using the package `haskell-platform`.)

## How it works
The daemon will, first of all, deny any connection from any IP other than `127.0.0.1`.
It will otherwise proceed by looking up what process is connecting to it by making a call to `fuser`,
thereafter it knows the PID to the process making the request. After additional checks for who is running the process and reading the environment variables associated to the process it will pass these on to the next step.

### Mimicing the user
After retrieving all information needed to fully impersonate the user it will `fork(2)` a new process.
The new process will change its GID and UID to that of the user (using `setgid(2)` and `setuid(2)`), to ensure no privilege elevation occurs, before replacing that newly created process with the SQRL client named `sqrl-cps` (using `execvpe(3)`).

The client will be started with filtered list of the environment variables that was in use by the requesting application.
This includes `DISPLAY` and `XAUTHORITY` among others.
Any GUI opened as a consecuense of the request will start in the same session of X as the application that initiated the request.
This allows for multiple users to be connected to the same machine without interference and any user that is connected through multiple sessions will have any GUI show up in the right place.

The client is started with the following arguments:
* optional `--cps-referer=http://example.com/page` - the contents of the `Referer` HTTP header, if any.
* `--cps-daemon-flags=3` - a decimal number designation which flags are respected by the daemon (see [Flags](#flags))
* `--cps-user=nixuser` - the name of the user running the process that connected
* `--cps-invoker=cromium-browse` - the name of the process application
* `--cps-invoker-full=/var/lib/chromium-browser/chromium-browser` - the full path to the executable that is making the request
* `--cps-port=50123` - the port number that was used for the request (might be good for logging purposes)
* `--cps-query-string=?nut=rctXDOVqYKZi-XZeASYflA&sfn=RXhhbXBsZQ` - the query string used when connecting to 127.0.0.1:25519
* `--cps-path-info=/example.com/sqrl` - the path used when connecting to 127.0.0.1:25519
* `sqrl://example.com/sqrl?nut=rctXDOVqYKZi-XZeASYflA&sfn=RXhhbXBsZQ` - the sqrl link that is requested

### Client Action
The client that gets executed during the call to `sqrl-cps` (wich currently must be placed in either `$HOME/.sqrl/bin/sqrl-cps` or `/bin/sqrl-cps`) reads the arugment list using what it think is important.
It may choose to disregard all other arguments than the `sqrl://example.com/sqrl?nut=rctXDOVqYKZi-XZeASYflA&sfn=RXhhbXBsZQ` without any additional checks.
The client then performs any SQRL actions it finds neccessary before sending the user to some other content.

When the client is done; either by a successful login, a failure, or another action.
The client will generate the response which will be shown to the user.
The easiest is by only sending the user on by only giving the deamon an URI to the new resource.
To ensure that the application being executed actually has something to do with SQRL the daemon awaits a header containing the following 7 bytes "SQRLCPS" followed by an additional byte representing some [flags](#Flags) regarding the response.

#### <a name="flags"></a>Flags
* `0b00000001` - the client will not send a redirect URI but instead it will send a number of HTTP headers, each separated by CRLF (`"\r\n"`) followed by an empty line. (As is the standard for HTTP.)
* `0b00000010` - after all other processing is done the entity of this response is sent to `stdout` until `EOF`. Otherwise the deamon may generate an enity of it's choice.

If the first flag (`0b00000001`) _is not_ set then the client _must_ write a 16 bit big-endian number to `stdout` which is the length of the URI to redirect to.
After the two bytes representing the length, that many bytes are written to `stdout`, this is the absolute URI for redirection.

If the first flag (`0b00000001`) _is_ set then the client _must_ write a number of HTTP headers, each separated by `CRLF`, to `stdout`. This is then followed by an extra `CRLF` which means end of headers, as per the HTTP specification.

if the second flag (`0b00000010`) _is_ set then the client, after everything else is send, will write the response entity to `stdout`. If this flag _is not_ set the deamon may create a response of its choice.
