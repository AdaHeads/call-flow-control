Contents of this README:

    1. Introduction
    2. Dependencies
    3. Installing
    4. Configuring
    5. References

# Introduction

Call-flow-control is a middleware component that serves as a translation/synchronization layer 
between FreeSWITCH and our domain-specific application for routing calls through
a human receptionist.

Interaction and information is provided by a REST interface and a websocket - respectively.
API documentation can be found in the project wiki.

Alice is GPLv3, see COPYING3 and COPYING.RUNTIME.

# Dependencies

Call-flow-control depends on libesl[1], AWS[2] and the AdaHeads database servers stack[3].

IMPORTANT: call-flow-control depends on a running FreeSWITCH server deployed with a custom
  configuration. As this configuration is still private, you need to contact AdaHeads to
  obtain a copy. AdaHeads can be reached via info@adaheads.com.

# Installing

Running `make install` will install call-flow to it's default location: `/usr/local/call-flow`.
An alternate location can be set by overriding `PREFIX`. E.g
`PREFIX=/home/myuser/adaheads make install` will install to `/home/myuser/adaheads` instead.

# Configuring

You need to change the parameters (server names) in `$PREFIX/conf/main.conf` prior to starting
This should be all.

# Running

Call-flow-control can be run interactively, or started as a service. See the tools/ folder for
init scripts.

# References

[1] libesl
    https://github.com/AdaHeads/libesl
    Git: git clone git://github.com/AdaHeads/libesl.git

[2] Ada Web Server
    http://libre.adacore.com/libre/tools/aws/
    Git: git clone --recursive http://forge.open-do.org/anonscm/git/aws/aws.git

[3] AdaHeads Database servers stack
    https://github.com/AdaHeads/DatabaseServers
    Git: git clone git://github.com/AdaHeads/databaseservers.git
