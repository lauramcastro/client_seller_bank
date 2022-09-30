Client-seller-bank
=====

An OTP application that serves as sample project to demonstrate
the use of a  [session type checker for Erlang](https://github.com/lauramcastro/sessiontypechecker).

Build
-----

    $ rebar3 compile

Run
-----

    $ rebar3 shell

On running the application, a client, a seller and a bank will interact as described in the sequence diagram below.
After interacting, they will end their execution normally.

Consequently, one should see one of the following outputs:

**Client completes the transaction for the chosen title**

```
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling client_seller_bank
===> Booted client_seller_bank
Erlang/OTP 24 [erts-12.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [jit]
Eshell V12.2.1  (abort with ^G)
1> [whereis(client), whereis(seller), whereis(bank)].
[undefined,undefined,undefined]
```

thus confirming the three actors have finished. Upon tearing down the application, execution traces
that were collected will be printed, ilustrating what happened:

```
2> application:stop(client_seller_bank).             
== APPLICATION PARTICIPANTS ==
[{client,<0.156.0>,worker,[client]},{bank,<0.155.0>,worker,[bank]},{seller,<0.154.0>,worker,[seller]}]
== START EXECUTION TRACES (41) ==
client --> seller : {client,title,"Novecento"}
seller processes: {client,title,"Novecento"}
seller --> client : {seller,price,25}
client processes: {seller,price,25}
client --> seller : {client,ok}
seller processes: {client,ok}
seller --> bank : {seller,price,25}
seller --> bank : {seller,start_delegation,<0.154.0>}
bank processes: {seller,price,25}
bank processes: {seller,start_delegation,<0.154.0>}
bank --> client : {seller,pay,25}
client processes: {seller,pay,25}
client --> seller : {client,card,"0000000000000000"}
bank processes: {client,card,"0000000000000000"}
bank --> seller : {bank,end_delegation}
bank --> seller : {bank,ok}
seller processes: {bank,end_delegation}
seller processes: {bank,ok}
seller --> client : {seller,date,"22 ottobre 2022"}
bank ends 
seller ends 
client processes: {seller,date,"22 ottobre 2022"}
client ends 
== END EXECUTION TRACES ==
```

**Client does not complete the transaction for the chosen title because of the bank rejecting its card**

```
== APPLICATION PARTICIPANTS ==
[{client,<0.148.0>,worker,[client]},{bank,<0.147.0>,worker,[bank]},{seller,<0.146.0>,worker,[seller]}]
== START EXECUTION TRACES (44) ==
client --> seller : {client,title,"Novecento"}
seller processes: {client,title,"Novecento"}
seller --> client : {seller,price,25}
client processes: {seller,price,25}
client --> seller : {client,ok}
seller processes: {client,ok}
seller --> bank : {seller,price,25}
seller --> bank : {seller,start_delegation,<0.146.0>}
bank processes: {seller,price,25}
bank processes: {seller,start_delegation,<0.146.0>}
bank --> client : {seller,pay,25}
client processes: {seller,pay,25}
client --> seller : {client,card,"123456789101112"}
bank processes: {client,card,"123456789101112"}
bank --> seller : {bank,end_delegation}
bank --> seller : {bank,ko}
seller processes: {bank,end_delegation}
seller processes: {bank,ko}
seller --> client : {seller,ko}
bank ends 
seller ends 
client processes: {seller,ko}
client ends 
== END EXECUTION TRACES ==

```

**Client does not complete the transaction for the chosen title due to high price**

```
== APPLICATION PARTICIPANTS ==
[{client,<0.148.0>,worker,[client]},{bank,<0.147.0>,worker,[bank]},{seller,<0.146.0>,worker,[seller]}]
== START EXECUTION TRACES (31) ==
client --> seller : {client,title,"Ultimo tango a Parigi"}
seller processes: {client,title,"Ultimo tango a Parigi"}
seller --> client : {seller,price,100}
client processes: {seller,price,100}
client --> seller : {client,ko}
seller processes: {client,ko}
client ends 
seller ends 
== END EXECUTION TRACES ==
```

Note that in this latter case, the bank will stay alive since, as a delegate, it only acts upon delegation from the seller, as stated by the session type:

```
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling client_seller_bank
===> Booted client_seller_bank
Erlang/OTP 24 [erts-12.2.1] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [jit]
Eshell V12.2.1  (abort with ^G)
1> [whereis(client), whereis(seller), whereis(bank)].
[undefined,undefined,<0.147.0>]
```
