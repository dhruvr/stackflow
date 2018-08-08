
# Deploying Yesod to Heroku, can&apos;t build statically

## Question
      
I'm very new to Yesod and I'm having trouble building Yesod statically so I can deploy to Heroku.

I have changed the default .cabal file to reflect static compilation

    if flag(production)
       cpp-options:   -DPRODUCTION
       ghc-options:   -Wall -threaded -O2 -static -optl-static
    else
       ghc-options:   -Wall -threaded -O0
    

And it no longer builds. I get a whole bunch of warnings and then a slew of undefined references like this:

    Linking dist/build/personal-website/personal-website ...
    /usr/lib/ghc-7.0.3/libHSrts_thr.a(Linker.thr_o): In function
    `internal_dlopen':
    Linker.c:(.text+0x407): warning: Using 'dlopen' in statically linked
    applications requires at runtime the shared libraries from the glibc
    version used for linking
    /usr/lib/ghc-7.0.3/unix-2.4.2.0/libHSunix-2.4.2.0.a(HsUnix.o): In
    function `__hsunix_getpwent':
    HsUnix.c:(.text+0xa1): warning: Using 'getpwent' in statically linked
    applications requires at runtime the shared libraries from the glibc
    version used for linking
    /usr/lib/ghc-7.0.3/unix-2.4.2.0/libHSunix-2.4.2.0.a(HsUnix.o): In
    function `__hsunix_getpwnam_r':
    HsUnix.c:(.text+0xb1): warning: Using 'getpwnam_r' in statically
    linked applications requires at runtime the shared libraries from the
    glibc version used for linking
    /usr/lib/libpq.a(thread.o): In function `pqGetpwuid':
    (.text+0x15): warning: Using 'getpwuid_r' in statically linked
    applications requires at runtime the shared libraries from the glibc
    version used for linking
    /usr/lib/libpq.a(ip.o): In function `pg_getaddrinfo_all':
    (.text+0x31): warning: Using 'getaddrinfo' in statically linked
    applications requires at runtime the shared libraries from the glibc
    version used for linking
    /usr/lib/ghc-7.0.3/site-local/network-2.3.0.2/
    libHSnetwork-2.3.0.2.a(BSD__63.o): In function `sD3z_info':
    (.text+0xe4): warning: Using 'gethostbyname' in statically linked
    applications requires at runtime the shared libraries from the glibc
    version used for linking
    /usr/lib/ghc-7.0.3/site-local/network-2.3.0.2/
    libHSnetwork-2.3.0.2.a(BSD__164.o): In function `sFKc_info':
    (.text+0x12d): warning: Using 'getprotobyname' in statically linked
    applications requires at runtime the shared libraries from the glibc
    version used for linking
    /usr/lib/ghc-7.0.3/site-local/network-2.3.0.2/
    libHSnetwork-2.3.0.2.a(BSD__155.o): In function `sFDs_info':
    (.text+0x4c): warning: Using 'getservbyname' in statically linked
    applications requires at runtime the shared libraries from the glibc
    version used for linking
    /usr/lib/libpq.a(fe-misc.o): In function `pqSocketCheck':
    (.text+0xa2d): undefined reference to `SSL_pending'
    /usr/lib/libpq.a(fe-secure.o): In function `SSLerrmessage':
    (.text+0x31): undefined reference to `ERR_get_error'
    /usr/lib/libpq.a(fe-secure.o): In function `SSLerrmessage':
    (.text+0x41): undefined reference to `ERR_reason_error_string'
    /usr/lib/libpq.a(fe-secure.o): In function `initialize_SSL':
    (.text+0x2f8): undefined reference to `SSL_check_private_key'
    /usr/lib/libpq.a(fe-secure.o): In function `initialize_SSL':
    (.text+0x3c0): undefined reference to `SSL_CTX_load_verify_locations'
    (... snip ...)
    

If I just compile with just `-static` and without `-optl-static` everything builds fine but the application crashes when it tries to start on Heroku.

    2011-12-28T01:20:51+00:00 heroku[web.1]: Starting process with command
    `./dist/build/personal-website/personal-website -p 41083`
    2011-12-28T01:20:51+00:00 app[web.1]: ./dist/build/personal-website/
    personal-website: error while loading shared libraries: libgmp.so.10:
    cannot open shared object file: No such file or directory
    2011-12-28T01:20:52+00:00 heroku[web.1]: State changed from starting
    to crashed
    

I tried adding libgmp.so.10 to the LD\_LIBRARY\_PATH as suggested in [here](http://translate.google.com/translate?hl=en&sl=ja&tl=en&u=http://d.hatena.ne.jp/thimura/20111117/1321480728) and then got the following error:

    2011-12-28T01:31:23+00:00 app[web.1]: ./dist/build/personal-website/
    personal-website: /lib/libc.so.6: version `GLIBC_2.14' not found
    (required by ./dist/build/personal-website/personal-website)
    2011-12-28T01:31:23+00:00 app[web.1]: ./dist/build/personal-website/
    personal-website: /lib/libc.so.6: version `GLIBC_2.14' not found
    (required by /app/dist/build/personal-website/libgmp.so.10)
    2011-12-28T01:31:25+00:00 heroku[web.1]: State changed from starting
    to crashed
    2011-12-28T01:31:25+00:00 heroku[web.1]: Process exited
    

It seems that the version of libc that I'm compiling against is different. I tried also adding libc to the batch of libraries the same way I did for libgmp but this results in a segmentation fault when the application starts on the Heroku side.

Everything works fine on my PC. I'm running 64bit archlinux with ghc 7.0.3. [The blog post on the official Yesod blog](http://www.yesodweb.com/blog/2011/07/haskell-on-heroku) looked pretty easy but I'm stumped at this point. Anyone have any ideas? If there's a way to get this thing working _without_ building statically I'm open to that too.

**EDIT**

Per `Employed Russians` answer I did the following to fix this.

First created a new directory `lib` under the project directory and copied the missing shared libraries into it. You can get this information by running `ldd path/to/executable` and `heroku run ldd path/to/executable` and comparing the output.

I then did `heroku config:add LD_LIBRARY_PATH=./lib` so when the application is started the dynamic linker will look for libraries in the new lib directory.

Finally I created an ubuntu 11.10 virtual machine and built and deployed to Heroku from there, this has an old enough glibc that it works on the Heroku host.

Edit: I've since written a tutorial on the [Yesod wiki](https://github.com/yesodweb/yesod/wiki/Deploying-Yesod-Apps-to-Heroku)
## Answer
      
I have no idea what Yesod is, but I know _exactly_ what each of your other errors means.

First, you should _not_ try to link statically. The warning you get is exactly right: **if** you link statically, and use one of the routines for which you are getting the warning, then you must arrange to run on a system with _exactly_ the same version of libc.so.6 as the one you used at build time.

Contrary to popular belief, static linking produces _less_, not more, portable executables on Linux.

Your other (static) link errors are caused by missing `libopenssl.a` at link time.

But let's assume that you are going to go the "sane" route, and use dynamic linking.

For dynamic linking, Linux (and most other UNIXes) support backward compatibility: an old binary continues to work on newer systems. But they don't support forward compatibility (a binary built on a newer system will generally _not_ run on an older one).

But that's what you are trying to do: you built on a system with glibc-2.14 (or newer), and you are running on a system with glibc-2.13 (or older).

The other thing you need to know is that glibc is composed of some 200+ binaries that must all match _exactly_. Two key binaries are `/lib/ld-linux.so` and `/lib/libc.so.6` (but there are many more: `libpthread.so.0`, `libnsl.so.1`, etc. etc). If some of these binaries came from different versions of glibc, you usually get a crash. And that is exactly what you got, when you tried to place your glibc-2.14 `libc.so.6` on the `LD_LIBRARY_PATH` \-\- it no longer matches the system `/lib/ld-linux`.

So what are the solutions? There are several possibilities (in increasing difficulty):

1.  You could copy `ld-2.14.so` (the target of `/lib/ld-linux` symlink) to the target system, and invoke it explicitly:
    
        /path/to/ld-2.14.so --library-path <whatever> /path/to/your/executable
        
    
    This generally works, but can confuse an application that looks at `argv[0]`, and breaks for applications that re-exec themselves.
    
2.  You could build on an older system.
    
3.  You could use `appgcc` (this option has disappeared, see [this](http://web.archive.org/web/20090902074009/http://autopackage.org/apbuild-apgcc.php) for description of what it used to be).
    
4.  You could set up a chroot environment matching the target system, and build inside that chroot.
    
5.  You could build yourself a Linux-to-olderLinux crosscompiler
    