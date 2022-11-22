These 'common options' aren't common in the sense that option types
should 'inherit' them - rather, when passing to a compound command,
these are options that apply to each in the compound.

For example, ANDROID_BUSYBOX, when passed to sshDu or similar, may
affect both the ssh usage (default ports, user, etc.) and the du usage
(path to use, option syntax, etc.).

As a counter example, every command will probably have a path option.
but given an sshDu usage, there are potentially two 'path' options - one
for the ssh, and one for the du.  Hence, that is not a 'common option' in
the sense it is used here.
