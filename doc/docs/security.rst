Security considerations
-----------------------

Pygments provides no guarantees on execution time, which needs to be taken
into consideration when using Pygments to process arbitrary user inputs. For
example, if you have a web service which uses Pygments for highlighting, there
may be inputs which will cause the Pygments process to run "forever" and/or use
significant amounts of memory. This can subsequently be used to perform a
remote denial-of-service attack on the server if the processes are not
terminated quickly.

Unfortunately, it's practically impossible to harden Pygments itself against 
those issues: Some regular expressions can result in "catastrophic 
backtracking", but other bugs like incorrect matchers can also
cause similar problems, and there is no way to find them in an automated fashion
(short of solving the halting problem.) Pygments has extensive unit tests, 
automated randomized testing, and is also tested by `OSS-Fuzz <https://github.com/google/oss-fuzz/tree/master/projects/pygments>`_, 
but we will never be able to eliminate all bugs in this area.

Our recommendations are:

* Ensure that the Pygments process is *terminated* after a reasonably short
  timeout. In general Pygments should take seconds at most for reasonably-sized
  input.
* *Limit* the number of concurrent Pygments processes to avoid oversubscription
  of resources.

The Pygments authors will treat any bug resulting in long processing times with
high priority -- it's one of those things that will be fixed in a patch release.
When reporting a bug where you suspect super-linear execution times, please make
sure to attach an input to reproduce it.

`pip-audit`, CVEs, and GitHub security advisories
===============================================

We've seen various CVEs and security advisories filed against Pygments ignoring
the guideline above. Typically, they'll report a ReDoS (remote
denial-of-service) vulnerability in Pygments. Generally those advisories will
provide some input that causes a regex to go into catastrophic backtracking or
similar, resulting in long processing times. First of all, please note that
Pygments on its own does not allow remote input -- it's a command line
application or library, but it has no RPC mechanism, nor does it start a server
by default. The "remote" part only appears when Pygments is wrapped in a service
that allows arbitrary user input. In this case, we *strongly* recommend to take
the security considerations above into account, specifically sandboxing and
resource limitations.

If your CI pipeline fails because of automated checks for CVEs, we kindly ask
you to review the particular issue and consider adding an exception before
asking for an urgent fix. Especially if you're using Pygments for your
documentation and not wrapping it into a service, you are *not* vulnerable (the
worst thing that can happen is that someone writes a PR which will cause a
CI job that builds the docs to time out, but that's what timeouts are for, and
if someone can send a PR there are many ways to attack your CI infrastructure.)
`pip-audit` for example allows you to selectively ignore individual security
issues, making it easy for you to fix your CI while we prepare a new release.

We try to fix security issues, but we do ask everyone to treat user-provided
input as dangerous and protect in depth against it. If you find a way to make
Pygments execute arbitrary code, we'll do our best to fix it quickly, but if it
is a hang, the guidance above applies. We also recommend reading:
https://lwn.net/Articles/944399/