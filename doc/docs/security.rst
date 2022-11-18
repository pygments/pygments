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