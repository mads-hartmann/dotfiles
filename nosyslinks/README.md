# no syslinks

**Am I really just creating an ad-hoc, informally-specified, bug-ridden, slow implementation of half of a  proper configuration management system?**

Idea: Right now I use sym-links to keep all my files in this git repository so I can track changes. What if instead I just give up and accept that my system is an every-changing mess? All I really want is to be able to take snapshots every once in a while with comments attached and diffs so I can remember what I changed.

Maybe just have a data file that describes the folders/files to track and then copy them into a folder and commit. It can also copy back out to the system to override system changes. That's it.
