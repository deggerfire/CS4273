# CS4273

# Git Info

**If anything is unclear or you do not know how to use git ask your git master, we would rather you asked then blow up the git**

## Branch Structure

Main - The latest version of fully merged and “working” software between both teams. Only the git master should merge into this branch **!You should NOT be working on this branch!**

Group-[x] - The latest version of fully merged software for either group. **!You should NOT be working on this branch!**

[something descriptive] - The branch that you should be working on. The name should have something to do with what changes are on the branch.

## Working Conduct

It is important that you follow these rules because this will prevent merge conflicts which can take hours to sort out, trust me I’ve dealt with it before, not fun.

1) State in discord where/what you are working on to the relevant people. Also say when you are done working.

2) Don’t work on methods that someone says they are working on. If you need to mess with a method someone is working on, talk to them.

3) Keep up with the Group-x branch, before you start making changes to code, rebase your branch to this one and when you are done merge Group-x to your branch. Working in this context refers to actively, if you are not actively working then your stuff should be on Group-x (unless the code does not work).
   
5) Do not make random changes around the code, only work where you have told people you are working, if you see something bad in the code just leave it and let people know in discord or make a ticket, at some point the chance will open up to fix it.

6) **COMMENT YOUR CODE!** Add as many as possible and try to make your code look neat, everyone will thank you for it. If you are having a hard time adding comments, ask someone for help. Format your comments like: // This is a comment

7) When you are trying to merge any branches you should make sure all relevant people are informed.

8) Careful what you add to tracking, we don’t want something like add _pycache_. This is mainly an issue if you use git in vscode, I think _pycache_ is in gitinore but not 100% sure but that only covers it not other random compiler stuff. Add things to .gitinore if relevant.

These rules are not hard set; you can play fast and loose with them they are just general things to prevent git conflicts but if you do go breaking them understand that this could cause a lot of issues if you mess up and could cost people many hours, so be 100% sure you know what you are doing when you break them.


